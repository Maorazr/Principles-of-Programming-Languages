export const MISSING_KEY = '___MISSING_KEY___'
export const MISSING_TABLE_SERVICE = '___MISSING_TABLE_SERVICE___'

export type Table<T> = Readonly<Record<string, Readonly<T>>>

export type TableService<T> = {
    get(key: string): Promise<T>;
    set(key: string, val: T): Promise<void>;
    delete(key: string): Promise<void>;
}

// Q 2.1 (a)
export function makeTableService<T>(sync: (table?: Table<T>) => Promise<Table<T>>): TableService<T> {

    const curr_table = sync();
    return {
        get(key: string): Promise<T> {
            return new Promise((resolve, reject) => {
                sync()
                    .then((data) => typeof data[key] !== 'undefined' ? resolve(data[key]) : reject(MISSING_KEY))
                    .catch((err) => console.error("failed",err));
            });
        },
        set(key: string, val: T): Promise<void> {
            return new Promise<void>((resolve, reject) => {
                curr_table.then((table) => {
                    let mutable : Record<string, Readonly<T>> = table;
                    mutable[key] = val;
                    return sync(mutable);
                }).then(() => resolve())
                    .catch(() => reject(MISSING_KEY))
            })
        },
        delete(key: string): Promise<void> {
            return new Promise<void>((resolve, reject) => {
                curr_table.then((table) => {
                    let mutable : Record<string, Readonly<T>>  =  table;
                    if (key in table) delete mutable[key];
                    return sync((mutable));
                }).then(() => resolve())
                    .catch(() => reject(MISSING_KEY))
            })
        }
    }
}

// Q 2.1 (b)
export function getAll<T>(store: TableService<T>, keys: string[]): Promise<T[]> {
    const s = keys.map((v) => store.get(v));
    return Promise.all(s);
}


// Q 2.2
export type Reference = { table: string, key: string }

export type TableServiceTable = Table<TableService<object>>

export function isReference<T>(obj: T | Reference): obj is Reference {
    return typeof obj === 'object' && 'table' in obj
}

export async function constructObjectFromTables(tables: TableServiceTable, ref: Reference) {
    async function deref(ref: Reference) {
        const tableServ = tables[ref.table];
        if (typeof tableServ !== 'undefined') {
            try {
                let currGet = await tableServ.get(ref.key);
                let entries = Object.entries(currGet);
                for (let i = 0; i < entries.length; i++) {
                    if (isReference(entries[i][1])) {
                        entries[i][1] = await deref(entries[i][1]);
                    }
                }
                return Object.fromEntries(await Promise.all(entries));
                //return Promise.resolve(Object.fromEntries(entries));
            }
            catch (err) {
                //console.error(MISSING_TABLE_SERVICE);
                return Promise.reject(MISSING_KEY);
            }
        }
        else {
            return Promise.reject(MISSING_TABLE_SERVICE);
        }
    }

    return deref(ref)
}

// Q 2.3
export function lazyProduct<T1, T2>(g1: () => Generator<T1>, g2: () => Generator<T2>): () => Generator<[T1, T2]> {
    return function* () {
        for (const n of g1()) {
            for (const m of g2())
                yield [n, m];
        }
    }
}

export function lazyZip<T1, T2>(g1: () => Generator<T1>, g2: () => Generator<T2>): () => Generator<[T1, T2]> {
    return function* () {
        let gen2 = g2();
        for (const n of g1()) {
            yield [n, gen2.next().value];
        }
    }
}

// Q 2.4
export type ReactiveTableService<T> = {
    get(key: string): T;
    set(key: string, val: T): Promise<void>;
    delete(key: string): Promise<void>;
    subscribe(observer: (table: Table<T>) => void): void
}


export async function makeReactiveTableService<T>(sync: (table?: Table<T>) => Promise<Table<T>>, optimistic: boolean): Promise<ReactiveTableService<T>> {
    // optional initialization code
    let _table: Table<T> = await sync()
    let observers: ((table: Table<T>) => void)[] = [];

    const handleMutation = async (newTable: Table<T>) => {
        if (optimistic)
            observers.map((func) => func(newTable));

        await sync(newTable).catch((err) => Promise.reject(err)).then(() => _table = newTable);
    }

    return {
        get(key: string): T {
            if (key in _table) {
                return _table[key]
            } else {
                throw MISSING_KEY
            }
        },

        set(key: string, val: T): Promise<void> {
            let mutable : Record<string, Readonly<T>> = Object.fromEntries(Object.entries(_table));
            mutable[key] = val;

            return handleMutation(mutable).catch((err) => {
                observers.map((func) => func(_table));
                return Promise.reject(err);

            }).then(() => {
                if (!optimistic)
                    observers.map((func) => func(mutable));
            })
        },


        delete(key: string): Promise<void> {
            let mutable : Record<string, Readonly<T>> = Object.fromEntries(Object.entries(_table));
            key in mutable ? delete mutable[key] : Promise.reject(MISSING_KEY);

            return handleMutation(mutable).catch((err) => {
                observers.map((func) => func(_table));
                return Promise.reject(err);
            })
                .then(() => {
                    if (!optimistic)
                        observers.map((func) => func(mutable));
                })
        },
        subscribe(observer: (table: Table<T>) => void): void {
            observers.push(observer);
        }
    }
}