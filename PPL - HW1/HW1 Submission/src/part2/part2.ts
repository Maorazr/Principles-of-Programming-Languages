import * as R from "ramda";

const stringToArray = R.split("");

/* Question 1 */
//helpers
const isLetter: (x:string) => boolean = (x:string):boolean =>
         (65 <= x.charCodeAt(0) && x.charCodeAt(0) <= 90) ||
         (97 <= x.charCodeAt(0) && x.charCodeAt(0) <= 122);

const isLetterF = R.filter(isLetter);

export const countLetters: (s: string) => {} = R.pipe(
    stringToArray,
    isLetterF,
    R.countBy(R.toLower)
 );

/* Question 2 */
const isParenthesis = (x: string): boolean => {
    if (x === "[" || x === "{" ||
    x === "(" || x === ")" ||
     x === "}" || x === "]")
        return true;
else 
    return false;
}

const extractParenthesis = R.filter(isParenthesis);

const popIfMatch = (stack: string[], open: string, close: string): string[] =>
        R.isEmpty(stack) ? [close] :
        R.head(stack) === open ? R.tail(stack) : R.prepend(close, stack);

const pairReducer = (stack: string[], c: string): string[] =>
        c === "(" || c === "{" || c === "[" ? R.prepend(c, stack) :
        c === ")" ? popIfMatch(stack, "(", c) :
        c === "}" ? popIfMatch(stack, "{", c) :
        c === "]" ? popIfMatch(stack, "[", c) :
        stack;


// /* Question 2 /*
export const isPaired: (s: string) => boolean = R.pipe(
    stringToArray,
    extractParenthesis,
    R.reduce(pairReducer, []),
    R.isEmpty
    );


/* Question 3 */
export interface WordTree {
    root: string;
    children: WordTree[];
}

export const treeToSentence = (t: WordTree):string => {
    return (t.children).reduce(
       (acc : string, curr: WordTree):string => {
          return acc.concat(" " + treeToSentence(curr));
        }
    ,t.root);
}

