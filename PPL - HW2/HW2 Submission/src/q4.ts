import { isSymbolSExp, isEmptySExp, isCompoundSExp, Value, isClosure, closureToString, compoundSExpToString } from '../imp/L3-value';
import exp from "constants";
import { map, zipWith } from "ramda";
import { VarDecl, LitExp, AppExp, CExp, Exp, ProcExp, LetExp, makeLetExp, Program, PrimOp } from "./L31-ast";
import {
    isVarRef, isStrExp, isPrimOp, isBoolExp, isNumExp, isAppExp, isAtomicExp, isCExp, isDefineExp, isExp, isIfExp, isLetExp, isLitExp,
    isProcExp, isProgram, makeAppExp, makeDefineExp, makeIfExp, makeProcExp, makeProgram, makeBinding, Binding
} from "./L31-ast";
import { Result, bind, makeFailure, makeOk, mapResult, safe2, mapv } from "../shared/result";
import { makeBox, unbox } from "../shared/box";
import { first } from '../shared/list';
import { isNumber, isString } from '../shared/type-predicates';

/*
Purpose: Transform L3 AST to JavaScript program string
Signature: l30ToJS(l2AST)
Type: [EXP | Program] => Result<string>
*/


export const l30ToJS = (exp: Exp | Program): Result<string>  =>
    isProgram(exp) ? bind(mapResult(l30ToJS, exp.exps), exps => makeOk(exps.join(";\n"))) :
    isBoolExp(exp) ? makeOk(valueToString(exp.val)):
    isNumExp(exp) ? makeOk(valueToString(exp.val)): 
    isStrExp(exp) ? makeOk(valueToString(exp.val)):
    isLitExp(exp) ? makeOk(`Symbol.for(\"${valueToString(exp.val)}\")`):
    isVarRef(exp) ? makeOk(exp.var):
    isProcExp(exp) ? bind(l30ToJS(exp.body[0]),  body => makeOk("((" + map((p) => p.var, exp.args).join(",") + ")" + " => "  + body + ")")): 
    isDefineExp(exp) ? bind(l30ToJS(exp.val), val => makeOk(`const ${exp.var.var} = ${val}`)):
    isIfExp(exp) ? bind(l30ToJS(exp.test), test => (bind(l30ToJS(exp.then), then => bind(l30ToJS(exp.alt), alt => makeOk(`(${test} ? ${then} : ${alt})`))))):
    isAppExp(exp) ? (
        isPrimOp(exp.rator) ? primOpToJS(exp.rator, exp.rands) : 
        safe2((rator: string, rands: string[]) => makeOk(`${rator}(${rands.join(",")})`))
               (l30ToJS(exp.rator), mapResult(l30ToJS, exp.rands))
        ):
    isPrimOp(exp) ? makeOk(convertOp(exp.op)):
    isLetExp(exp) ? l30ToJS(rewriteAllLet(exp)):
    makeFailure("failure");

        
export const convertOp = (op: string): string =>
    op === "=" || op === "eq?" || op === "string=?" ? "===" :
    op === "symbol?" ? "((x) => (typeof (x) === symbol))" :
    op === "boolean?" ? "((x) => (typeof(x) === boolean))" :
    op === "number?" ? "((x) => (typeof(x) === number))" :
    op === "string?" ? "((x) => (typeof(x) === string))" :
    op === "and" ? "&&" :
    op === "or" ? "||" :
    op;

const primOpToJS = (rator : PrimOp, rands : CExp[]) : Result<string> => 
    rator.op === "not" ? bind(l30ToJS(rands[0]), (rand : string) => makeOk("(!" + rand + ")")):
    rator.op === "number?" || rator.op === "boolean?" || rator.op === "symbol?" || rator.op === "string?" ? 
        bind(l30ToJS(rands[0]), (rand : string) => makeOk(`${convertOp(rator.op)}(${rands[0]})`)):
    rator.op === "=" || rator.op == "eq?" || rator.op === "string=?" ? bind(mapResult(l30ToJS,rands), (rands) => makeOk("(" + rands.join(" === ") + ")")):
    bind(mapResult(l30ToJS,rands), (rands) => makeOk("(" + rands.join(" " + convertOp(rator.op) + " ") + ")"));    

export const valueToString = (val: Value): string =>
    isNumber(val) ?  val.toString() :
    val === true ? 'true' :
    val === false ? 'false' :
    isString(val) ? `"${val}"` :
    isClosure(val) ? closureToString(val) :
    isPrimOp(val) ? val.op :
    isSymbolSExp(val) ? val.val :
    isEmptySExp(val) ? "'()" :
    isCompoundSExp(val) ? compoundSExpToString(val) :
    val;


export const rewriteLet = (e: LetExp): AppExp => {
    const vars = map((b) => b.var, e.bindings);
    const vals = map((b) => b.val, e.bindings);
    return makeAppExp(
             makeProcExp(vars, e.body),
            vals);
    }


export const rewriteAllLet = (exp: Program | Exp): Program | Exp =>
    isExp(exp) ? rewriteAllLetExp(exp):
    isProgram(exp) ? makeProgram(map(rewriteAllLetExp, exp.exps)):
    exp;

export const rewriteAllLetExp = (exp: Exp): Exp =>
    isCExp(exp) ? rewriteAllLetCExp(exp):
    isDefineExp(exp) ? makeDefineExp(exp.var, rewriteAllLetCExp(exp.val)):
    exp;

export const rewriteAllLetCExp = (exp: CExp): CExp =>
    isAtomicExp(exp) ? exp:
    isLitExp(exp) ? exp:
    isIfExp(exp) ? makeIfExp(rewriteAllLetCExp(exp.test),
                   rewriteAllLetCExp(exp.then),
                   rewriteAllLetCExp(exp.alt)):
    isAppExp(exp) ? makeAppExp(rewriteAllLetCExp(exp.rator),
                    map(rewriteAllLetCExp, exp.rands)):
    isProcExp(exp) ? makeProcExp(exp.args, map(rewriteAllLetCExp, exp.body)):
    isLetExp(exp) ? rewriteAllLetCExp(rewriteLet(exp)):
    exp;