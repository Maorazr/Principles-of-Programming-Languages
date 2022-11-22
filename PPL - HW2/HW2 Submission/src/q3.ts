import { isSymbolSExp, isEmptySExp, isCompoundSExp, Value, isClosure, closureToString, compoundSExpToString } from '../imp/L3-value';
import exp from "constants";
import { map, zipWith } from "ramda";
import { VarDecl, LitExp, AppExp, CExp, Exp, ProcExp, LetExp, makeLetExp, Program, PrimOp, LetPlusExp, makeLetPlusExp, isLetPlusExp } from "./L31-ast";
import {
    isVarRef, isStrExp, isPrimOp, isBoolExp, isNumExp, isAppExp, isAtomicExp, isCExp, isDefineExp, isExp, isIfExp, isLetExp, isLitExp,
    isProcExp, isProgram, makeAppExp, makeDefineExp, makeIfExp, makeProcExp, makeProgram, makeBinding, Binding
} from "./L31-ast";
import { Result, bind, makeFailure, makeOk, mapResult, safe2, mapv } from "../shared/result";
import { makeBox, unbox } from "../shared/box";
import { first } from '../shared/list';
import { isNumber, isString } from '../shared/type-predicates';


/*
Purpose: Transform L31 AST to L3 AST
Signature: l31ToL3(l31AST)
Type: [Exp | Program] => Result<Exp | Program>
*/
/*
Purpose: Transform L31 AST to L3 AST
Signature: l31ToL3(l31AST)
Type: [Exp | Program] => Result<Exp | Program>
*/
export const L31ToL3 = (exp: Exp | Program): Result<Exp | Program> => 
    isProgram(exp) ? bind(mapResult(rewrtieAllLetPlusExp, exp.exps), (exps: Exp[]) => makeOk(makeProgram(exps))) :
    isExp(exp) ? rewrtieAllLetPlusExp(exp) :
    makeFailure("Never");


export const rewrtieAllLetPlusExp = (exp: Exp): Result<Exp> => 
    isCExp(exp) ? rewrtieAllLetPlusCExp(exp) :
    isDefineExp(exp) ? bind(rewrtieAllLetPlusCExp(exp.val), (val: CExp) => makeOk(makeDefineExp(exp.var, val))):
    makeFailure("");


export const rewrtieAllLetPlusCExp = (exp: CExp): Result<CExp> => 
    isBoolExp(exp) ? makeOk(exp):
    isNumExp(exp) ? makeOk(exp):
    isPrimOp(exp) ? makeOk(exp):
    isVarRef(exp) ? makeOk(exp):
    isLitExp(exp) ? makeOk(exp):
    
    isIfExp(exp) ? bind(rewrtieAllLetPlusCExp(exp.test), test => 
        bind( rewrtieAllLetPlusCExp(exp.then), then => 
            bind(rewrtieAllLetPlusCExp(exp.alt), alt => makeOk(makeIfExp(test, then, alt))))):

    isAppExp(exp) ? safe2((rator: CExp, rands: CExp[]) => makeOk(makeAppExp(rator, rands)))
               (rewrtieAllLetPlusCExp(exp.rator), mapResult(rewrtieAllLetPlusCExp, exp.rands)):

    isProcExp(exp) ? bind(mapResult(rewrtieAllLetPlusCExp, exp.body), (body: CExp[]) => makeOk(makeProcExp(exp.args, body))):
    isLetExp(exp) ? safe2((bindings: CExp[], body: CExp[]) => makeOk(makeLetExp(exp.bindings, body)))
                (mapResult(rewrtieAllLetPlusCExp, map((p) => p.val, exp.bindings)), mapResult(rewrtieAllLetPlusCExp, exp.body)):
    isLetPlusExp(exp) ? rewrtieAllLetPlusCExp(rewriteLetPLus(exp)):
    makeFailure("Shut up");

// rewrite a single let* as let
export const rewriteLetPLus = (e: LetPlusExp): LetExp => {
    if (e.bindings.length === 1){
        return makeLetExp(e.bindings, e.body);
    }
    return makeLetExp(e.bindings.slice(0,1), [makeLetPlusExp( e.bindings.slice(1), e.body)])

}




