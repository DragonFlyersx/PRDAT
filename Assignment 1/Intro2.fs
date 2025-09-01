(* Programming language concepts for software developers, 2010-08-28 *)

(* Evaluating simple expressions with variables *)

module Intro2

(* Association lists map object language variables to their values *)

let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)];; // This is the eviroment

let emptyenv = []; (* the empty environment *) // set an empty enviroment

let rec lookup env x = // function foor looking up a value in the enviroment
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

let cvalue = lookup env "c";;


(* Object language expressions with variables *)

type expr = 
  | CstI of int
  | Var of string
  | Prim of string * expr * expr
  | If of expr * expr * expr;;

type aexpr = // this is type we made of aexpr 1.2.1
  | CstI of int
  | Var of string
  | Add of aexpr * aexpr
  | Mul of aexpr * aexpr
  | Sub of aexpr * aexpr;;
(*
    2 ∗ (v − (w + z))
    Sub(Var "v", Add(Var "w", Var "z"))

    2 ∗ (v − (w + z))
    Mul(CstI 2, Sub(Var "v", Add(Var "w", Var "z")))

    x + y + z + v
    Add(Var "x", Add(Var "y", Add(Var "z", Var "v")))

This is for 1.2.2
*)
let test = Add(Var "x", Add(Var "y", Add(Var "z", Var "v")))

let rec fmt (ae: aexpr): string = // 1.2.3
    match ae with 
    | CstI i -> string i
    | Var i -> i
    | Add(i1, i2) -> "( " + fmt i1 + " + " + fmt i2 + " )"
    | Mul(i1, i2) -> "( " + fmt i1 + " * " + fmt i2 + " )"
    | Sub(i1, i2) -> "( " + fmt i1 + " - " + fmt i2 + " )"


let simplify (ae: aexpr) : aexpr = // 1.2.4 solution for not the ambitious student
    match ae with
        | Add(CstI 0,e) -> e
        | Add(e,CstI 0) -> e
        | Sub(e,CstI 0) -> e
        | Mul(CstI 1,e) -> e
        | Mul(e,CstI 1) -> e
        | Mul(CstI 0,e) -> CstI 0
        | Mul(e,CstI 0) -> CstI 0
        | Sub(e1,e2) when e1 = e2 -> CstI 0
        | _ -> ae

let test2 = Mul(Add(CstI 1, CstI 0),Add(Var "x", CstI 0)) // test case
let test3 = Add(CstI 0,Var "x") // test case

let test4 = Add(CstI 1, CstI 0) // test case

let bad = Add(Var "x", Var "y")

let rec simplify2 (ae: aexpr) : aexpr = // 1.2.4 solution for the ambitious student
    match ae with
        | Add(i1, i2) -> 
                let s1 = simplify2 i1
                let s2 = simplify2 i2
                match s1, s2 with
                | CstI 0, e | e, CstI 0 -> e
                | _ -> Add(s1, s2)
        | Sub(i1, i2) ->
                let s1 = simplify2 i1
                let s2 = simplify2 i2
                match s1, s2 with
                | e1, e2 when e1 = e2 -> CstI 0
                | e, CstI 0 -> e
                | _ -> Sub(s1, s2)
        | Mul(i1, i2) ->
                let s1 = simplify2 i1
                let s2 = simplify2 i2
                match s1, s2 with
                | CstI 0, _ | _, CstI 0 -> CstI 0
                | CstI 1, e | e, CstI 1 -> e
                | _ -> Mul(s1, s2)
        | _ -> ae



let rec differentiate (ae:aexpr) : aexpr = // 1.2.5
    match ae with
    | CstI i -> CstI 0
    | Var i -> CstI 1
    | Add(i1,i2) -> Add(differentiate i1, differentiate i2) 
    | Sub(i1,i2) -> Sub(differentiate i1, differentiate i2) 
    | Mul(i1, i2) ->
        Add(Mul(differentiate i1, i2), Mul(i1, differentiate i2))

// Test for 1.2.5 differentiate.
let testdif1 : aexpr = differentiate (CstI 2) 
let testdif2 : aexpr = differentiate (Var "x")
let testdif3 : aexpr = differentiate (Add(CstI 3, Var "x"))
let testdif4 : aexpr= differentiate (Mul(Var "x", Var "x"))



let e1 = CstI 17;;

let e2 = Prim("+", CstI 3, Var "a");;

let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a");;


(* Evaluation within an environment *)

let rec eval e (env : (string * int) list) : int = // we have updated this eval function and changed 1.1.1 and 1.1.5
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x 
    | Prim("+", e1, e2) -> eval e1 env + eval e2 env
    | Prim("*", e1, e2) -> eval e1 env * eval e2 env
    | Prim("-", e1, e2) -> eval e1 env - eval e2 env
    | Prim("max", e1, e2) -> let one = eval e1 env  
                             let two = eval e2 env
                             if one > two then one
                             else two
    | Prim("min", e1, e2) -> let one = eval e1 env  
                             let two = eval e2 env
                             if one < two then one
                             else two
    | Prim("==", e1, e2) ->  let one = eval e1 env  
                             let two = eval e2 env
                             if one = two then 1
                             else 0
    | If(e1, e2, e3) -> if eval e1 env <> 0 then eval e2 env else eval e3 env
    | Prim _            -> failwith "unknown primitive";;


// this is for 1.1.3, 1.1.5 We continued to change this for both the if and to keep the match on ope
let rec eval2 e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x 
    | Prim(ope, e1, e2) -> 
        let i1 = eval2 e1 env
        let i2 = eval2 e2 env
        match ope with
            | "+" -> i1 + i2
            | "*" -> i1 * i2
            | "-" -> i1 - i2
            | "max" -> if i1 > i2 then i1 else i2
            | "min" -> if i1 > i2 then i2 else i1
            | "==" -> if i1 = i2 then 1 else 0
            | _ -> failwith "unknown operator"
    | If(e1, e2, e3) -> if eval2 e1 env <> 0 then eval2 e2 env else eval2 e3 env

(*
1.1.2
Example expresion in the extended expression language using abstract syntax

    max(3, 10) * min(4, 8) + ==(3, 3)

   let test = Prim("+", Prim("*", Prim("max", CstI 3, CstI 10), Prim("min", CstI 4, CstI 8)), Prim("==", CstI 3, CstI 3))


    
   eval test []


 *)

// Example we created
let test = Prim("+", Prim("*", Prim("max", CstI 3, CstI 10), Prim("min", CstI 4, CstI 8)), Prim("==", CstI 3, CstI 3))



let e1v  = eval e1 env;;
let e2v1 = eval e2 env;;
let e2v2 = eval e2 [("a", 314)];;
let e3v  = eval e3 env;;
