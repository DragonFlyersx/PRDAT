(* Programming language concepts for software developers, 2010-08-28 *)

(* Evaluating simple expressions with variables *)

module Intro2

(* Association lists map object language variables to their values *)

let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)];;

let emptyenv = []; (* the empty environment *)

let rec lookup env x =
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

type aexpr =
  | CstI of int
  | Var of string
  | Add of aexpr * aexpr
  | Sub of aexpr * aexpr
  | Mul of aexpr * aexpr;;

(*
1.2.2
Example representation:
    v − (w + z)
        Sub(Var "v", Add(Var "w", Var "z"))
    
    2 ∗ (v − (w + z))
        Mul(CstI 2, Sub(Var "v", Add(Var "w", Var "z")))

    x + y + z + v
        Add(Var "x", Add(Var "y", Add(Var "z", Var "v")))
*)

let rec fmt (ae: aexpr) : string =
    match ae with
    | CstI i -> string i
    | Var x -> x
    | Add(e1, e2) -> "(" + fmt e1 + " + " + fmt e2 + ")"
    | Sub(e1, e2) -> "(" + fmt e1 + " - " + fmt e2 + ")"
    | Mul(e1, e2) -> "(" + fmt e1 + " * " + fmt e2 + ")"

(*
1.2.3
test of expression:
    let test = Sub(Var "x", CstI 34);;

    fmt test;;

*)


let e1 = expr.CstI 17;;

let e2 = expr.Prim("+", expr.CstI 3, expr.Var "a");;

let e3 = expr.Prim("+", expr.Prim("*", expr.Var "b", expr.CstI 9), expr.Var "a");;


(* Evaluation within an environment *)

let rec eval (e: expr) (env : (string * int) list) : int =
    match e with
    | expr.CstI i            -> i
    | expr.Var x             -> lookup env x 
    | Prim(ope, e1, e2) ->
        let i1 = eval e1 env
        let i2 = eval e2 env
        match ope with
        | "+" -> i1 + i2
        | "*" -> i1 * i2
        | "-" -> i1 - i2
        | "min" -> if i1 < i2 then i1 else i2
        | "max" -> if i1 > i2 then i1 else i2
        | "==" ->  if i1 = i2 then 1 else 0
    | If(e1, e2, e3) -> if not ((eval e1 env) = 0) then eval e2 env else eval e3 env
    | Prim _            -> failwith "unknown primitive";;

(*
1.1.2
Example expresion in the extended expression language using abstract syntax

    max(3, 10) * min(4, 8) + ==(3, 3)

   let test = Prim("+", Prim("*", Prim("max", CstI 3, CstI 10), Prim("min", CstI 4, CstI 8)), Prim("==", CstI 3, CstI 3));;
    
   eval test [];;
   
1.1.5
    
    let test = If(Var "a", CstI 11, CstI 22);;
    
    eval test [("a", 312)]


1.2.5 notes:
How It Works - Mathematical Rules:
Constants (CstI i): The derivative of a constant is 0
d/dx(5) = 0
Variables (Var x): The derivative of a variable is 1
d/dx(x) = 1
Addition (Add(e1, e2)): Sum rule - derivative of sum is sum of derivatives
d/dx(e1 + e2) = d/dx(e1) + d/dx(e2)
Subtraction (Sub(e1, e2)): Difference rule - same as addition
d/dx(e1 - e2) = d/dx(e1) - d/dx(e2)
Multiplication (Mul(e1, e2)): Product rule
d/dx(e1 * e2) = (d/dx(e1) * e2) + (e1 * d/dx(e2))

 *)


let e1v  = eval e1 env;;
let e2v1 = eval e2 env;;
let e2v2 = eval e2 [("a", 314)];;
let e3v  = eval e3 env;;
