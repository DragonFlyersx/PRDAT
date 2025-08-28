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

let e1 = CstI 17;;

let e2 = Prim("+", CstI 3, Var "a");;

let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a");;


(* Evaluation within an environment *)

let rec eval e (env : (string * int) list) : int =
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
    | If(e1, e2, e3) -> if eval e1 env = 0 then eval e3 env else eval e2 env
    | Prim _            -> failwith "unknown primitive";;


// this is for 1.1.3
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
    | If(e1, e2, e3) -> if eval2 e1 env = 0 then eval2 e3 env else eval2 e2 env

(*
Example expresion in the extended expression language using abstract syntax

    max(3, 10) * min(4, 8) + ==(3, 3)

   let test = Prim("+", Prim("*", Prim("max", CstI 3, CstI 10), Prim("min", CstI 4, CstI 8)), Prim("==", CstI 3, CstI 3))


    
   eval test []


 *)

let test = Prim("+", Prim("*", Prim("max", CstI 3, CstI 10), Prim("min", CstI 4, CstI 8)), Prim("==", CstI 3, CstI 3))



let e1v  = eval e1 env;;
let e2v1 = eval e2 env;;
let e2v2 = eval e2 [("a", 314)];;
let e3v  = eval e3 env;;
