(* File Fun/Fun.fs
   A strict functional language with integers and first-order 
   one-argument functions * sestoft@itu.dk

   Does not support mutually recursive function bindings.

   Performs tail recursion in constant space (because F# does).
*)

module Fun

open Absyn

(* Environment operations *)

type 'v env = (string * 'v) list

let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

(* A runtime value is an integer or a function closure *)

type value = 
  | Int of int
  | Closure of string * string list * expr * value env       (* (f, [x], fBody, fDeclEnv) *)

// A Closure is a tuple of the function name, the list of parameter names, the function body, and the environment at the point of function declaration. The environment is needed to support static scoping.

let rec eval (e : expr) (env : value env) : int =
    match e with 
    | CstI i -> i
    | CstB b -> if b then 1 else 0
    | Var x ->
        match lookup env x with
        | Int i -> i 
        | _     -> failwith "eval Var"
    | Prim(ope, e1, e2) -> 
        let i1 = eval e1 env
        let i2 = eval e2 env
        match ope with
        | "*" -> i1 * i2
        | "+" -> i1 + i2
        | "-" -> i1 - i2
        | "=" -> if i1 = i2 then 1 else 0
        | "<" -> if i1 < i2 then 1 else 0
        | _   -> failwith ("unknown primitive " + ope)
    | Let(x, eRhs, letBody) -> 
        let xVal = Int(eval eRhs env)
        let bodyEnv = (x, xVal) :: env
        eval letBody bodyEnv
    | If(e1, e2, e3) -> 
        let b = eval e1 env
        if b <> 0 then eval e2 env
        else eval e3 env
    | Letfun(f, xs, fBody, letBody) ->
        let bodyEnv = (f, Closure(f, xs, fBody, env)) :: env
        eval letBody bodyEnv
    | Call(eFun, eArgs) ->
        match eFun with
        | Var f ->
            let fClosure = lookup env f
            match fClosure with
            | Closure (f, xs, fBody, fDeclEnv) ->
                if List.length xs <> List.length eArgs then // If the number of parameters is not equal to the number of arguments
                    failwith ("function " + f + " expects " + string(List.length xs) + " arguments, got " + string(List.length eArgs))
                else
                    let argValues = List.map (fun eArg -> Int(eval eArg env)) eArgs
                    let paramBindings = List.zip xs argValues // Create a list of parameter bindings by zipping the parameter names with the argument values. 
                    let fBodyEnv = paramBindings @ (f, fClosure) :: fDeclEnv // The function body environment extends the function declaration environment with the bindings for the function name and the parameter names. The parameter bindings are added first to ensure that they shadow any bindings in the declaration environment.
                    eval fBody fBodyEnv 
            | _ -> failwith "eval Call: not a function"
        | _ -> failwith "eval Call: not first-order function"

// Different environments explainted:

// The function declaration environment (fDeclEnv) is the environment that was in effect at the point where the function was declared. 
// This environment is captured in the closure when the function is defined. It is used to support static scoping, 
// which means that a function can access variables that were in scope at the point of its declaration, 
// even if it is called from a different environment.

// The function body environment (fBodyEnv) is the environment that is used when evaluating the function body. 
// It is created by extending the function declaration environment with the bindings for the function name and the parameter names. 
// This environment is used to evaluate the function body when the function is called.

// The let body environment (bodyEnv) is the environment that is used when evaluating the body of a let expression. 
// It is created by extending the current environment with the binding for the variable defined in the let expression. 
// This environment is used to evaluate the let body, 
// and it is also used to create the function body environment when a function is defined within a let expression.


(* Evaluate in empty environment: program must have no free variables: *)

let run e = eval e [];;

(* Examples in abstract syntax *)

let ex1 = Letfun("f1", ["x"], Prim("+", Var "x", CstI 1), 
                 Call(Var "f1", [CstI 12]));;

(* Example: factorial *)

let ex2 = Letfun("fac", ["x"],
                 If(Prim("=", Var "x", CstI 0),
                    CstI 1,
                    Prim("*", Var "x", 
                              Call(Var "fac", 
                                   [Prim("-", Var "x", CstI 1)]))),
                 Call(Var "fac", [Var "n"]));;

 let fac10 = eval ex2 [("n", Int 10)];;


(* Example: deep recursion to check for constant-space tail recursion *)

let ex3 = Letfun("deep", ["x"], 
                 If(Prim("=", Var "x", CstI 0),
                    CstI 1,
                    Call(Var "deep", [Prim("-", Var "x", CstI 1)])),
                 Call(Var "deep", [Var "count"]));;
    
let rundeep n = eval ex3 [("count", Int n)];;

(* Example: static scope (result 14) or dynamic scope (result 25) *)

let ex4 =
    Let("y", CstI 11,
        Letfun("f", ["x"], Prim("+", Var "x", Var "y"),
               Let("y", CstI 22, Call(Var "f", [CstI 3]))));;

(* Example: two function definitions: a comparison and Fibonacci *)

let ex5 = 
    Letfun("ge2", ["x"], Prim("<", CstI 1, Var "x"),
           Letfun("fib", ["n"],
                  If(Call(Var "ge2", [Var "n"]),
                     Prim("+",
                          Call(Var "fib", [Prim("-", Var "n", CstI 1)]),
                          Call(Var "fib", [Prim("-", Var "n", CstI 2)])),
                     CstI 1), Call(Var "fib", [CstI 25])));;
                     
