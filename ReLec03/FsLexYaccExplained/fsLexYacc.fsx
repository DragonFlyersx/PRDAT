(* This file expects the Expr example to be loaded in interactive *)


open (*Microsoft.*)FSharp.Text
open Absyn

let str = "2+4"
let lexbuf =  Lexing.LexBuffer<char>.FromString(str)

let nextToken (lexbuf : Lexing.LexBuffer<char>) : ExprPar.token =
  if not lexbuf.IsPastEndOfStream
  then ExprLex.Token lexbuf
  else ExprPar.EOF

let listTokens (lexbuf : Lexing.LexBuffer<char>) =
  let rec listTokens' () =
    if lexbuf.IsPastEndOfStream
    then []
    else ExprLex.Token lexbuf :: listTokens' ()
  listTokens' ()

let parse (lexbuf : Lexing.LexBuffer<char>) =
  try 
    ExprPar.Main ExprLex.Token lexbuf
  with 
    | exn -> let pos = lexbuf.EndPos 
             failwithf "%s near line %d, column %d\n" 
                (exn.Message) (pos.Line+1) pos.Column
    
    
