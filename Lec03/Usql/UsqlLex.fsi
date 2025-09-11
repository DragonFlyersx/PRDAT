
module UsqlLex
open System.Collections.Generic      (* for Dictionary *)
open (*Microsoft.*)FSharp.Text.Lexing
open UsqlPar;/// Rule Token
val Token: lexbuf: LexBuffer<char> -> token
/// Rule SkipToEndLine
val SkipToEndLine: lexbuf: LexBuffer<char> -> token
/// Rule String
val String: chars: obj -> lexbuf: LexBuffer<char> -> token
