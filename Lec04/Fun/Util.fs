(* The use of ! and := is depricated, see
   https://github.com/fsharp/fslang-design/blob/main/FSharp-6.0/FS-1111-refcell-op-information-messages.md

   Below introduces the operators again.
*)

module Util

let (!) (r: 'T ref)  = r.Value
let (:=) (r: 'T ref) (v: 'T)  = r.Value <- v
//let incr (r: int ref)  = r.Value <- r.Value + 1
//let decr (r: int ref)  = r.Value <- r.Value - 1


let rec sum n =
   if n = 0 then 0
   else n + sum (n-1);

let rec power e =
   if e = 0 then 1
   else 3 * power (e-1);;

// 3^11 + 3^10 + 3^9 + 3^8 + (...) + 3^1 + 3^0
let rec sumpower e =
   if e = 0 then 1
   else power e + sumpower(e-1);;

