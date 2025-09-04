

(* Ex 2.4 - assemble to integers *)
(* SCST = 0, SVAR = 1, SADD = 2, SSUB = 3, SMUL = 4, SPOP = 5, SSWAP = 6; *)
let sinstrToInt = function
  | SCstI i -> [0;i]
  | SVar i  -> [1;i]
  | SAdd    -> [2]
  | SSub    -> [3]
  | SMul    -> [4]
  | SPop    -> [5]
  | SSwap   -> [6]

// [0; 17; 1; 0; 1; 1; 2; 6; 5]
// [0; 4; 0; 8; 2]

// 17, a=0, b=1, add, swap, pop
// 4, 8, add

// Add(8,4)

let rec assemble (instrs : sinstr list) : int list = 
    instrs 
    |> List.collect sinstrToInt // x [0;17] y [1;0] z [1;1] add [2] swap [6] pop [5] :: :: :: [0;17;1;0;1;1;2;6;5]

(* Output the integers in list inss to the text file called fname: *)

let intsToFile (inss : int list) (fname : string) = 
    let text = String.concat " " (List.map string inss)
    System.IO.File.WriteAllText(fname, text);;
