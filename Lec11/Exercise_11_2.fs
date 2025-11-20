module Exercise_11_2

// i - Write a continuation-passing version revc of the list reversal function rev
let rec revc xs k = 
    match xs with
    | [] -> k []
    | x::xr -> revc xr (fun v -> k (v @ [x]))

// Testing
let id = fun v -> v
revc [1; 2; 3] id // = [3; 2; 1]

// ii - What happens if you call it as revc xs (fun v -> v @ v) instead?
revc [1; 2; 3] (fun v -> v @ v) // = [3; 2; 1; 3; 2; 1]

// ANSWER: It will reverse the list and then concatenate the reversed list with itself.

// iii - Write an accumulator version of list reversal function
let rec revi xs acc =
    match xs with
    | [] -> acc
    | x::xr -> revi xr (x :: acc)

revi [1; 2; 3] [] // = [3; 2; 1]