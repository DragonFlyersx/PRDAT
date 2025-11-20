module Exercise_11_3

// Continuation version of prod
let rec prodc xs k =
    match xs with
    | [] -> k 1
    | x::xr -> prodc xr (fun v -> k (x * v))

let id = fun v -> v
let res = prodc [2; 4; 8] id // = 64