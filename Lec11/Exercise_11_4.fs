module Exercise_11_4

// We optimize the function by checking for 0. 
// If a 0 is found, we immediately call the current continuation k with 0. 
// This skips processing the rest of the list (xr).
let rec prodc xs k =
    match xs with
    | [] -> k 1
    | x::xr -> 
        if x = 0 then k 0 
        else prodc xr (fun v -> k (x * v))

let id = fun v -> v
let res = prodc [2; 5; 0; 10] id // = 0
let res2 = prodc [2; 5; 10] id // = 100

// tail recursive version
let rec prodi xs acc =
    match xs with
    | [] -> acc
    | x::xr -> 
        if x = 0 then 0 
        else prodi xr (acc * x)

let res3 = prodi [2; 5; 0; 7] 1 // = 0
let res4 = prodi [2; 5; 7] 1 // = 70

// The smart thing with the accumulator version is that it returns immediately when a 0 is found,
// whereas the continuation version still has to execute its chain of continuations.
// Both saves time by not traversing the rest of the list.