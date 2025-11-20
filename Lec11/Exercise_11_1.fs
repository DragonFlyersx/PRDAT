module Exercise_11_1
    
// i
// Continuation version
let rec lenc xs k =
    match xs with
    | [] -> k 0
    | x::xr -> lenc xr (fun v -> k (1 + v))
    
// Calling with identity
let id = fun v -> v
let res1 = lenc [2; 5; 7] id

// Calling with printf
lenc [2; 5; 7] (fun v -> printf "The answer is '%d'\n" v)

// ii
// What happens if you call it as lenc xs (fun v -> 2*v) instead?
// ANSWER: It will compute the length of the list and then multiply it by 2
let res2 = lenc [2; 5; 7] (fun v -> 2 * v) // Result is 6

// iii
    
// Accumulator version
let rec leni xs acc =
    match xs with
    | [] -> acc
    | x::xr -> leni xr (acc + 1)
    
let res3 = leni [2; 5; 7] 0

// What is the relation between lenc and leni?
// ANSWER: Both lenc and leni allow the function to be tail-recursive
// (no stack frame is held waiting for the recursive call to return).
// However, they achieve this differently:
// In leni with accumulator, the count is stored in the integer argument acc. It is computed on the way down the recursion.
// In lenc with continuation, the count is computed on the way back up the recursion, using the continuation k to build up the final result.