module Exercise_11_8

// i

// Write an expression that produces and prints the values 3 5 7 9. 
// 2×x+1 for x in 1..4   
let task1a = Every(Write(Prim("+", Prim("*", CstI 2, FromTo(1, 4)), CstI 1)))

// Write an expression that produces and prints the values 21 22 31 32 41 42.
// x×10+y where x in 2..4 and y in 1..2
let task1b = Every(Write(Prim("+", Prim("*", FromTo(2, 4), CstI 10), FromTo(1, 2))))

// ii
// write an expression that prints the least multiple of 7 that is greater than 50.
let task2 = Write(Prim("<", CstI 50, Prim("*", CstI 7, FromTo(1, 100)))) // = 56

// iii
// Changes / additions are in icon.fs:
// - expr
// - eval

    
// Test sqr
let testSqr = Every(Write(Prim1("sqr", FromTo(3, 6)))) // = 9 16 25 36

// Test even
let testEven = Every(Write(Prim1("even", FromTo(1, 7)))) // = 2 4 6

// We dont know for sure how to test multiples, but here is an attempt
let testVisual = And(Write(Prim1("multiples", CstI 3)), Fail) // Should print forever multiples of 3