// Example:
// run (fromFile "7.2_example1_fib.c") [10];;

// Can cause integer overflow if n is too big

void main(int n) {
    int a; // Cannot declare and initalize in the same line
    int b;
    int temp;
    a = 0;
    b = 1;

    while (n > 0) {
        print(a);
        temp = a + b;
        a = b;
        b = temp;
        n = n - 1;
    }
    println;
}
