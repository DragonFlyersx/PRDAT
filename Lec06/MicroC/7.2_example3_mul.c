// Example:
// run (fromFile "7.2_example3_mul.c") [2;2000];;

void main(int n, int m) {
    while (n < m) {
        print n;
        n = n * 2;
    }
    println;
}

