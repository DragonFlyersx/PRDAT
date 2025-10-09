// Example:
// run (fromFile "7.2_example2_div.c") [200];;

void main(int n) {
    print n;
    while (n % 2 == 0) {
        n = n / 2;
        print n;
    }
    println;
}