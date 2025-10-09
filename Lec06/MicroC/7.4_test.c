void preinc() {
    int x;
    x = 5;
    print(++x);
    println;
}

void predec() {
    int x;
    x = 5;
    print(--x);
    println;
}

void preincArray() {
    int arr[5];
    arr[1] = 5;
    ++arr[1];
    print(arr[1]);
    println;
}

void predecArray() {
    int arr[5];
    arr[1] = 5;
    --arr[1];
    print(arr[1]);
    println;
}

void main() {

    preinc();
    predec();
    preincArray();
    predecArray();
    
}
