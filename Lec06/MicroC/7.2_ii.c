
// sum of squares in an array

void squares(int n, int arr[]) {
    int i;
    i = 0;
    while (i < n) {
        arr[i] = i * i;
        i = i + 1;
    }
}

void arrsum(int n, int arr[], int *sump) {
    int i;
    i = 0;
    *sump = 0;
    while (i<n) {
        *sump = *sump + arr[i];
        i = i + 1;
    }
}

void main(int n) {
    int arr[20];
    int sum;
    squares(n, arr);
    arrsum(n, arr, &sum);
    print(sum);
    println;
}
