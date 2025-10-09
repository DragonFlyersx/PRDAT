
// sum of squares in an array

void squares(int n, int arr[]) {
    int i;
    for (i=0; i<n; i=i+1) {
        arr[i] = i * i;
    }
}

void arrsum(int n, int arr[], int *sump) {
    int i;
    *sump = 0;
    for (i=0; i<n; i=i+1) {
        *sump = *sump + arr[i];
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
