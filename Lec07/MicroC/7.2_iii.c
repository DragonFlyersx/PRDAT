

void histogram(int freqLen, int ns[], int max, int freq[]) {
    int i;
    int c;

    /* Initialize freq array to zero in every index */
    i = 0;
    while (i <= max) {
        freq[i] = 0;
        i = i + 1;
    }

    /* Count frequencies */
    i = 0;
    while (i < freqLen) {
        c = ns[i];        /* value at position i */
        freq[c] = freq[c] + 1;
        i = i + 1;
    }
}

void main() {
    int arr[7];
    int freq[4];
    int i;

    /* Fill arr with example values */
    arr[0] = 1;
    arr[1] = 2;
    arr[2] = 1;
    arr[3] = 1;
    arr[4] = 1;
    arr[5] = 2;
    arr[6] = 0;

    /* Call histogram */
    histogram(7, arr, 3, freq);
 

    /* Print freq array */
    i = 0;
    while (i <= 3) {
        print(freq[i]);
        i = i + 1;
    }
    println;
}