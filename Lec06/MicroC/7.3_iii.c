

void histogram(int freqLen, int ns[], int max, int freq[]) {
    int i;
    int c;

    /* Initialize freq array to zero in every index */
    for (i=0; i <= max; i=i+1){
        freq[i] = 0;
    }

    /* Count frequencies */
    for (i=0; i < freqLen; i=i+1){
        c = ns[i];        /* value at position i */
        freq[c] = freq[c] + 1;
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

    for (i=0; i<=3; i=i+1) {
        print(freq[i]);
    }
    println;
}