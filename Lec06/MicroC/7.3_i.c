
// takes the sum of an array of integers
//takes a element from th list and adds it to the sum 
// then Increase i as it moves to the next element in the array,
// in main we create the ineger sump, and an array of 4 integers and pouplate the indexes with integers.
// we then call the function arrsum with the array and the address of sump
// to store the result. and then print the sum at the end.

void arrsum(int n, int arr[], int *sump) {
    int i;
    *sump = 0;
    for (i=0; i<n; i=i+1) {
        *sump = *sump + arr[i];
    }
}

void main() {
    int arr[4];
    int sump;
    int i;

    
    arr[0] = 7;
    arr[1] = 13;
    arr[2] = 9;
    arr[3] = 8;

    
    arrsum(4, arr, &sump);

    
    print(sump);
    println;  
}