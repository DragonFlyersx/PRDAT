int main() {
    int i;
    i = 0;
    int arr[3];
    arr[0] = 10;
    arr[1] = 20;
    arr[2] = 30;
    ++i;
    ++arr[++i];
    return arr[1] + i;
}