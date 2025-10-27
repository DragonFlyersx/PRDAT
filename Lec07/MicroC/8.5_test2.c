int main() {
  int x;
  x = 0;
  // only the else branch should set x to 6 (expect return 6)
  x = (0 ? (x = 5) : (x = 6));
  return x;
}