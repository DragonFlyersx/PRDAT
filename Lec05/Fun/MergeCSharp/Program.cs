class Program
{
    static int[] merge(int[] xs, int[] ys)
    {
        // Create a new array to hold the merged result
        int[] result = new int[xs.Length + ys.Length];
        int i = 0, j = 0, k = 0;

        // While there are elements in both arrays
        while (i < xs.Length && j < ys.Length)
        {
            if (xs[i] < ys[j]) // If current element in xs is smaller
                result[k++] = xs[i++]; // Add it to result and move to next in xs
            else
                result[k++] = ys[j++]; // Add current element in ys to result and move to next in ys
        }

        // If there are remaining elements in xs, add them
        while (i < xs.Length)
            result[k++] = xs[i++];

        // If there are remaining elements in ys, add them
        while (j < ys.Length)
            result[k++] = ys[j++];

        return result;
    }
    
    static void Main(string[] args)
    {
        int[] xs = { 3, 5, 12 };
        int[] ys = { 2, 3, 4, 7 };
        int[] merged = merge(xs, ys);
        Console.WriteLine(string.Join(", ", merged));
    }
}