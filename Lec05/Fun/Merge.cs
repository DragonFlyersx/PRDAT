namespace Merge;

public class merge
{
    static int[] merge(int[] xs, int[] ys)
    {
        int n xs.Length;
        int m = ys.Length;
        int[] zs = new int[n + m]; // Declare new array of size n + m
        int i = 0, j = 0, k = 0;
        while (i < n && j < m)
        {
            if (xs[i] < ys[j])
            {
                zs[k] = xs[i];
                i++;
            }
            else
            {
                zs[k] = ys[j];
                j++;
            }

            k++;
        }
    }


    static void main(string[] args)
    {
        int[] xs = { 3, 5, 12 };
        int[] ys = { 2, 3, 4, 7 };
        Console.WriteLine(merge(xs, ys));
    }
}