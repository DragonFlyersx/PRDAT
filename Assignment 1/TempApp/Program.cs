using System;

abstract class Expr
{
    public abstract override string ToString();
    
}

class CstI : Expr
{
    protected readonly int i;
    public CstI(int i) { this.i = i; }
    public override string ToString() { return i.ToString(); }
}

class Var : Expr
{
    protected readonly string num;
    public Var(string num) { this.num = num; }
    public override string ToString() { return num.ToString(); }
}

abstract class Binop : Expr
{
    protected readonly Expr e1;
    protected readonly Expr e2;
    public Binop(Expr e1, Expr e2) { this.e1 = e1; this.e2 = e2; }
}

class Add : Binop {
    public Add(Expr e1, Expr e2) : base(e1, e2){}
    public override string ToString() { return $"( {e1} + {e2} )"; }
}

class Mul : Binop
{
    public Mul(Expr e1, Expr e2) : base(e1, e2){}

    public override string ToString()
    {
        return $"( {e1} * {e2} )";
    }

}

class Sub : Binop
{
    public Sub(Expr e1, Expr e2) : base (e1,e2){}

    public override string ToString()
    {
        return $"( {e1} - {e2} )";
    }
}

class Program
{
    static void Main()
    {
        // Example 1: 17 + z
        Expr e1 = new Add(new CstI(17), new Var("z"));
        Console.WriteLine("e1 = " + e1);

        // Example 2: (3 * x) + 5
        Expr e2 = new Add(new Mul(new CstI(3), new Var("x")), new CstI(5));
        Console.WriteLine("e2 = " + e2);

        // Example 3: (a - 4) * (b + 2)
        Expr e3 = new Mul(
            new Sub(new Var("a"), new CstI(4)),
            new Add(new Var("b"), new CstI(2))
        );
        Console.WriteLine("e3 = " + e3);

        // Example 4: ((10 - y) + (x * 2)) * (z - 1)
        Expr e4 = new Mul(
            new Add(
                new Sub(new CstI(10), new Var("y")),
                new Mul(new Var("x"), new CstI(2))
            ),
            new Sub(new Var("z"), new CstI(1))
        );
        Console.WriteLine("e4 = " + e4);
    }
}
