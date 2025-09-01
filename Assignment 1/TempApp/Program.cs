using System;

abstract class Expr
{
    public abstract override string ToString();
}

class CstI : Expr
{
    protected readonly int i;

    public CstI(int i)
    {
        this.i = i;
    }

    public override string ToString()
    {
        return i.ToString();
    }
}

class Var : Expr
{
    protected readonly string num;

    public Var(string num)
    {
        this.num = num;
    }

    public override string ToString()
    {
        return num.ToString();
    }
}

abstract class Binop : Expr
{
    protected readonly Expr e1;
    protected readonly Expr e2;

    public Binop(Expr e1, Expr e2)
    {
        this.e1 = e1;
        this.e2 = e2;
    }
}

class Add : Binop {
    public Add(Expr e1, Expr e2) : base(e1, e2){}


    public override string ToString()
    {
        return $"( {e1} + {e2} )";
    }
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
        Expr e = new Add(new CstI(17), new Var("z"));
        Console.WriteLine(e.ToString());
    }
}
