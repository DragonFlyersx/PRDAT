using System; // 1.4.1

abstract class Expr
{
    public abstract override string ToString();
    public abstract int eval(Dictionary<string,int> env); // 1.4.3
    public abstract Expr simplify();
}

class CstI : Expr
{
    protected readonly int i;
    public CstI(int i) { this.i = i; }
    public override string ToString() { return i.ToString(); }
    public override int eval(Dictionary<string,int> env) { return i; } // 1.4.3
    public override Expr simplify() { return this; }
}

class Var : Expr
{
    protected readonly string num;
    public Var(string num) { this.num = num; }
    public override string ToString() { return num.ToString(); }

    public override int eval(Dictionary<string, int> env) // 1.4.3
    {
        return env[num];
    }
    
    public override Expr simplify() { return this; }

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
    
    public override int eval(Dictionary<string,int> env) // 1.4.3
    {        return e1.eval(env) + e2.eval(env);    }
    
    public override Expr simplify()
    {
        var zero = new CstI (0);
        var s1 = e1.simplify();
        var s2 = e2.simplify();
        // Check if s1 is constant and is 0.
        if (s1 is CstI c1 && c1.eval(null) == 0) { return s2; }
        // Check if s2 is constant and is 0.
        else if (s2 is CstI c2 && c2.eval(null) == 0) { return s1; }
        // If both sides are constants, return their sum as a constant.
        else { return new Add(s1, s2); }
    }
    
}

class Mul : Binop
{
    public Mul(Expr e1, Expr e2) : base(e1, e2){}

    public override string ToString()
    {
        return $"( {e1} * {e2} )";
    }
    public override int eval(Dictionary<string,int> env) // 1.4.3
    {        return e1.eval(env) * e2.eval(env);    }

    public override Expr simplify()
    {
        var s1 = e1.simplify();
        var s2 = e2.simplify();
        // If either side is constant 0, return 0
        if (s1 is CstI c1 && c1.eval(null) == 0 || s2 is CstI c2 && c2.eval(null) == 0)
            return new CstI(0);
        // If left side is 1, return right side
        if (s1 is CstI c3 && c3.eval(null) == 1)
            return s2;
        // If right side is 1, return left side
        if (s2 is CstI c4 && c4.eval(null) == 1)
            return s1;
        // Otherwise, return the (possibly simplified) multiplication
        return new Mul(s1, s2);
    }

}

class Sub : Binop
{
    public Sub(Expr e1, Expr e2) : base (e1,e2){}
    

    public override string ToString()
    {
        return $"( {e1} - {e2} )";
    }
    public override int eval(Dictionary<string,int> env) // 1.4.3
    {        return e1.eval(env) - e2.eval(env);    }
    
    public override Expr simplify()
    {
        var s1 = e1.simplify();
        var s2 = e2.simplify();
        // If right side expression is constant 0, return left side expression
        if (s2 is CstI c2 && c2.eval(null) == 0) { return s1; }
        // If both sides are constants, return their difference as a constant
        else if (s1 is CstI c1 && s2 is CstI c2b)
        {
            return new CstI(c1.eval(null) - c2b.eval(null));
        }
        else { return new Sub(s1, s2); }
    }
}

class Program
{
    static void Main()
    {
        // Example 1: 17 + z
        Expr e1 = new Add(new CstI(17), new Var("z")); // 1.4.2
        Console.WriteLine("e1 = " + e1);

        // Example 2: (3 * x) + 5
        Expr e2 = new Add(new Mul(new CstI(3), new Var("x")), new CstI(5)); // 1.4.2
        Console.WriteLine("e2 = " + e2);

        // Example 3: (a - 4) * (b + 2)
        Expr e3 = new Mul( // 1.4.2
            new Sub(new Var("a"), new CstI(4)),
            new Add(new Var("b"), new CstI(2))
        );
        Console.WriteLine("e3 = " + e3);

        // Example 4: ((10 - y) + (x * 2)) * (z - 1)
        Expr e4 = new Mul( // 1.4.2
            new Add(
                new Sub(new CstI(10), new Var("y")),
                new Mul(new Var("x"), new CstI(2))
            ),
            new Sub(new Var("z"), new CstI(1)) // 1.4.2
        );
        Console.WriteLine("e4 = " + e4); // 1.4.2
        
        //Example 5 (with simplification): (0 + x) + (y - 0) + (0 * z)
        Expr e5 = new Mul( // 1.4.2
            new Add(new CstI(0), new Var("x")),
            new Mul(
                new Sub(new Var("y"), new CstI(0)),
                new Mul(new CstI(1), new Var("z"))
            )
        );
        Console.WriteLine("e5 = " + e5); // 1.4.2
        
        // 1.4.3
        
        var env = new Dictionary<string, int> { { "x", 2 }, { "y", 3 }, { "z", 4 }, { "a", 5 }, { "b", 6 } };
        Console.WriteLine("e1 evaluated = " + e1.eval(env));
        Console.WriteLine("e2 evaluated = " + e2.eval(env));
        Console.WriteLine("e3 evaluated = " + e3.eval(env));
        Console.WriteLine("e4 evaluated = " + e4.eval(env));
        
        
        // 1.4.4
        Console.WriteLine("e1 simplified = " + e1.simplify());
        Console.WriteLine("e2 simplified = " + e2.simplify());
        Console.WriteLine("e3 simplified = " + e3.simplify());
        Console.WriteLine("e4 simplified = " + e4.simplify());
        Console.WriteLine("e5 simplified = " + e5.simplify());
        // Expr simplifyExpr = new Add(new CstI (0), new CstI(2));
        // Console.WriteLine("simplifyExpr simplified = " + simplifyExpr.simplify());
    }
    /*
    let test2 = Mul(Add(CstI 1, CstI 0),Add(Var "x", CstI 0)) // test case
    let test3 = Add(CstI 0,Var "x") // test case

    let test4 = Add(CstI 1, CstI 0) // test case

    let bad = Add(Var "x", Var "y")

    let bad2 = Add(CstI 0, CstI 0)
    */
    
    
    
}
