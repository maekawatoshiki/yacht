using System;

public class Vec {
  public int x = 0;
  public int y = 0;
  public int norm2() {
    return x * x + y * y;
  }
}

public class Node {
  public bool isNum;
  public char num;
  public char op;
  public Node lhs;
  public Node rhs;
  public static Node MakeNum(char num) {
    Node node = new Node();
    node.isNum = true;
    node.num = num;
    return node;
  }
  public static Node MakeOp(char op, Node lhs, Node rhs) {
    Node node = new Node();
    node.isNum = false;
    node.op = op;
    node.lhs = lhs;
    node.rhs = rhs;
    return node;
  }
  public void Dump() {
    DumpSub();
    Console.WriteLine("");
  }
  public void DumpSub() {
    if (isNum) DumpNum(); else DumpOp();
  }
  public void DumpNum() {
    Console.Write(" ");
    Console.Write(num);
  }
  public void DumpOp() {
    Console.Write(" (");
    Console.Write(op);
    lhs.DumpSub();
    rhs.DumpSub();
    Console.Write(")");
  }
}

public class Calc {
  public string expr;
  public int pos, len;
  public Calc(string expr_) {
    expr = expr_;
    pos = 0;
    len = expr.Length;
  }
  public void Eval() {
    Node node = ExprAdd();
    node.Dump();
  }
  Node ExprAdd() {
    Node left = ExprMul();
    while (!End() && GetChar() == '+') {
      char op = NextChar();
      Node right = ExprMul();
      left = Node.MakeOp(op, left, right);
    }
    return left;
  }
  Node ExprMul() {
    Node left = ExprDigit();
    while (!End() && GetChar() == '*') {
      char op = NextChar();
      Node right = ExprDigit();
      left = Node.MakeOp(op, left, right);
    }
    return left;
  }
  Node ExprDigit() {
    return Node.MakeNum(NextChar());
  }
  bool End() {
    return pos >= len;
  }
  char NextChar() {
    return expr[pos++];
  }
  char GetChar() {
    return expr[pos];
  }
}

public class Hello {
  static int fibo(int x) {
    if (x < 2) return 1;
    return fibo(x - 1) + fibo(x - 2);
  }

  static bool is_prime(int n) {
    if (n == 2) return true;
    if (n % 2 == 0 || n <= 1) return false;
    for (int k = 3; k * k <= n; k += 2) 
      if (n % k == 0) 
        return false;
    return true;
  }

  public static void Main() {
    Console.WriteLine("hello world");

    Console.WriteLine(fibo(10));

    for (int i = 2; i < 10; i++) {
      if (is_prime(i)) Console.WriteLine(i);
    }
    
    Vec v = new Vec();
    Console.WriteLine(v.norm2());
    v.x = 2; v.y = 3;
    Console.WriteLine(v.norm2());

    Calc calc = new Calc("1*2+3+4*5");
    calc.Eval();
  }
}
