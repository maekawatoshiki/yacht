using System;

public class Vec {
  public int x;
  public int y;
  public Vec() {
    x = 0;
    y = 0;
  }
  public int norm2() {
    return x * x + y * y;
  }
}

public class Node {
  public bool is_num;
  public char num;
  public char op;
  public Node lhs;
  public Node rhs;
  public static Node make_num(char num) {
    Node node = new Node();
    node.is_num = true;
    node.num = num;
    return node;
  }
  public static Node make_op(char op, Node lhs, Node rhs) {
    Node node = new Node();
    node.is_num = false;
    node.op = op;
    node.lhs = lhs;
    node.rhs = rhs;
    return node;
  }
  public void dump() {
    dump_sub();
    Console.WriteLine("");
  }
  public void dump_sub() {
    if (is_num) {
      dump_num();
    } else {
      dump_op();
    }
  }
  public void dump_num() {
    Console.Write(" ");
    Console.Write(num);
  }
  public void dump_op() {
    Console.Write(" (");
    Console.Write(op);
    lhs.dump_sub();
    rhs.dump_sub();
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
  public void eval() {
    Node node = expr_add();
    node.dump();
  }
  public Node expr_add() {
    Node left = expr_digit();
    while (!end() && get() == '+') {
      char op = next();
      Node right = expr_digit();
      left = Node.make_op(op, left, right);
    }
    return left;
  }
  public Node expr_digit() {
    return Node.make_num(next());
  }
  bool end() {
    return pos >= len;
  }
  char next() {
    return expr[pos++];
  }
  char get() {
    return expr[pos];
  }
  bool cur_char_is_digit() {
    char c = expr[pos];
    if (c == '0' || c == '1' || c == '2' || 
        c == '3' || c == '4' || c == '5' || 
        c == '6' || c == '7' || c == '8' || 
        c == '9') return true;
    return false;
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
    Console.WriteLine(fibo(10));
    for (int i = 2; i < 10; i++) {
      if (is_prime(i)) Console.WriteLine(i);
    }
    
    Vec v = new Vec();
    Console.WriteLine(v.norm2());
    v.x = 2; v.y = 3;
    Console.WriteLine(v.norm2());
    Calc calc = new Calc("1+2+3");
    calc.eval();
  }
}
