using System;

public class Node {
  public virtual void Dump() {}
  public virtual int Eval() { return 0; }
}

class OpNode : Node {
  public char op;
  public Node lhs;
  public Node rhs;

  public OpNode(char op_, Node lhs_, Node rhs_) {
    op = op_;
    lhs = lhs_;
    rhs = rhs_;
  }

  public override void Dump() {
    Console.Write(" (");
    Console.Write(op);
    lhs.Dump();
    rhs.Dump();
    Console.Write(")");
  }

  public override int Eval() {
    int l = lhs.Eval(), r = rhs.Eval();
    if (op == '+') return l + r;
    if (op == '*') return l * r;
    return 0;
  }
}

public class NumNode : Node {
  public char num;

  public NumNode(char num_) {
    num = num_;
  }

  public override void Dump() {
    Console.Write(" ");
    Console.Write(num);
  }

  public override int Eval() { return num - '0'; }
}

public class Parser {
  public string expr;
  public int pos, len;

  public Parser(string expr_) {
    expr = expr_;
    pos = 0;
    len = expr.Length;
  }

  public Node Parse() {
    Console.Write("Expression: ");
    Console.WriteLine(expr);
    return ExprAdd();
  }

  Node ExprAdd() {
    Node left = ExprMul();
    while (!End() && GetChar() == '+') {
      char op = NextChar();
      Node right = ExprMul();
      left = new OpNode(op, left, right);
    }
    return left;
  }

  Node ExprMul() {
    Node left = ExprDigit();
    while (!End() && GetChar() == '*') {
      char op = NextChar();
      Node right = ExprDigit();
      left = new OpNode(op, left, right);
    }
    return left;
  }

  Node ExprDigit() {
    return new NumNode(NextChar());
  }

  bool End() { return pos >= len; }
  char NextChar() { return expr[pos++]; }
  char GetChar() { return expr[pos]; }
}

public class Calc {
  public static void Main() {
    var parser = new Parser("1+2*3+4");
    var node = parser.Parse();

    Console.Write("S expr:");
    node.Dump();
    
    Console.Write("\nEval: ");
    Console.WriteLine(node.Eval());
  }
}
