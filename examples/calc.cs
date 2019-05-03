using System;

class Node {
  public virtual void Dump() {}
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
}

class NumNode : Node {
  public char num;

  public NumNode(char num_) {
    num = num_;
  }

  public override void Dump() {
    Console.Write(" ");
    Console.Write(num);
  }
}

public class Parser {
  public string expr;
  public int pos, len;

  public Parser(string expr_) {
    expr = expr_;
    pos = 0;
    len = expr.Length;
  }

  public void Parse() {
    Console.Write("Expression: ");
    Console.WriteLine(expr);

    Node node = ExprAdd();
    Console.Write("RPN:");
    node.Dump();
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
    parser.Parse();
  }
}
