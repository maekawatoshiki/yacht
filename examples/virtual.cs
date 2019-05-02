using System;

class Shape {
  public virtual void Say() {
    Console.WriteLine("Shape");
  }
}

class Triangle : Shape {
  public override void Say() {
    Console.WriteLine("Triangle");
  }
}

class Rectangle : Shape {
  public override void Say() {
    Console.WriteLine("Rectangle");
  }
}

public class Sample {
  public static void Main() {
    Shape shape = new Shape();
    shape.Say();

    shape = new Triangle();
    shape.Say();

    shape = new Rectangle();
    shape.Say();
  }
}
