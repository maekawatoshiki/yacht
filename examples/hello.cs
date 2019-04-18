using System;

public class Hello {
  static int f(int x, int y) {
    return x + y;
  }

  public static void Main() {
    Console.WriteLine("Hello world!");
    Console.WriteLine(f(12, 10));
  }
}
