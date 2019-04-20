using System;

public class Hello {
  static int f(int x) {
    if (x < 10) {
      Console.WriteLine(x);
      f(x + 1);
    }
    return x;
  }

  public static void Main() {
    Console.WriteLine("Hello world!");
    f(0);
  }
}
