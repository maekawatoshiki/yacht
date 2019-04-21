using System;

public class Hello {
  static int f(int x) {
    // return x == 1 ? 2 : 3;
    if (x < 2) return 1;
    return f(x - 1) + f(x - 2);
  }

  public static void Main() {
    Console.WriteLine(f(10));
    // Console.WriteLine("Hello world!");
    // Console.WriteLine(f(10));
  }
}
