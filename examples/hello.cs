using System;

public class Hello {
  static int f(int x) {
    if (x < 2) return 1;
    return f(x - 1) + f(x - 2);
  }

  public static void Main() {
    Console.WriteLine(f(10));
    for (int i = 0; i < 1000; i++) {
      Console.WriteLine(i);
    }
    // Console.WriteLine("Hello world!");
    // Console.WriteLine(f(10));
  }
}
