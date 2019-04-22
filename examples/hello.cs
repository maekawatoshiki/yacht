using System;

public class Hello {
  static int fibo(int x) {
    if (x < 2) return 1;
    return fibo(x - 1) + fibo(x - 2);
  }

  // static 

  public static void Main() {
    Console.WriteLine("Hello world!");
    Console.WriteLine(fibo(10));
  }
}
