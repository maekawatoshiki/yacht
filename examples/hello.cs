using System;

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
    Console.WriteLine("Hello world!");
    Console.WriteLine(fibo(10));
    for (int i = 2; i < 10; i++) {
      if (is_prime(i)) Console.WriteLine(i);
    }
  }
}
