using System;

public class Prime {
  static bool is_prime(int n) {
    if (n == 2) return true;
    if (n % 2 == 0 || n <= 1) return false;
    for (int k = 3; k * k <= n; k += 2) 
      if (n % k == 0) 
        return false;
    return true;
  }

  static void eratosthenes_sieve(int max) {
    bool[] a = new bool[max];
    a[0] = false;
    for (int i = 1; i < max; i++)
      a[i] = true;
    for (int i = 0; i * i < max; i++) 
      if (a[i])
        for (int k = i + 1; (i + 1) * k <= max; k++)
          a[(i + 1) * k - 1] = false;
    for (int i = 0; i < max; i++) 
      if (a[i]) Console.WriteLine(i + 1);
  }

  public static void Main() {
    for (int i = 2; i < 100; i++) {
      if (is_prime(i)) Console.WriteLine(i);
    }

    eratosthenes_sieve(100);
  }
}
