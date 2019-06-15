using System;

public class Random {  
  private uint x;  
  private uint y;  
  private uint z;  
  private uint w;  

  public Random(uint seed) {  
    SetSeed(seed);  
  }  

  public void SetSeed(uint seed) {  
    x = 521288629;  
    y = 341235113;  
    z = seed;  
    w = x ^ z;  
  }  

  public uint Next() {  
    uint t = x ^ (x << 11);  
    x = y;  
    y = z;  
    z = w;  
    w = (w ^ (w >> 19)) ^ (t ^ (t >> 8));  
    return w;
  } 

  public uint Next(int max) {
    return Next() % (uint)max;
  }
}  

public class Prime {
  public static bool MillerRabinTest(int n, Random r) {
    if ((n < 2) || (n % 2 == 0)) return n == 2;
    
    int s = n - 1;
    while (s % 2 == 0) s >>= 1;

    for (int i = 0; i < 20; i++) {
      int a = (int)r.Next(n - 1) + 1;
      int temp = s;
      long mod = 1;
      for (int j = 0; j < temp; ++j) mod = (mod * a) % n;
      while (temp != n - 1 && mod != 1 && mod != n - 1) {
        mod = (mod * mod) % n;
        temp *= 2;
      }
      if (mod != n - 1 && temp % 2 == 0) return false;
    }

    return true;
  }

  static bool IsPrime(int n) {
    if (n == 2) return true;
    if (n % 2 == 0 || n <= 1) return false;
    for (int k = 3; k * k <= n; k += 2) 
      if (n % k == 0) 
        return false;
    return true;
  }

  static void EratosthenesSieve(int max) {
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
    Console.WriteLine("Normal");
    for (int i = 2; i < 100; i++) {
      if (IsPrime(i)) Console.WriteLine(i);
    }

    Console.WriteLine("Eratosthenes Sieve");
    EratosthenesSieve(100);

    Console.WriteLine("Miller Rabin");
    Random r = new Random(13245);
    for (int i = 100; i < 300; i++) {
      if (MillerRabinTest(i, r)) Console.WriteLine(i);
    }
  }
}
