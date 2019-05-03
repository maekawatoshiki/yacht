using System;

class BigIntList {
  public int n;
  public BigIntList next;

  public BigIntList(int n_) {
    n = n_;
    next = null;
  }

  public void Dump() {
    if (next != null) {
      next.Dump();
    }

    Console.Write(n);
  }
}

class BigInt {
  public BigIntList list;

  public BigInt(int n) {
    list = new BigIntList(n);
  }

  public BigInt Add(BigInt val) {
    int carry = 0;
    BigInt c = new BigInt(0);
    BigIntList al = list;
    BigIntList bl = val.list;
    BigIntList cl = c.list;

    for (;;) {
      cl.n += carry;
      if (al != null) {
        cl.n += al.n;
        al = al.next;
      }
      if (bl != null) {
        cl.n += bl.n;
        bl = bl.next;
      }
      carry = cl.n / 1000000000;
      cl.n %=        1000000000;
      
      if (al != null || bl != null || carry > 0) {
        cl.next = new BigIntList(0);
        cl = cl.next;
      } else break;
    }

    return c;
  }

  public BigInt Mul(BigInt val, int n) {
    var ret = new BigInt(0);
    for (int i = 0; i < n; i++) {
      ret = ret.Add(val);
    }
    return ret;
  }

  public void Dump() {
    list.Dump();
    Console.WriteLine("");
  }
}

public class A {
  public static void Main() {
    var n = new BigInt(1);
    for (int i = 0; i < 100; i++) {
      n = n.Mul(n, i + 1);
      Console.Write(i);
      Console.Write("! = ");
      n.Dump();
    }
  }
}
