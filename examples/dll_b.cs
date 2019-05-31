using System;
public class B {
  int x, y;
  public B(int x_, int y_) {
    x = x_; y = y_;
  }
  public void show() {
    Console.WriteLine("[class B] " + (x + y));
  }
  public static void hi() {
    C.hi();
    Console.WriteLine("[class B] Hi");
  }
}
