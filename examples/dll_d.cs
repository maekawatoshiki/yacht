using System;
public class D {
  string msg;
  public D(string msg_) {
    msg = msg_;
  }
  public void show() {
    Console.WriteLine("[class D] " + msg);
  }
}
