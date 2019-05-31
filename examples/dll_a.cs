public class A {
  public static void Main() {
    B.hi();
    var b = new B(1, 2);
    b.show();
    (new D("another dll")).show();
  }
}
