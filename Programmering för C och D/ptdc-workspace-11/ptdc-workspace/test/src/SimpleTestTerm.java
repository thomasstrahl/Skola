






public class SimpleTestTerm {
    protected Constant a = new Constant("a"); // a
    protected Variable x = new Variable("x"); // x
    protected Variable y = new Variable("y"); // y
    protected TermList xList = new TermList(x); // (x)
    protected Function fx = new Function("f", xList); // f(x)
    protected TermList yfxList = new TermList(y, fx); // (y, f(x))
    protected Function gyfx = new Function("g", yfxList); // g(y, f(x))

    protected void printBoth(Object expected, Object actual) {
        System.out.print(expected);
        System.out.print("\t\t");
        System.out.println(actual);
    }

    public void testAll() {
        printBoth(a, a.substitute(x, y)); // a[x\y] = a
        printBoth(y, x.substitute(x, y)); // x[x\y] = y
        printBoth(a, y.substitute(y, a)); // y[y\a] = a
        printBoth(y, y.substitute(x, y)); // y[x\y] = y
        printBoth(new TermList(y), xList.substitute(x, y)); // (x)[x\y] = (y)
        Term expected = new Function("f", new TermList(fx));
        printBoth(expected, fx.substitute(x, fx)); // f(x)[x\f(x)] = f(f(x))
        expected = new Function("g", new TermList(y, new Function("f",
                new TermList(fx))));
        printBoth(expected, gyfx.substitute(x, fx)); // g(y, f(x))[x\f(x)] =
                                                     // g(y, f(f(x)))
    }
    
    public static void main(String[] args) {
        new SimpleTestTerm().testAll();
    }
  
}