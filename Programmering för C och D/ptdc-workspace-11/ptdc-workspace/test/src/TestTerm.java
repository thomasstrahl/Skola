package term;

import junit.framework.TestCase;

public class TestTerm extends TestCase {
    protected Constant a = new Constant("a"); // a
    protected Variable x = new Variable("x"); // x
    protected Variable y = new Variable("y"); // y
    protected TermList xList = new TermList(x); // (x)
    protected Function fx = new Function("f", xList); // f(x)
    protected TermList yfxList = new TermList(y, fx); // (y, f(x))
    protected Function gyfx = new Function("g", yfxList); // g(y, f(x))

    protected void assertEqualsByToString(Object expected, Object actual) {
        assertEquals(expected.toString(), actual.toString());
    }

    public void test1() { // a[x\y] = a
        assertEqualsByToString(a, a.substitute(x, y));
    }

    public void test2() { // x[x\y] = y
        assertEqualsByToString(y, x.substitute(x, y));
    }

    public void test3() { // y[y\a] = a
        assertEqualsByToString(a, y.substitute(y, a));
    }

    public void test4() { // y[x\y] = y
        assertEqualsByToString(y, y.substitute(x, y));
    }

    public void test5() { // (x)[x\y] = (y)
        assertEqualsByToString(new TermList(y), xList.substitute(x, y));
    }

    public void test6() { // f(x)[x\f(x)] = f(f(x))
        Term expected = new Function("f", new TermList(fx));
        assertEqualsByToString(expected, fx.substitute(x, fx));
    }

    public void test7() { // g(y, f(x))[x\f(x)] = g(y, f(f(x)))
        Term expected = new Function("g", new TermList(y, new Function("f",
                new TermList(fx))));
        assertEqualsByToString(expected, gyfx.substitute(x, fx));
    }
}
