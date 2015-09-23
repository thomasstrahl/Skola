package predicate;

import term.TermList;
import term.TestTerm;
import term.Variable;

public class TestPredicate extends TestTerm {
    private Variable z = new Variable("z"); // z
    private Predicate px = new Predicate("P", xList); // P(x)
    private Predicate py = new Predicate("P", new TermList(y)); // P(y)
    private Predicate pz = new Predicate("P", new TermList(z)); // P(z)
    private Predicate qxy = new Predicate("Q", new TermList(x, y)); // Q(x, y)
    private Implies pxImpliesPy = new Implies(px, py); // P(x) -> P(y)
    private ForAll forAllxPx = new ForAll(x, px); // FORALL x . P(x)
    private ForAll forAllxPy = new ForAll(x, py); // FORALL x . P(y)
    private ForAll forAllxPz = new ForAll(x, pz); // FORALL x . P(z)
    private ForAll forAllxQxy = new ForAll(x, qxy); // FORALL x . Q(x, y)

    public void testPredicate() { // P(x)[x\y] = P(y)
        assertEqualsByToString(py, px.substitute(x, y));
    }

    public void testImplies() { // (P(x) -> P(y)[x\y] = P(y) -> P(y)
        Expr pyImpliesPy = new Implies(py, py);
        assertEqualsByToString(pyImpliesPy, pxImpliesPy.substitute(x, y));
    }

    public void testForAll1() { // (FORALL x . P(x))[x\y] = FORALL x . P(x)
        assertEqualsByToString(forAllxPx, forAllxPx.substitute(x, y));
    }

    public void testForAll2() { // (FORALL x . P(x))[x\y] = FORALL x . P(x)
        assertEqualsByToString(forAllxPz, forAllxPy.substitute(y, z));
    }

    public void testForAll3() { // (FORALL x . Q(x,y))[y\f(x)] = FORALL v0 . Q(v0,f(x)) 
        Variable v0 = new Variable();
        Variable.resetCounter();
        Expr expected = new ForAll(v0, new Predicate("Q", new TermList(v0, fx)));
        assertEqualsByToString(expected, forAllxQxy.substitute(y, fx));
    }
}
