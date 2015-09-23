package predicate;

import term.SimpleTestTerm;
import term.TermList;
import term.Variable;

public class SimpleTestPredicate extends SimpleTestTerm {
    private Variable z = new Variable("z"); // z
    private Variable v0 = new Variable(); // v0
    private Predicate px = new Predicate("P", xList); // P(x)
    private Predicate py = new Predicate("P", new TermList(y)); // P(y)
    private Predicate pz = new Predicate("P", new TermList(z)); // P(z)
    private Predicate qxy = new Predicate("Q", new TermList(x, y)); // Q(x, y)
    private Implies pxImpliesPy = new Implies(px, py); // P(x) -> P(y)
    private ForAll forAllxPx = new ForAll(x, px); // FORALL x . P(x)
    private ForAll forAllxPy = new ForAll(x, py); // FORALL x . P(y)
    private ForAll forAllxPz = new ForAll(x, pz); // FORALL x . P(z)
    private ForAll forAllxQxy = new ForAll(x, qxy); // FORALL x . Q(x, y)

    public void testAllExpr() {
        // P(x)[x\y] = P(y)
        printBoth(py, px.substitute(x, y));
        // (P(x) -> P(y))[x\y] = P(y) -> P(y)
        Expr expected = new Implies(py, py);
        printBoth(expected, pxImpliesPy.substitute(x, y));
        // (FORALL x . P(x))[x\y] = FORALL x . P(x)
        printBoth(forAllxPx, forAllxPx.substitute(x, y));
        // (FORALL x . P(y))[y\z] = FORALL x . P(z)
        printBoth(forAllxPz, forAllxPy.substitute(y, z));
        // (FORALL x . Q(x,y)[y\f(x)] = FORALL v0 . Q(v0, f(x))
        Variable.resetCounter();
        
        expected = new ForAll(v0, new Predicate("Q", new TermList(v0, fx)));
        printBoth(expected, forAllxQxy.substitute(y, fx));
    }


    public static void main(String[] args) {
        new SimpleTestPredicate().testAllExpr();
    }
}
