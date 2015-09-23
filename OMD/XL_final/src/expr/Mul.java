package expr;

class Mul extends BinaryExpr {
    Mul(Expr expr1, Expr expr2) {
        super(expr1, expr2);
        precedence1 = 1;
        precedence2 = 1;
    }

    public double op(double op1, double op2) {
        return op1 * op2;
    }

    protected String opString() {
        return "*";
    }
}