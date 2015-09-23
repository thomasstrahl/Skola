package expr;

class Add extends BinaryExpr {
    Add(Expr expr1, Expr expr2) {
        super(expr1, expr2);
        precedence1 = 0;
        precedence2 = 0;
    }

    public double op(double op1, double op2) {
        return op1 + op2;
    }

    protected String opString() {
        return "+";
    }
}