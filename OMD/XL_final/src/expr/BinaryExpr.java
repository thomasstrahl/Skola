package expr;

abstract class BinaryExpr extends Expr {
    private Expr expr1;
    private Expr expr2;
    protected int precedence1;
    protected int precedence2;

    protected BinaryExpr(Expr expr1, Expr expr2) {
        this.expr1 = expr1;
        this.expr2 = expr2;
    }

    abstract protected double op(double op1, double op2);

    abstract protected String opString();

    public String toString(int prec) {
        StringBuilder builder = new StringBuilder();
        boolean parentheses = prec > precedence1;
        if (parentheses) {
            builder.append('(');
        }
        builder.append(expr1.toString(precedence1));
        builder.append(opString());
        builder.append(expr2.toString(precedence2));
        if (parentheses) {
            builder.append(')');
        }
        return builder.toString();
    }

    public double value(Environment env) {
        return op(expr1.value(env), expr2.value(env));
    }
}