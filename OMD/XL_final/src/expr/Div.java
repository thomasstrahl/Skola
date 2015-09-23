package expr;

import model.XLException;

class Div extends BinaryExpr {
    Div(Expr expr1, Expr expr2) {
        super(expr1, expr2);
        precedence1 = 1;
        precedence2 = 2;
    }

    public double op(double op1, double op2) {
        if (op2 != 0)
            return op1 / op2;
        else
            throw new XLException("division by zero");
    }

    protected String opString() {
        return "/";
    }
}