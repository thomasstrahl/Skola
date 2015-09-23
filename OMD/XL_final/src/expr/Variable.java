package expr;

class Variable extends Expr {
    private String name;

    Variable(String name) {
        this.name = name;
    }

    public String toString(int prec) {
        return name.toString();
    }

    public double value(Environment env) {
        return env.value(name);
    }
}