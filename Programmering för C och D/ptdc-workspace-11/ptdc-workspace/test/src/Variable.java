

import java.util.Set;
import java.util.regex.Pattern;

public class Variable implements Term {
    private static int counter;
    private String name;

    public Variable() {
        this.name = "v" + counter;
        counter++;
    }

    public Variable(String name) {
        this.name = name;
        if (Pattern.matches("v\\d+", name)) {
            throw new RuntimeException("Variable: the name '" + name
                    + "' is not allowed");
        }
    }

    public static void resetCounter() {
        counter = 0;
    }

    public Set<Variable> collectVariables(Set<Variable> set) {
        set.add(this);
        return set;
    }

    public boolean equals(Object object) {
        if (object instanceof Variable) {
            Variable other = (Variable) object;
            return name.equals(other.name);
        } else {
            return false;
        }
    }

    public int hashCode() {
        return name.hashCode();
    }

    public String toString() {
        return name;
    }

	@Override
	public Term substitute(Variable x, Term term) {
		if (name.compareTo(x.toString()) == 0){
			return term;
		}
		return this;
	}
}
