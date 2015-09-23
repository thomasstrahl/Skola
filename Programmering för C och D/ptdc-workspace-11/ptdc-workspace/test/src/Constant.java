

import java.util.Set;

public class Constant implements Term {
    private String value;

    public Constant(String value) {
        this.value = value;
    }

    public Set<Variable> collectVariables(Set<Variable> set) {
        return set;
    }

    public String toString() {
        return value;
    }

	@Override
	public Term substitute(Variable x, Term term) {
		if (value.compareTo(x.toString())== 0){
			return term;
		} 
		return this;
	}
}