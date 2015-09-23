package predicate;

import java.util.HashSet;
import term.Term;
import term.Variable;

public class ForAll implements Expr {
	private Variable variable;
	private Predicate p1;
	
	public ForAll(Variable variable1, Predicate p1){
		this.variable = variable1;
		this.p1 = p1;
	}

	@Override
	public ForAll substitute(Variable variable1, Term term) {
		if(variable.equals(variable1)){
			return this;
			//fall 3
		} else if (term.collectVariables(new HashSet<Variable>()).contains(variable)){
			Variable variable2 = new Variable();
			Predicate e1 = p1.substitute(variable1, term);
			System.out.println(e1.toString());
			e1.addInList(variable2);
			return new ForAll(variable2, e1);
			//fall 2
		} else {
			return new ForAll(variable, p1.substitute(variable1, term));
		}
	}
	
	public String toString(){
		return variable.toString() + p1.toString();
		
	}
}

