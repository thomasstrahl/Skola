package predicate;

import term.Term;
import term.Variable;

public class Implies implements Expr{
	private Expr p1, p2;
	
	public Implies(Expr p1, Expr p2){
		this.p1 = p1;
		this.p2 = p2;
	}

	@Override
	public Implies substitute(Variable variable, Term term) {
		return new Implies(p1.substitute(variable, term), p2.substitute(variable, term));
	}
	
	public String toString(){
		return p1.toString() + " -> " + p2.toString();
	}
	
	
}
