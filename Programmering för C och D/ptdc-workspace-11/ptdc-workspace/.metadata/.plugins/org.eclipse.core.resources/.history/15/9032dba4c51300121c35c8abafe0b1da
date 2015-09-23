package predicate;

import term.Term;
import term.TermList;
import term.Variable;

public class Predicate implements Expr {
	protected String name;
	protected TermList list;
	
	public Predicate(String name, TermList list1){
		this.name = name;
		list = list1;	
	}
	public void addInList(Variable variable){
		list.add(0, variable);
	}
	

	public Predicate substitute(Variable x, Term y) {
		if (list.contains(x)){
			return new Predicate(this.name, new TermList(y));
		}
			return this;
	}
	public String toString(){
		return name + list;
	}
}
