package term;

import java.util.Set;

public class Function implements Term {
    private String name;
    private TermList list;

    public Function(String name, TermList list) {
        this.name = name;
        this.list = list;
    }

    public Set<Variable> collectVariables(Set<Variable> set) {
        return list.collectVariables(set);
    }

    public String toString() {
        return name + list;
    }

	@Override
	public Term substitute(Variable x, Term term) {
		
	if(name.compareTo(x.toString()) == 0){
		return term;
	} else if (list.contains(term)){
//		Function f1 = new Function(term.)
		list.add((term.substitute(x, term)));
		list.remove(term);
		return new Function(this.name, list);
	} else 
		return new Function(this.name, new TermList(term));
	}
}
