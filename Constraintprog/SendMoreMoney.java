/**
 *  SendMoreMoney.java 
 *  This file is part of JaCoP.
 *
 *  JaCoP is a Java Constraint Programming solver. 
 *	
 *	Copyright (C) 2000-2008 Krzysztof Kuchcinski and Radoslaw Szymanek
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *  
 *  Notwithstanding any other provision of this License, the copyright
 *  owners of this work supplement the terms of this License with terms
 *  prohibiting misrepresentation of the origin of this work and requiring
 *  that modified versions of this work be marked in reasonable ways as
 *  different from the original version. This supplement of the license
 *  terms is in accordance with Section 7 of GNU Affero General Public
 *  License version 3.
 *
 *  You should have received a copy of the GNU Affero General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */


import java.util.ArrayList;

import org.jacop.constraints.Alldiff;
import org.jacop.constraints.SumWeight;
import org.jacop.constraints.XmulCeqZ;
import org.jacop.constraints.XneqC;
import org.jacop.constraints.XneqY;
import org.jacop.constraints.XplusYeqZ;
import org.jacop.core.IntVar;
import org.jacop.core.Store;
import org.jacop.search.DepthFirstSearch;
import org.jacop.search.IndomainMin;
import org.jacop.search.SelectChoicePoint;
import org.jacop.search.SimpleSelect;
import org.jacop.search.SmallestDomain;

/**
 * 
 * It is a simple arithmetic logic puzzle, where SEND+MORE=MONEY.
 * 
 *  Find for the equation on the left
 *   what digits are represented by the letters
 *    different letters represent different digits

 *  SEND           9567
 * +MORE =======> +1085
 * MONEY          10652
 * 
 * @author Krzysztof Kuchcinski
 * @version 4.1
 *
 */
public class SendMoreMoney  {

    Store store = new Store();

    /**
     * It executes the program to solve this simple logic puzzle.
     * @param args no arguments used.
     */
	
    public static void main(String args[]) {

	SendMoreMoney exampleGlobal = new SendMoreMoney();
		
	exampleGlobal.model();		

    }
	
	
    /**
     * 1. Every CP program consists of two parts. The first one is a model and
     * the second one is the specification of the search. 
     * This creates a model which uses global constraints to provide consize modeling. 
     * The model consists of variables and constraints. 
     */

    public void model() {
		
	ArrayList<IntVar> vars = new ArrayList<IntVar>();

	/**
	 * A constraint store can be considered as a manager of constraints 
	 * and variables. It is always required to have a constraint store 
	 * in order to create a CP model of the problem. 
	 */
	store = new Store();

	/**
	 * First, we need to specify the variables. In this problem we 
	 * will for certain have variables representing the value of different
	 * letters.
	 */

	/**
	 * We create variables. Each variable is created in a given 
	 * constraint store. We provide information about variable name. 
	 * However, the most part part of the specification is about the initial
	 * domain. The initial domain is specified to be in between 0 and 9. 
	 * 
	 * One important feature of CP is that a variable must take a value from
	 * the initial domain. The actual value of the variable can not be outside
	 * the initial domain. In some sense, the initial domain is also like a
	 * constraint.
	 * 
	 *  A variable is an integer variable, so eventually it will have an integer
	 *  value.
	 *  
	 *  FDV - Finite Domain Variable.
	 */

	IntVar s = new IntVar(store, "S", 0, 9);
	IntVar e = new IntVar(store, "E", 0, 9);
	IntVar n = new IntVar(store, "N", 0, 9);
	IntVar d = new IntVar(store, "D", 0, 9);
	IntVar m = new IntVar(store, "M", 0, 9);
	IntVar o = new IntVar(store, "O", 0, 9);
	IntVar r = new IntVar(store, "R", 0, 9);
	IntVar y = new IntVar(store, "Y", 0, 9);

	// Creating arrays for FDVs
	IntVar digits[] = { s, e, n, d, m, o, r, y };
	IntVar send[] = { s, e, n, d };
	IntVar more[] = { m, o, r, e };
	IntVar money[] = { m, o, n, e, y };

	for (IntVar v : digits)
	    vars.add(v);
		
	/**
	 * After specifying the variables we need to specify the 
	 * constraints. 
	 * 
	 * All the constraints which are specified have to be satisfied
	 * by the solution. 
	 * 
	 * Constraints are added to the model through executing the impose function 
	 * of the constraint store.
	 *
	 * Instead of using 28 primitive constraints of the form XneqY
	 * we can use only one global constraint. 
	 * 
	 * A global constraint may have number of advantages when compared
	 * to primitive (basic) constraints : 
	 * 
	 * a). It give more concise model. A relatively minor improvement. 
	 * 
	 * b) the global constraint is aware of the relation not only between
	 * two variables, but actually between all pairs within a set of variables.
	 * A global has a potential of reducing an exponential size of the search tree
	 * to a constant small size search tree. 
	 * 
	 *  Example. If three variables had a domain 1..2, then primitive constraints
	 *  will not notice any problem, because any pair of variables can be assigned
	 *  to value 1 or 2 and the variables will be different. However, the global
	 *  constraint can notice that there are only two different values in the domains
	 *  but three variables, so the constraint is not possible to satisfy.
	 *  
	 *  A (primitive) constraint can not assign a value to a variable, unless it has
	 *  to be equal to that value. In our example above the primitive constraint can
	 *  not make the first variable equal to one.
	 */
	store.impose(new Alldiff(digits));

	/**
	 * We would like to express the relation that SEND + MORE = MONEY. We have value
	 * for particular letters but not for the whole words. Now, the task is to figure 
	 * out the value of the word given values of the letters.
	 * 
	 * This can be easily achieved using SumWeight constraint. A SumWeight constraint
	 * will take a list of variable and a list of weights and multiple each variable by
	 * a corresponding weight and make the result equal to the last (third parameter) of
	 * the constraint.
	 */
		
	int[] weights5 = { 10000, 1000, 100, 10, 1 };
	int[] weights4 = { 1000, 100, 10, 1 };

	/**
	 * We create auxilary variables in order to make it easier to express some of the
	 * constraints. Each auxilary variable holds a value for a given word.
	 */
	IntVar valueSEND = new IntVar(store, "v(SEND)", 0, 9999);
	IntVar valueMORE = new IntVar(store, "v(MORE)", 0, 9999);
	IntVar valueMONEY = new IntVar(store, "v(MONEY)", 0, 99999);

	/**
	 * Constraints for getting value for words
	 * SEND = 1000 * S + 100 * E + N * 10 + D * 1
	 * MORE = 1000 * M + 100 * O + R * 10 + E * 1
	 * MONEY = 10000 * M + 1000 * O + 100 * N + E * 10 + Y * 1
	 */
	store.impose(new SumWeight(send, weights4, valueSEND));
	store.impose(new SumWeight(more, weights4, valueMORE));
	store.impose(new SumWeight(money, weights5, valueMONEY));

	/**
	 * The auxilary variables allow us to express the main constraint
	 * of the problem in very simple manner. We just use XplusYeqZ constraint.
	 */
	store.impose(new XplusYeqZ(valueSEND, valueMORE, valueMONEY));

	/**
	 * Implied constraint after transformation of SEND+MORE=MONEY.
	 * 1000 * S + 91 * E + 10 * R + D -9000 * M - 900 * O -90 * N = Y.
	 * It removes 2 wrong decisions.
	 */
		
	int [] weightsImplied = {1000, 91, 10, 1, -9000, -900, -90}; 
	IntVar [] varsImplied = {s, e, r, d, m, o, n};
	store.impose(new SumWeight(varsImplied, weightsImplied, y));
		
	/**
	 * The two constraints below were not explicit in the problem description. However, 
	 * they are still valid constraints. The programmer task is to find and make the 
	 * constraint explicit in the constraint model.
	 *
	 * Since S is the first digit of SEND and M is the first digit of MORE or MONEY
	 * both letters can not be equal to zero
	 */
	store.impose(new XneqC(s, 0));
	store.impose(new XneqC(m, 0));		
		
	/**
	 * We have very concise model of the problem. It contains only 28 lines of code. 
	 * It is thanks to the constraints which incorporate reasoning mechanisms which 
	 * are used as soon as some decisions about variables domains are taken. 
	 * 
	 * In addition, the constraints can specify quite complex relationships between variables, 
	 * like an alldiff constraint. 
	 */
		

	SimpleDFS search = new SimpleDFS(store);
	search.setVariablesToReport(digits);

	boolean result = search.label(digits);

	System.out.println (result);

    }

}
