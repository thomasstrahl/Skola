package model;

import expr.Environment;

public class ExprStrategy implements SheetStrategy{
	
	
	@Override
	public double execute(String name, FieldValue cell, Environment env) {
		return cell.value(env);
	}

}
