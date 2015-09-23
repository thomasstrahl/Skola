package model;

import expr.Environment;

public class ErrorStrategy implements SheetStrategy{
	private String origin;
	
	public ErrorStrategy(String origin){
		this.origin = origin;
	}

	@Override
	public double execute(String name, FieldValue cell, Environment env) {
		if(name.compareTo(origin) == 0){
			throw new XLException("Cyclic dependancy");
		}
		return cell.value(env);
	}

}
