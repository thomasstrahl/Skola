package model;

import expr.Environment;

public abstract class FieldValue{
	protected String value;
	
	public FieldValue(String value) {
		this.value = value;
	}
	
	public abstract String str();
	public abstract String valueString(Environment env);
	public abstract double value(Environment env);
	
	public void changeValue(String value){
		this.value = value;
	}
	public String toString(){
		return str();
	}
}
