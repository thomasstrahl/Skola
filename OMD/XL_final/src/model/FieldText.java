package model;

import expr.Environment;

public class FieldText extends FieldValue {

	public FieldText(String value){
		super(value);
	}
	@Override
	public String str() {
		System.out.println(value);
		return "#"+value;
	}
	@Override
	public String valueString(Environment env) {
		return value;
	}
	@Override
	public double value(Environment env) {
		return 0;
	}


}
