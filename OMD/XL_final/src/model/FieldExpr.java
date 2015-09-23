package model;

import java.io.IOException;

import expr.*;

public class FieldExpr extends FieldValue {
	private Expr expr;
	
	public FieldExpr(String str) throws IOException{
		super(str);
		expr = new ExprParser().build(value);
	}
	@Override
	public String str() {
		return expr.toString();
	}
	@Override
	public String valueString(Environment env) {
		Double d = expr.value(env);
		return d.toString();
	}
	@Override
	public double value(Environment env) {
		return expr.value(env);
	}

}
