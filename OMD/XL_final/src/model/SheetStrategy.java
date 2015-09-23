package model;

import expr.Environment;

public interface SheetStrategy {
	public double execute(String name, FieldValue cell, Environment env);
}
