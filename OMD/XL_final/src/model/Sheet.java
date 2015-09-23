package model;

import expr.Environment;

import gui.SlotLabels;


import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import java.util.Observable;


public class Sheet extends Observable implements Environment{

	private HashMap<String, FieldValue> map;
	private SlotLabels labels;
	private SheetStrategy strategy;
	private boolean load;
	
	public Sheet(SlotLabels labels){
		map = new HashMap<String, FieldValue>();
		this.labels = labels;
		this.load = false;
		addObserver(labels);
	}
	
	public void setStrategy(SheetStrategy strategy){
		this.strategy = strategy;
	}
	
	public void setLoad(boolean load){
		this.load = load;
	}
	
	
	public void Save(String path){
		try{
			XLPrintStream saver = new XLPrintStream(path);
			saver.save(map.entrySet());
		}
		catch(FileNotFoundException e){
			System.out.println("You broke physics");
		}
	}
	
	public FieldValue getField(String name){
		return map.get(name);
	}

	public void updateField(String field, String value) throws IOException, XLException {
		if(value.isEmpty()){
			map.remove(field);
		}else if(value.charAt(0) == '#'){
			FieldValue fv = new FieldText(value.substring(1));
			map.put(field, fv);
		}else{
			FieldValue Oldfv = map.get(field);
			map.put(field, new FieldExpr(value));
			if (!load) {
				try {
					checkErrors();
				} catch (XLException e) {
					if (Oldfv != null) {
						map.put(field, Oldfv);
					} else {
						map.remove(field);
					}
					throw e;
				}
				
			}
			setStrategy(new ExprStrategy());
			setChanged();
			notifyObservers(map);
		}
		

	}
	
	public void clearAll(){
		map = new HashMap<String, FieldValue>();
		labels.clearAll();
		setChanged();
		notifyObservers(map);
	}
	public void clear(){
		map.remove(labels.clearCurrent());
	}
	
	public void checkErrors() throws IOException, XLException {
		for(String name : map.keySet()){
			checkError(name);
		}
	}
	public void checkError(String name){
		setStrategy(new ErrorStrategy(name));
		map.get(name).value(this);
		setStrategy(new ExprStrategy());
	}
	
	@Override
	public double value(String name) {
		if(map.containsKey(name)){
			return strategy.execute(name, map.get(name), this);
		}else{
			throw new XLException("Empty field");
		}
	}
	public String getText(String name) {
		System.out.println(name);
		if(map.containsKey(name)){
			return map.get(name).toString();			
		}
		return "";
	}
	public void merge(Sheet sheet){
		this.map.putAll(sheet.map);
		setChanged();
		notifyObservers(map);
	}

	public String fieldValue(String name) {
		if (map.containsKey(name)) {
			return map.get(name).valueString(this);
		}
		return "";
	}

	

}
