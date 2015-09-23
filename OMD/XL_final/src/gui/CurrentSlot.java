package gui;

import java.awt.Color;
import java.util.Observable;
import java.util.Observer;

import model.Sheet;

public class CurrentSlot implements Observer{
	private String s;
	private SlotLabel label;
	private Editor edit;
	private Sheet sheet;
	
	public CurrentSlot(SlotLabel l, String s, Sheet sheet){
		label = l;
		this.s = s;
		this.sheet = sheet;
	}
	public CurrentSlot(){
		s ="";
		label = new SlotLabel();
	}
	@Override
	public void update(Observable arg0, Object arg1) {
		s = arg1.toString();
		label.setBackground(Color.WHITE);
		label = ((CurrentSlot) arg1).getLabel();
		label.setBackground(Color.YELLOW);
		String temptext = label.getText();
		if(temptext.charAt(0) == ' '){
			edit.setText("");
		}
		else{
			edit.setText(sheet.getText(s));
		}
	}
	public String toString(){
		return s;
	}
	public SlotLabel getLabel(){
		return label;
	}
	public void updateField(boolean success){
		if(success){
			label.setText(edit.getText());			
		}else{
			label.setText("    ");}
	}
	public void addEditor(Editor e){
		edit = e;
	}
	public void addSheet(Sheet sheet){
		this.sheet = sheet;
	}

}
