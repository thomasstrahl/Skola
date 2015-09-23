package gui;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Observable;
import java.util.Observer;

import javax.swing.SwingConstants;

import model.FieldValue;
import model.Sheet;

import control.MouseListenerLabel;

@SuppressWarnings("serial")
public class SlotLabels extends GridPanel implements Observer{
    private List<SlotLabel> labelList;
    private CurrentSlot current;
    private Sheet sheet;
    private StatusLabel status;
    
    public SlotLabels(int rows, int cols, CurrentLabel sl, CurrentSlot cs, StatusLabel status) {
    	super(rows + 1, cols);
    	this.current = cs;
    	this.status = status;
    	labelList = new ArrayList<SlotLabel>(rows * cols);
        for (char ch = 'A'; ch < 'A' + cols; ch++) {
            add(new ColoredLabel(Character.toString(ch), Color.LIGHT_GRAY,
                    SwingConstants.CENTER));
        }
        for (int row = 1; row <= rows; row++) {
            for (char ch = 'A'; ch < 'A' + cols; ch++) {
                SlotLabel label = new SlotLabel();
                add(label);
                labelList.add(label);
                MouseListenerLabel tempobserver = new MouseListenerLabel(label, ""+ch+row, status, sheet);
                label.addMouseListener(tempobserver);
                tempobserver.addObserver(current);
                tempobserver.addObserver(sl);
            }
        }
    }
    public void addSheet(Sheet sheet){
    	this.sheet = sheet;
    	sheet.addObserver(this);
    }
	@Override
	public void update(Observable arg0, Object arg1) {
		Map<String, FieldValue> map = (Map<String, FieldValue>) arg1;
		for(Entry<String, FieldValue> e : map.entrySet()){
			String key = e.getKey();
			int index = (key.charAt(0)-'A') + ((Integer.valueOf(key.substring(1))-1)*8);
			labelList.get(index).setText(sheet.fieldValue(key));
		}
	}

	public void clearAll(){
		for(SlotLabel s : labelList){
			s.setText("                  ");
		}
	}
	public String clearCurrent(){
		SlotLabel label = current.getLabel();
		label.setText("           ");		
		return current.toString();
	}
}
