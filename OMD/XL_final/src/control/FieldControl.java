package control;

import gui.CurrentSlot;
import gui.Editor;
import gui.SlotLabel;
import gui.StatusLabel;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

import model.FieldValue;
import model.Sheet;
import model.XLException;

public class FieldControl implements ActionListener {
	private Sheet model;
	private CurrentSlot view;
	private StatusLabel status;
	
	public FieldControl(CurrentSlot view, Sheet model, StatusLabel status){
		this.model = model;
		this.view = view;
		this.status = status;
	}
	public void addModel(Sheet model){
		this.model = model;
	}
	
	public void addView(CurrentSlot view){
		this.view = view;
	}
	
	@Override
	public void actionPerformed(ActionEvent e) {
		String field = view.toString();
		try {
			view.updateField(true);
			model.updateField(field, view.getLabel().getText());
		} catch (IOException e1) {
			status.setText(e1.getLocalizedMessage());
			
		} catch (XLException e2){
			status.setText(e2.getLocalizedMessage());
		
		}
	}
}
