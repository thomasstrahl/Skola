package control;
import gui.CurrentSlot;
import gui.SlotLabel;
import gui.StatusLabel;

import java.awt.event.*;
import java.util.Observable;

import model.Sheet;
public class MouseListenerLabel extends Observable implements MouseListener{
	private CurrentSlot label;
	private StatusLabel status;
	public MouseListenerLabel(SlotLabel l, String s, StatusLabel status, Sheet sheet){
		label = new CurrentSlot(l, s, sheet);
		this.status = status;
	}
	@Override
	public void mouseClicked(MouseEvent arg0) {
		status.setText(" ");
		setChanged();
		notifyObservers(label);
	}

	@Override
	public void mouseEntered(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mouseExited(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mousePressed(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mouseReleased(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}

}
