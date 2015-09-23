package gui.menu;

import gui.SlotLabels;
import gui.StatusLabel;
import gui.XL;
import java.io.FileNotFoundException;
import javax.swing.JFileChooser;

import model.Sheet;
import model.XLBufferedReader;
import model.XLException;

@SuppressWarnings("serial")
class LoadMenuItem extends OpenMenuItem {
	private Sheet sheet;
	private StatusLabel status;
	private SlotLabels labels;
	
    public LoadMenuItem(XL xl, StatusLabel statusLabel, Sheet sheet, SlotLabels labels) {
        super(xl, statusLabel, "Load");
        this.sheet = sheet;
        this.status = statusLabel;
        this.labels = labels;
    }

    protected void action(String path) throws FileNotFoundException {
    	XLBufferedReader reader = new XLBufferedReader(path);
    	try {
    		reader.load(sheet, labels);
    	} catch(XLException e) {
        	status.setText(e.getLocalizedMessage());
    	}
    	xl.setTitle(path);
    }

    protected int openDialog(JFileChooser fileChooser) {
        return fileChooser.showOpenDialog(xl);
    }
}