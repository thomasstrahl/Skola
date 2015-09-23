package gui.menu;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JMenuItem;

import model.Sheet;

class ClearMenuItem extends JMenuItem implements ActionListener {
	private Sheet sheet;
    public ClearMenuItem(Sheet sheet) {
        super("Clear");
        this.sheet = sheet;
        addActionListener(this);
    }

    public void actionPerformed(ActionEvent e) {
        sheet.clear();
    }
}