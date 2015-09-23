package gui.menu;

import gui.StatusLabel;
import gui.XL;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileNotFoundException;
import javax.swing.JFileChooser;
import javax.swing.JMenuItem;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileNameExtensionFilter;

public abstract class OpenMenuItem extends JMenuItem implements ActionListener {
    protected StatusLabel statusLabel;
    protected XL xl;

    protected OpenMenuItem(XL xl, StatusLabel statusLabel, String title) {
        super(title);
        this.xl = xl;
        this.statusLabel = statusLabel;
        addActionListener(this);
    }

    protected abstract void action(String path) throws FileNotFoundException;

    public void actionPerformed(ActionEvent event) {
        JFileChooser fileChooser = new JFileChooser(".");
        FileFilter filter = new FileNameExtensionFilter("XL files", "xl");
        fileChooser.setFileFilter(filter);
        int option = openDialog(fileChooser);
        if (option == JFileChooser.APPROVE_OPTION) {
            File file = fileChooser.getSelectedFile();
            try {
                action(file.toString());
                xl.rename(file.getName());
            } catch (FileNotFoundException e) {
                statusLabel.setText(e.getMessage());
            }
        }
    }

    protected abstract int openDialog(JFileChooser fileChooser);
}