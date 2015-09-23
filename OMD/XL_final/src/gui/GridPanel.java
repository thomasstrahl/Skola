package gui;

import java.awt.Color;
import java.awt.GridLayout;
import javax.swing.JPanel;

public class GridPanel extends JPanel {
    public GridPanel(int rows, int columns) {
        super(new GridLayout(rows, columns, 2, 2));
        setBackground(Color.BLACK);
    }
}