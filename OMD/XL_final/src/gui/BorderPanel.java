package gui;

import java.awt.BorderLayout;
import java.awt.Color;
import javax.swing.JPanel;

public class BorderPanel extends JPanel {
    protected BorderPanel() {
        super(new BorderLayout(2, 2));
        setBackground(Color.BLACK);
    }
}
