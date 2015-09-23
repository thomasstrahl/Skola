package gui;

import java.awt.Color;
import javax.swing.JTextField;

import control.FieldControl;

public class Editor extends JTextField {
    public Editor(FieldControl fc) {
        setBackground(Color.WHITE);
        addActionListener(fc);
    }
}