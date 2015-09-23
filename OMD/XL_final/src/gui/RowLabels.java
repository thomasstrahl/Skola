package gui;

import static java.awt.Color.LIGHT_GRAY;
import static javax.swing.SwingConstants.RIGHT;

class RowLabels extends GridPanel {
    RowLabels(int rows) {
        super(rows + 1, 1);
        add(new ColoredLabel("", LIGHT_GRAY, RIGHT));
        for (int i = 1; i <= rows; i++) {
            add(new ColoredLabel(String.valueOf(i), LIGHT_GRAY, RIGHT));
        }
    }
}