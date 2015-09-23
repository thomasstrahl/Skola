package fractal;
import javax.swing.*;
import java.awt.*;

public class CommandPanel extends JPanel {
	public CommandPanel(FractalView view) {
		setLayout(new FlowLayout(FlowLayout.LEFT));
		add(new OrderUpButton(view));
		add(new OrderDownButton(view));
	}
}
