package fractal;
import javax.swing.*;
import java.awt.*;

public class DrawingPanel extends JPanel {
	FractalView view;

	public DrawingPanel(FractalView view) {
		this.view = view;
		setBorder(BorderFactory.createLineBorder(Color.black));
	}

	public Dimension getPreferredSize() {
		return new Dimension(600,600);
	}

	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		Fractal fractal = view.getFractal();
		g.drawString(fractal.toString(), 10, 20);
		g.setClip(0, 0, (int) getSize().getWidth(), (int) getSize().getHeight());
		fractal.draw(new TurtleGraphics(g));
	}  
}
