package fractal;
import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class FractalView {
	private CommandPanel commandPanel;
	private DrawingPanel drawingPanel;
	private Fractal actFractal;

	public FractalView(Fractal[] fractals, String title) {
		JFrame frame = new JFrame(title);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		commandPanel = new CommandPanel(this);
		drawingPanel = new DrawingPanel(this);

		frame.add(commandPanel, BorderLayout.SOUTH);
		frame.add(drawingPanel, BorderLayout.CENTER);

		actFractal = fractals[0];

		JMenu fractalMenu = new JMenu("Fraktaler");
		JMenuItem[] menuItems = new JMenuItem[fractals.length];
		for (int i = 0; i < fractals.length; i++) {
			menuItems[i]  = new JMenuItem(fractals[i].getTitle());
			menuItems[i].addActionListener(new MenuListener(fractals[i]));
			fractalMenu.add(menuItems[i]);
		}
		JMenuBar menuBar = new JMenuBar();
		menuBar.add(fractalMenu);
		frame.setJMenuBar(menuBar);

		frame.pack();
		frame.setVisible(true);	
	}

	public Fractal getFractal() {
		return actFractal;
	}

	public void setFractal(Fractal fractal) {
		actFractal = fractal;
	}

	public void updateDrawing() {
		drawingPanel.repaint();
	}

	class MenuListener implements ActionListener {
		private Fractal fractal;

		public MenuListener(Fractal fractal) {
			this.fractal = fractal;
		}

		public void actionPerformed(ActionEvent e) {
			actFractal = fractal;
			updateDrawing();
		}
	}

}
