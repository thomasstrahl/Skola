package fractal;

import javax.swing.*;

import java.awt.event.*;

public class OrderDownButton extends JButton implements ActionListener{
	private FractalView view;
	
	
	public OrderDownButton(FractalView view) {
		super("<");
		this.view = view;
		addActionListener(this);
		this.setToolTipText("Minskar fraktalens ordning.");
	}

	public void actionPerformed(ActionEvent e) {
		Fractal fractal = view.getFractal();
		if(fractal.getOrder()!=0){
		
		fractal.setOrder(fractal.getOrder() - 1);
		view.updateDrawing();
	}
	}	
}
