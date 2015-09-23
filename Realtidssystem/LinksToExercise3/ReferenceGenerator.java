import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import se.lth.control.*;

public class ReferenceGenerator extends Thread {
	private double amplitude;
	private int period;
	private double sign = -1.0;
	private double ref;
	
	private class RefGUI {
		private JPanel paramsLabelPanel = new JPanel();
		private JPanel paramsFieldPanel = new JPanel();
		private BoxPanel paramsPanel = new BoxPanel(BoxPanel.HORIZONTAL);
		private JTextField paramsAmpField = new JTextField();
		private JTextField paramsPeriodField = new JTextField();
		
		public RefGUI(double amp, double h) {
			MainFrame.showLoading();
			paramsLabelPanel.setLayout(new GridLayout(0,1));
			paramsLabelPanel.add(new JLabel("Amp: "));
			paramsLabelPanel.add(new JLabel("Period: "));
			
			paramsFieldPanel.setLayout(new GridLayout(0,1));
			paramsFieldPanel.add(paramsAmpField); 
			paramsFieldPanel.add(paramsPeriodField);   
			paramsPanel.add(paramsLabelPanel);
			paramsPanel.addGlue();
			paramsPanel.add(paramsFieldPanel);
			paramsPanel.addFixed(10);
			Double d = new Double(amp);
			paramsAmpField.setText(d.toString());
			d = new Double(h);
			paramsPeriodField.setText(d.toString());
			paramsAmpField.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					String tempString = paramsAmpField.getText();
					boolean ok = true;
					Double doubleObject = new Double(0.0);
					try {
						doubleObject = new Double(tempString);
					} catch (Exception x) {
						ok = false;
					}
					double tempValue;
					if (ok) {
						tempValue = doubleObject.doubleValue();
					} else {
						tempValue = -1.0;
					}
					if (tempValue < 0.0) {
						Double d1 = new Double(amplitude);
						paramsAmpField.setText(d1.toString());
					} else {
						amplitude = tempValue;
					}
				}
			});
			paramsPeriodField.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					String tempString = paramsPeriodField.getText();
					boolean ok = true;
					Double doubleObject = new Double(0.0);
					try {
						doubleObject = new Double(tempString);
					} catch (Exception x) {
						ok = false;
					}
					double tempValue;
					if (ok) {
						tempValue = doubleObject.doubleValue();
					} else {
						tempValue = -1.0;
					}
					if (tempValue < 0.0) {
						Double d1 = new Double(period);
						paramsPeriodField.setText(d1.toString());
					} else {
						period = (int)(tempValue*1000/2);
					}
				}
			});  
			MainFrame.setPanel(paramsPanel,"RefGen");
		}
	}
	
	public ReferenceGenerator(double h, double a) {
		amplitude = a;
		period = (int)(h*1000/2);
		new RefGUI(a,h);
	}
	
	public synchronized double getRef() 
	{
		return ref;
	}
	
	public void run() {
		try {
			while (!isInterrupted()) {
				synchronized (this) {
					sign = - sign;
					ref = amplitude * sign;
				}
				sleep(period);
			}
		} catch (InterruptedException e) {
			// Requested to stop
		}
	}
}
