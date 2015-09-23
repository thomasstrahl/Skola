import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import se.lth.control.*;

public class PIGUI {
	private String name;
	private PI pi;
	private PIParameters params;
	private JPanel paramsLabelPanel = new JPanel();
	private JPanel paramsFieldPanel = new JPanel();
	private BoxPanel paramsPanel = new BoxPanel(BoxPanel.HORIZONTAL);
	private DoubleField paramsKField = new DoubleField(6,3);
	private DoubleField paramsTiField = new DoubleField(6,3);
	private DoubleField paramsTrField = new DoubleField(6,3);
	private DoubleField paramsBetaField = new DoubleField(6,3);
	private DoubleField paramsHField = new DoubleField(6,3);
	private JButton paramsButton = new JButton("Apply");

	public PIGUI(PI pCon, PIParameters p, String n) {
		name = n;
		pi = pCon;
		params = p;
		MainFrame.showLoading();
		paramsLabelPanel.setLayout(new GridLayout(0,1));
		paramsLabelPanel.add(new JLabel("K: "));
		paramsLabelPanel.add(new JLabel("Ti: "));
		paramsLabelPanel.add(new JLabel("Tr: "));
		paramsLabelPanel.add(new JLabel("Beta: "));
		paramsLabelPanel.add(new JLabel("h: "));

		paramsFieldPanel.setLayout(new GridLayout(0,1));
		paramsFieldPanel.add(paramsKField); 
		paramsFieldPanel.add(paramsTiField);
		paramsFieldPanel.add(paramsTrField);
		paramsFieldPanel.add(paramsBetaField);
		paramsFieldPanel.add(paramsHField);
		paramsPanel.add(paramsLabelPanel);
		paramsPanel.addGlue();
		paramsPanel.add(paramsFieldPanel);
		paramsPanel.addFixed(10);
		paramsKField.setValue(p.K);
		paramsTiField.setValue(p.Ti);
		paramsTrField.setValue(p.Tr);
		paramsBetaField.setValue(p.Beta);
		paramsHField.setValue(p.H);
		paramsKField.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {

				double tempValue = paramsKField.getValue();
				params.K = tempValue;
				paramsButton.setEnabled(true);
			}
		});
		paramsTiField.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				double tempValue = paramsTiField.getValue();
				if (tempValue < 0.0) {
					paramsTiField.setValue(params.Ti);
				} else {
					params.Ti = tempValue;
					paramsButton.setEnabled(true);
					params.integratorOn = (params.Ti != 0.0);
				}
			}

		});
		paramsTrField.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				double tempValue = paramsTrField.getValue();
				if (tempValue < 0.0) {
					paramsTrField.setValue(params.Tr);
				} else {
					params.Tr = tempValue;
					paramsButton.setEnabled(true);
				}
			}
		});

		paramsBetaField.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				double tempValue = paramsBetaField.getValue();
				if (tempValue < 0.0 || tempValue > 1.0) {
					paramsBetaField.setValue(params.Beta);
				} else {
					params.Beta = tempValue;
					paramsButton.setEnabled(true);
				}
			}
		});

		paramsHField.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				double tempValue = paramsHField.getValue();
				if (tempValue < 0.0) {
					paramsHField.setValue(params.H);
				} else {
					params.H = tempValue;
					paramsButton.setEnabled(true);
				}
			}
		});

		BoxPanel paramButtonPanel = new BoxPanel(BoxPanel.VERTICAL);
		paramButtonPanel.setBorder(BorderFactory.createTitledBorder("Parameters"));
		paramButtonPanel.addFixed(10);
		paramButtonPanel.add(paramsPanel);
		paramButtonPanel.addFixed(10);
		paramButtonPanel.add(paramsButton);

		paramsButton.setEnabled(false);
		paramsButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				pi.setParameters(params);
				paramsButton.setEnabled(false);}
		});
		MainFrame.setPanel(paramButtonPanel,name);
	}



}

