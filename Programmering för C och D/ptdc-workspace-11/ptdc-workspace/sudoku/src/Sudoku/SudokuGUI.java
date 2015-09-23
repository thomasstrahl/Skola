package Sudoku;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.*;

public class SudokuGUI {

	JFrame frame;
	Sudoku sudoku;
	JTextField[][] field;
	/**
	 * Creates a SudokuGUI complete with squares (JTextFields) and the buttons SolveButton and ClearButton.
	 * SolveButton: Sends the information to the solve method in Sudoku.java where the sudoku is solved.
	 * The solved sudoku is displayed in the GUI.
	 * ClearButton: Clears the matrix containing JTextFields.
	 */
	public SudokuGUI() {

		frame = new JFrame("Sudoku!");
		frame.setMinimumSize(new Dimension(400, 400));
		sudoku = new Sudoku();
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		JPanel panel = new JPanel();
		panel.setLayout(new GridLayout(9, 9));

		frame.add(panel);

		field = new JTextField[9][9];

		for (int i = 0; i < 9; i++) {
			for (int j = 0; j < 9; j++) {
				JTextField f = new JTextField();
				f.setHorizontalAlignment(JTextField.CENTER);
				field[i][j] = f;
				panel.add(field[i][j]);

				if (j >= 3 && j <= 5 && i >= 3 && i <= 5) {
					field[i][j].setBackground(Color.LIGHT_GRAY);
				}
				if (i >= 0 && i < 3 && j >= 0 && j < 3 || i >= 0 && i < 3
						&& j > 5 && j < 9) {
					field[i][j].setBackground(Color.LIGHT_GRAY);

				}

				if (i >= 6 && i < 9 && j >= 0 && j < 3 || i >= 6 && i < 9
						&& j > 5 && j < 9) {
					field[i][j].setBackground(Color.LIGHT_GRAY);
				}

			}

		}

		JPanel buttonPanel = new JPanel();

		JButton cButton = new ClearButton();
		buttonPanel.add(cButton);

		JButton sButton = new SolveButton();
		buttonPanel.add(sButton);

		frame.add(buttonPanel, BorderLayout.SOUTH);

		frame.pack();
		frame.setVisible(true);

	}
	
	private class ClearButton extends JButton implements ActionListener {

		public ClearButton() {
			super("Clear");
			addActionListener(this);

		}

		public void actionPerformed(ActionEvent e) {
			clearMatrix(field);
		}

	}

	private class SolveButton extends JButton implements ActionListener {

		public SolveButton() {
			super("Solve");
			addActionListener(this);

		}

		public void actionPerformed(ActionEvent e) {

			boolean isNotOk = false;

			for (int i = 0; i < 9; i++) {
				for (int j = 0; j < 9; j++) {
					String s = field[i][j].getText();

					if (!isOk(s)) {
						JOptionPane.showMessageDialog(getParent(),
								"Cheating will not be tolerated!");
						clearMatrix(field);
						isNotOk = true;
						i = 9;
						j = 9;

					}

					if (i < 9 && j < 9) {

						if (s.isEmpty()) {
							sudoku.set(i, j, 0);

						} else {
							int temp = Integer.parseInt(s);
							sudoku.set(i, j, temp);

						}
					}
				}
			}
			if (!isNotOk) {
				if (!sudoku.solve(0, 0)) {
					JOptionPane.showMessageDialog(getParent(),
							"Sudoku has no solution");
					clearMatrix(field);

				} else {

					for (int i = 0; i < 9; i++) {
						for (int j = 0; j < 9; j++) {
							field[i][j].setText(Integer.toString(sudoku.get(i,
									j)));

						}
					}
				}
			}
		}
	}

	private boolean isOk(String s) {
		if (s.isEmpty()) {
			return true;
		}
		char c = s.charAt(0);
		if (s.length() == 1 && Character.isDigit(c)) {
			return true;
		} else {
			return false;
		}

	}

	private void clearMatrix(JTextField[][] field) {
		for (int k = 0; k < 9; k++) {
			for (int l = 0; l < 9; l++) {
				field[k][l].setText("");

			}
		}

	}
}
