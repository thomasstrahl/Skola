import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;


public class SudokuSolverGUI {
	JTextField[][] cells;
	int[][] values;

	/**
	 * Konstruktorn skapar ramen och l�gger in tv� paneler.
	 * En panel med sudokuns 81 f�lt och en panel med en solveknapp respektive clearknapp.
	 */
	public SudokuSolverGUI(){
		JFrame frame = new JFrame("SudokuSolver");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		values = new int[9][9];
		JPanel northPanel = new JPanel();
		frame.add(northPanel, BorderLayout.NORTH);

		northPanel.setLayout(new GridLayout(9,9));
		cells= new JTextField[9][9];

		for(int i=0; i<9; i++){
			for (int j=0; j<9; j++){
				cells[i][j] = new JTextField();

				if(i/3==1){

					if(j/3==1){
						cells[i][j].setBackground(Color.LIGHT_GRAY);
					}
				}
				else{
					if(j/3!=1){
						cells[i][j].setBackground(Color.LIGHT_GRAY);	
					}
				}



				northPanel.add(cells[i][j]);
			}

		}



		JPanel southPanel = new JPanel();
		frame.add(southPanel, BorderLayout.SOUTH);
		JButton solve = new SolveButton();
		JButton clear = new ClearButton();
		southPanel.add(solve);
		southPanel.add(clear);

		frame.pack();
		frame.setVisible(true);
	}
	/**
	 * Kontrollerar de inskrivna siffrorna med metoden checkNumbers. Ifall de �r giltiga skickas sudoku-matrisen
	 * till l�sningsmetoden och sudokut fylls sedan i.
	 */
	public void solveSudoku(){
		if(checkNumbers()){
			SudokuSolvingAlgorithm sudoku = new SudokuSolvingAlgorithm(values);
			int[][] solvedSudoku=sudoku.sudokuSolver();
			printSudoku(solvedSudoku);
		}
	}

	/**
	 * Kollar om numren som skrivits in �r giltiga (inga 0:or, bokst�ver eller dubbla siffror).
	 * Ifall numret �r ogiltigt informeras anv�ndaren med en dialogruta.
	 * @return	true ifall numren som skrivits in �r giltiga, annars false.
	 */
	public boolean checkNumbers(){
		for(int i=0; i<9; i++){
			for (int j=0; j<9; j++){

				if(cells[i][j].getText().equals("")){}
				else{
					if(cells[i][j].getText().equals("0")||cells[i][j].getText().length()>1){
						JOptionPane.showMessageDialog(null, "Du har inte skrivit ett nummer mellan 1 och 9 p� plats "+(i+1)+","+(j+1));
						return false;
					}
					else{

						try{

							values[i][j] = Integer.parseInt(cells[i][j].getText());
						}
						catch(NumberFormatException e){

							JOptionPane.showMessageDialog(null, "Du har inte skrivit ett nummer mellan 1 och 9 p� plats "+(i+1)+","+(j+1));
							return false;
						}
					}
				}
			}
		}
		return true;
	}
	/**
	 * Fyller i sudokut i det grafiska gr�nssnittet.
	 * @ param	sudoku-matrisen som ska ritas upp.
	 */
	public void printSudoku(int[][]solvedSudoku){

		if(solvedSudoku==null){
			JOptionPane.showMessageDialog(null, "ol�sbar sudoku");
		}
		else{
			for(int i=0; i<9; i++){
				for (int j=0; j<9; j++){

					cells[i][j].setText(String.valueOf(solvedSudoku[i][j]));
				}
			}
		}	

	}

	/**
	 * T�mmer det grafiska gr�nssnittet och sudoku-matrisen.
	 */
	public void clearSudoku(){
		for(int i=0; i<9; i++){
			for (int j=0; j<9; j++){
				cells[i][j].setText("");
				values[i][j]=0;
			}
		}

	}
	private class SolveButton extends JButton implements ActionListener {
		/**
		 * Skapar en solveknapp som lyssnar p� sig sj�lv.
		 */
		public SolveButton() {
			super("Solve");


			addActionListener(this);
		}
		/**
		 * K�r metoden solveSudoku n�r knappen trycks.
		 * @param 	en h�ndelse som har h�nt knappen (knappen har tryckts ner)
		 */
		public void actionPerformed(ActionEvent e) {
			solveSudoku();
		}
	}
	private class ClearButton extends JButton implements ActionListener {
		/**
		 * Skapar en clearknapp som lyssnar p� sig sj�lv.
		 */
		public ClearButton() {
			super("Clear");


			addActionListener(this);
		}
		/**
		 * K�r metoden clearSudoku n�r knappen trycks.
		 * @param	en h�ndelse som har h�nt knappen (knappen har tryckts ner)
		 */
		public void actionPerformed(ActionEvent e) {
			clearSudoku();



		}

	}

}
