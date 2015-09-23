import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;


public class SudokuSolverGUI {
	JTextField[][] cells;
	int[][] values;

	/**
	 * Konstruktorn skapar ramen och lägger in två paneler.
	 * En panel med sudokuns 81 fält och en panel med en solveknapp respektive clearknapp.
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
	 * Kontrollerar de inskrivna siffrorna med metoden checkNumbers. Ifall de är giltiga skickas sudoku-matrisen
	 * till lösningsmetoden och sudokut fylls sedan i.
	 */
	public void solveSudoku(){
		if(checkNumbers()){
			SudokuSolvingAlgorithm sudoku = new SudokuSolvingAlgorithm(values);
			int[][] solvedSudoku=sudoku.sudokuSolver();
			printSudoku(solvedSudoku);
		}
	}

	/**
	 * Kollar om numren som skrivits in är giltiga (inga 0:or, bokstäver eller dubbla siffror).
	 * Ifall numret är ogiltigt informeras användaren med en dialogruta.
	 * @return	true ifall numren som skrivits in är giltiga, annars false.
	 */
	public boolean checkNumbers(){
		for(int i=0; i<9; i++){
			for (int j=0; j<9; j++){

				if(cells[i][j].getText().equals("")){}
				else{
					if(cells[i][j].getText().equals("0")||cells[i][j].getText().length()>1){
						JOptionPane.showMessageDialog(null, "Du har inte skrivit ett nummer mellan 1 och 9 på plats "+(i+1)+","+(j+1));
						return false;
					}
					else{

						try{

							values[i][j] = Integer.parseInt(cells[i][j].getText());
						}
						catch(NumberFormatException e){

							JOptionPane.showMessageDialog(null, "Du har inte skrivit ett nummer mellan 1 och 9 på plats "+(i+1)+","+(j+1));
							return false;
						}
					}
				}
			}
		}
		return true;
	}
	/**
	 * Fyller i sudokut i det grafiska gränssnittet.
	 * @ param	sudoku-matrisen som ska ritas upp.
	 */
	public void printSudoku(int[][]solvedSudoku){

		if(solvedSudoku==null){
			JOptionPane.showMessageDialog(null, "olösbar sudoku");
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
	 * Tömmer det grafiska gränssnittet och sudoku-matrisen.
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
		 * Skapar en solveknapp som lyssnar på sig själv.
		 */
		public SolveButton() {
			super("Solve");


			addActionListener(this);
		}
		/**
		 * Kör metoden solveSudoku när knappen trycks.
		 * @param 	en händelse som har hänt knappen (knappen har tryckts ner)
		 */
		public void actionPerformed(ActionEvent e) {
			solveSudoku();
		}
	}
	private class ClearButton extends JButton implements ActionListener {
		/**
		 * Skapar en clearknapp som lyssnar på sig själv.
		 */
		public ClearButton() {
			super("Clear");


			addActionListener(this);
		}
		/**
		 * Kör metoden clearSudoku när knappen trycks.
		 * @param	en händelse som har hänt knappen (knappen har tryckts ner)
		 */
		public void actionPerformed(ActionEvent e) {
			clearSudoku();



		}

	}

}
