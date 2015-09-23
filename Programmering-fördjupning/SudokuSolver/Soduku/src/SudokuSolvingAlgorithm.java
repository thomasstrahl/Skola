
public class SudokuSolvingAlgorithm {
	private int[][] sudoku;

	/**
	 * Skapar ett sudokul�sningsobjekt.
	 * @param 	sudokun som ska l�sas
	 */
	public SudokuSolvingAlgorithm(int sudoku[][]){
		this.sudoku= sudoku;

	}
	/**
	 * L�ser sudokun.
	 * @return 	en l�st sudoku-matris eller null beroende p� om sudokun var l�sbar eller ej
	 */
	public int[][] sudokuSolver(){
		boolean solved=solve(0,0);	
		
		if(solved==true){
			return sudoku;
		}
		else {
			return null;
		}


	}
	//l�ser ett sudoku rekursivt med backtracking.
	private boolean solve(int row, int column){

		if(row>8){
			return true;
		}
		
		if(sudoku[row][column]!=0){
			if(runTest(sudoku[row][column],row,column,true)==true){
				if(column<8){

					return solve(row,column+1);

				}
				else{

					return solve(row+1,0);

				}
			}
			return false;	
		}

		for(int number=1; number<10; number++){
			if(runTest(number,row,column,false)){

				sudoku[row][column]=number;
				if(column<8){
					if(solve(row,column+1)){
						return true;
					}
				}
				else{
					if(solve(row+1,0)){
						return true;
					}
				}
			}

		}
		sudoku[row][column]=0;
		return false;


	}
	//returnerar antalet instanser av en siffra i en rad.
	private int testRow(int number,int row){
		int count=0;

		for(int k=0; k<9; k++){
			if(sudoku[row][k]==number){
				count++;
			}
		}
		return count;
	}
	//returnerar antalet instanser av en siffra i en kolumn.
	private int testColumn(int number, int column){
		int count=0;
		for(int k=0; k<9; k++){
			if(sudoku[k][column]==number){
				count++;
			}

		}
		return count;
	}


	//returnerar antalet instanser av en siffra i en ruta(3x3).
	private int testSquare(int number, int row, int column){
		int count=0;
		row = (row / 3) * 3 ;
		column = (column / 3) * 3 ;

		for(int r = 0; r < 3; r++){
			for(int c = 0; c < 3; c++){
				if(sudoku[row+r][column+c] == number){
					count++;
				}
			}
		}

		return count;

	}
	//testar att siffran f�r finnas enligt reglerna p� den best�mda platsen.
	private boolean runTest(int number, int row, int column,boolean userSet){

		if(userSet==false){
			if(testRow(number,row)==0 &&testColumn(number,column)==0 &&testSquare(number,row,column)==0){
				return true;
			}
			else{
				return false;
			}
		}
		else{
			if(testRow(number,row)==1 &&testColumn(number,column)==1 &&testSquare(number,row,column)==1){
				return true;
			}
			else{
				return false;
			}
		}
	}


}
