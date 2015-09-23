package Sudoku;

public class Sudoku {

	private int[][] mat;

	public Sudoku() {
		mat = new int[9][9];

	}

	/**
	 * Returns the value at the position y, x in the matrix mat.
	 * 
	 * @param y
	 * @param x
	 * @return Returns a int matrix.
	 */
	public int get(int y, int x) {

		return mat[y][x];

	}

	/**
	 * Sets the value at the position y, x in the matrix mat.
	 * 
	 * @param y
	 *            the coordinate
	 * @param x
	 *            the coordinate
	 * @param value
	 *            the integer to be added
	 */
	public void set(int y, int x, int value) {
		mat[y][x] = value;
	}

	/**
	 * Finds the solution for the sudoku. and And calls upon the private
	 * methods: rowCheck, colCheck and squareCheck to check if it is according
	 * to the rules.
	 * 
	 * @param y
	 *            the coordinate
	 * @param x
	 *            the coordinate
	 * @return Returns true if the sudoku has a solution, else false.
	 */
	public boolean solve(int y, int x) {
		if (y > 8) {
			return true;
		}

		int value = mat[y][x];

		if (!(value >= 0 || value <= 9)) {
			return false;
		}

		if (value == 0) {
			for (int i = 1; i < 10; i++) {
				if (rowCheck(y, x, i) && colCheck(y, x, i)
						&& squareCheck(y, x, i)) {
					mat[y][x] = i;

					boolean res = false;
					if (x == 8) {
						res = solve(y + 1, 0);
					} else {
						res = solve(y, x + 1);
					}

					if (res == true) {
						return true;
					}

				}

			}

			mat[y][x] = 0;
			return false;
		} else {

			if (x == 8) {
				return solve(y + 1, 0);
			}
			return solve(y, x + 1);

		}
	}
	

	private boolean rowCheck(int y, int x, int value) {
		for (int i = 0; i < 9; i++) {
			if (mat[y][i] == value && i != x) {
				return false;
			}
		}
		return true;
	}

	private boolean colCheck(int y, int x, int value) {
		for (int i = 0; i < 9; i++) {
			if (mat[i][x] == value && i != y) {
				return false;
			}
		}
		return true;
	}

	private boolean squareCheck(int y, int x, int value) {
		int tempX = (x / 3) * 3;
		int tempY = (y / 3) * 3;
		for (int i = 0; i < 3; i++) {
			for (int j = 0; j < 3; j++) {

				if (mat[tempY + i][tempX + j] == value && i != y && j != x) {
					return false;
				}
			}
		}

		return true;
	}
}
