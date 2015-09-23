% Sudoku solver.

include "globals.mzn";

array [1..9, 1..9] of var 1..9: sq;

predicate row_diff(int: r) =
    all_different (c in 1..9) (sq[r, c]);

predicate col_diff(int: c) =
    all_different (r in 1..9) (sq[r, c]);
  
predicate subgrid_diff(int: r, int: c) =
    all_different (i, j in 0..2) (sq[r + i, c + j]);
  
constraint forall (r in 1..9)         (row_diff(r));
constraint forall (c in 1..9)         (col_diff(c));
constraint forall (r, c in {1, 4, 7}) (subgrid_diff(r, c));
  
sq =
 [| _, _, _, _, _, _, _, _, _
  | _, 6, 8, 4, _, 1, _, 7, _
  | _, _, _, _, 8, 5, _, 3, _
  | _, 2, 6, 8, _, 9, _, 4, _
  | _, _, 7, _, _, _, 9, _, _
  | _, 5, _, 1, _, 6, 3, 2, _
  | _, 4, _, 6, 1, _, _, _, _
  | _, 3, _, 2, _, 7, 6, 9, _
  | _, _, _, _, _, _, _, _, _
 |];
  
solve satisfy;

output ["sq = ", show(sq), "\n"];