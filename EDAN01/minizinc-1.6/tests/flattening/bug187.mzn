% RUNS ON mzn2fzn

% Regression test for bug #187.  mzn2fzn would abort in with the
% application fo bool_clause below - it was missing code to handle
% the case where the inst of the first argument (the empty array) is 
% var.

predicate bool_clause(array [int] of var bool: as,
    array [int] of var bool: bs);

int: nn;
set of int: N = 1..nn;
int: ee;
set of int: E = 1..ee;
array[E,1..2] of int: es; % edge i is (es[i,1], es[i,2])

int: mm;
set of int: M = 1..mm;
int: ff;
set of int: F = 1..ff;
array[F,1..2] of int: fs; % edge i is (fs[i,1], fs[i,2])

int: vsyms;
array[1..vsyms]  of set of int: vs;

array[E,F] of var bool: b; 
array[N,M] of var bool: c;

array[N,M] of var bool: cms;

constraint forall(s in 1..vsyms)(
	   	  forall(m1,m2 in vs[s] where m1 < m2)(
		      forall(n1 in N)(
			  bool_clause([],[c[n1,m1],cms[n1,m2]])
		      )
 		  )
	   );       		    

solve satisfy;

nn = 8;
ee = 9;
es = [| 1,2
      | 1,4
      | 2,4
      | 3,4
      | 4,5
      | 5,6
      | 5,7
      | 5,8
      | 7,8 |]; 

mm = 7;
ff = 6;
fs = [| 1,3
      | 2,3
      | 3,4
      | 4,5
      | 4,6
      | 4,7 |];
vsyms = 2;
vs = [{1,2},{5,6,7}];
