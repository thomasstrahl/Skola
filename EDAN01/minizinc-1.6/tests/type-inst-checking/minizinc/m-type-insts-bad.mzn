% RUNS ON mzn_tc
% Sets
var set of float:          s1;      % error
var set of bool:           s2;      % error
set of var int:            s3;      % error
set of set of int:         s4;      % error
set of array[0..3] of int: s5;      % error
set of string:             s5b;     % error
% Arrays
array[0..3] of set of set of int:  a1;  % error (in the set)
array[0..3] of array[0..3] of int: a2;  % error
var array[0..3] of int:            a3;  % error
array[int] of int:                 a4;  % not an error

set of int: s6 = {3,7};
array[s6] of int: a6;                   % error in index

set of int: s7 = {1,3,5};
array[s7] of int: a7;                   % error in index

1..3.0: a;
3.0..s6: b;

%-----------------------------------------------------------------------------%

% At one point, this was aborting (bug #41).
array[no_such_name] of int: a8;

%-----------------------------------------------------------------------------%

% Zinc-only type constraint
(int: x where x > 0): a9;

% Zinc-only tuple
tuple(int,int): x2;

% Zinc-only record
record(int:a): x3;

% Zinc-only type variable
predicate p($T: x);

% No parens around type-inst exprs
%(int): a;

% Zinc-only operation TIE
annotation my_ann( op(int:(int,int)): a );

%-----------------------------------------------------------------------------%

solve satisfy;
