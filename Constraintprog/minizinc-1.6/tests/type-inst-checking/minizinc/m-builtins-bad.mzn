% RUNS ON mzn_tc
%-----------------------------------------------------------------------------%
% Built-ins that are in Zinc/FlatZinc but not in MiniZinc.
%-----------------------------------------------------------------------------%
% The types are all wrong for most of the args and return values, but that
% doesn't matter because the symbols don't even exist.

int: x = 1;
int: y = 2;
int: z = 3;
bool: b = true;

%-----------------------------------------------------------------------------%
% Zinc ones
%-----------------------------------------------------------------------------%
int: i1 = foldl(x, y, z);
int: i2 = foldr(x, y, z);


int: i4 = holds(x);

%-----------------------------------------------------------------------------%
% Some FlatZinc ones
%-----------------------------------------------------------------------------%
int: i5 = int_eq(x, y);
int: i6 = float_plus(x, y, z);
int: i7 = float_lin_eq_reif(x, y, z, b);

solve satisfy;
