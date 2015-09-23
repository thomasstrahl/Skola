% RUNS ON mzn_sc
% Zinc-only type item
type MyInt = int;

% Zinc-only flat enum item
enum C = { R, G, B };

% Zinc-only non-flat enum item
enum X = { foo(int: x), bar(float: y) };

% Zinc-only user-defined functions
function int: f(int: x) = 3;
