% RUNS ON mzn_tc
set of int: S = {1,2,3};
int: i = 3;

%-----------------------------------------------------------------------------%
% Sets of basic types
%-----------------------------------------------------------------------------%
var set of int:                 b1;     % not finite
var set of 1..3:                b2;     %     finite
var set of {1,3}:               b3;     %     finite
var set of S:                   b6;     %     finite
var set of i:                   b9;     % i cannot be used as a value, and
                                        %   so it shouldn't trigger a
                                        %   non-finiteness error

var set of array[0..3] of bool: c5;     % illegal MiniZinc set, but it
                                        % exposed a bug at one point -- it
                                        % was aborting erroneously

predicate d2(var set of int: x) = true; % not finite, but allowed

solve satisfy;
