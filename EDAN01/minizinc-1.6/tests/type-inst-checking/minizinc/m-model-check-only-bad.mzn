% RUNS ON mzn_tc
% These uses of undefined things should only be allowed with
% --model-check-only.  The comments give the errors we would get without
% --model-check-only.
set of 1..5: s;                     % Undefined variable
s: a;                               % Using undefined variable as a type
set of 1..5: s2 = s;                % Using undefined variable as a value

% This is bad even with --model-check-only -- you always have to assign
% let-local parameters.
par int: pi = let { par int: pi2 } in pi2;

solve satisfy;
