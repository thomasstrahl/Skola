% RUNS ON mzn2fzn
% RUNS ON mzn2fzn_cd
par int: a = 3;
array[0..3] of var 1..10: avi;

% Check substitutions occur in various kinds of item.
par int: b = a;
par int: c = b;
par int: d = c;
par int: e = d;
par int: f = c;

constraint avi[a] < 100;

predicate p(int: x) = x == a;   % global a, should be replaced with 3
predicate q(int: a) = a == 44;  % local a, should not be replaced with 3

annotation foo(int: x);

solve maximize avi[a];

% Check substitutions occur in various kinds of expressions.
% In particular, make sure local vars that shadow global vars are handled
% correctly.
par int: g0 = 0::foo(a);        % XXX: annotation disappears!
par int: g1 = a + a;                            % 6
par int: g2 = 1 + 2 + a + 4 + 5;                % 15
par set of int: g3 = {a, a, a};                 % {3}
array[0..3] of int: g4 = array1d(0..3, [a, a, a, a]);          % [3, 3, 3, 3]
array[0..a] of int: g5 = array1d(0..a, [i | i in 0..a]);       % [0, 1, 2, 3]
array[0..3] of int: g6 = array1d(0..3, [a | a in 0..3]);       % [0, 1, 2, 3]  (local 'a')
par int: g7 = if a == 3 then 4 else 5 endif;    % 4
par int: g8 = let { int: x = a } in x;          % 3
par int: g9 = let { int: a = 44 } in a;         % 44  (local 'a')
var bool: g10 = p(a);                           % true
var bool: g11 = q(44);                          % true

% Check let-local pars are substituted.
par int: m =
    let { int: a = 4, int: b = a, int: c = a, int: d = b } in c + d;    % 8


