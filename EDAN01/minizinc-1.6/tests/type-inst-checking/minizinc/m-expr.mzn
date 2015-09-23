% RUNS ON mzn_tc
par bool:  pb  = true /\ false \/ not true;
var bool:  vb1 = pb;                            % inst coercion
var bool:  vb2 = _;                             % type coercion

par int:   pi1 = 3 + 4 * 5;
par int:   pi2 = bool2int(true);                % explicit type coercion
var int:   vi1 = pi1;                           % inst coercion
var int:   vi2 = bool2int(vb1);                 % explicit type coercion
var int:   vi3 = _;                             % type coercion

par float: pf1 = 3.0 * 5e1 / 6.0;
par float: pf2 = int2float(pi1);                % explicit type coercion
var float: vf1 = pf1;                           % inst coercion
var float: vf2 = int2float(vi1);                % explicit type coercion
var float: vf3 = _;                             % type coercion

par set of 1..3: psi1 = {1, 2, 3};
par set of 1..3: psi2 = {i | i in psi1};
var set of 1..3: vsi1 = psi1;                   % inst coercion
var set of 1..3: vsi2 = _;                      % type coercion

array[1..3] of par int: api1 = [1,2,3];
array[1..3] of par int: api2 = [i | i in api1];
array[1..3] of var int: api4 = [pi1,pi1,pi1];   % inst coercion
array[1..3] of var int: api5 = [_,_,_];         % type coercion
array[1..2,1..2] of par int: apii = [|1,2|3,4|];

par int: pi3 = if pb then api1[1] else apii[1,1] endif;
par int: pi4 = let { par int: a = 1 } in a;

par bool: pb2 = forall (i in [1,2,3]) (i > 0);

var float: vf4 = int2float(3);      % expl. type coercion, impl. inst coercion

solve satisfy;
