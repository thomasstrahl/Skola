% RUNS ON mzn_tc


test      pp(par int: x) = x > 0;   % ret type-inst is 'par bool'


predicate pv(var int: x) = x > 0;   % ret type-inst is 'var bool'

par bool: b1 = pp(3);
par bool: b2 = pp(_+1);     % error: require par bool arg
par bool: b3 = pv(3);       % error: returns var bool
par bool: b4 = pv(_+1);     % error: returns var bool

solve satisfy;
