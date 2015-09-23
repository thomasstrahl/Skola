% RUNS ON mzn_tc
% Testing which kinds of variables can be used without being defined.  It
% assumes we are doing instance checking (ie. no --model-check-only).
%-----------------------------------------------------------------------------%
% Global variables
%-----------------------------------------------------------------------------%

par int: pi;
par int: pi2 = pi;                      % bad
var int: vi;
var int: vi2 = vi;                      % ok

par 1..5: pre;
par 1..5: pre2 = pre;                   % bad
var 1..5: vre;
var 1..5: vre2 = vre;                   % ok

par {1,2}: pse;
par {1,2}: pse2 = pse;                  % bad
var {1,2}: vse;
var {1,2}: vse2 = vse;                  % ok

par bool: pb;
par bool: pb2 = pb;                     % bad
var bool: vb;
var bool: vb2 = vb;                     % ok

par float: pf;
par float: pf2 = pf;                    % bad
var float: vf;
var float: vf2 = vf;                    % ok

par set of int: psi;
par set of int: psi2 = psi;             % bad
var set of 1..5: vsi;
var set of 1..5: vsi2 = vsi;            % ok

array[1..3] of par int: api;        
array[1..3] of par int: api2 = api;     % bad
array[1..3] of var int: avi;        
array[1..3] of var int: avi2 = avi;     % ok
var array[1..3] of int: vai;            % illegal type
var array[1..3] of int: vai2 = vai;     % illegal type

set of int: S = {1,2,3};
par S: pS;
par S: pS2 = pS;                        % bad
var S: vS;
var S: vS2 = vS;                        % ok

% These ones are bad for a different reason;  they're in here just to make
% sure that the definedness checking of them doesn't cause aborts or the like.
par min: po;                            % bad
no_such_type: t;                        % bad

%-----------------------------------------------------------------------------%
% Let-local variables
%-----------------------------------------------------------------------------%
par int: pi3 = let { par int: pi = 1 } in pi;       % ok
var int: vi3 = let { var int: vi = 1 } in vi;       % ok
par int: pi4 = let { par int: pi     } in pi;       % bad
var int: vi4 = let { var int: vi     } in vi;       % ok

solve satisfy;
