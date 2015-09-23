% RUNS ON mzn2fzn

% mzn2fzn was introducing an extra variable when flattening iffall in a
% reified context. The following model was being flattened to:
%
% var bool: BOOL____00001 :: var_is_introduced;
% var bool: BOOL____00002 :: is_defined_var :: var_is_introduced;
% var bool: b1;
% var bool: b2;
% constraint array_bool_xor([BOOL____00002, b1, b2]);
% constraint bool_eq(BOOL____00001, BOOL____00002) ::
% defines_var(BOOL____00002);
% constraint bool_le(b1, BOOL____00001);
% solve satisfy;
%

var bool: x1;
var bool: x2;

constraint x1 -> iffall([x1, x2]);

solve satisfy;
