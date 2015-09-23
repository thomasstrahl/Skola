annotation foo;
% The annotation on x should not be removed.
var int: x :: foo;
% The is_defined_var annotation on y should be removed.
var int: y :: foo :: is_defined_var = x;
var int: z;
% The defines_var(y) annotation should be removed.
constraint (x = z + z) :: foo :: defines_var(x) :: defines_var(y);
solve satisfy;
