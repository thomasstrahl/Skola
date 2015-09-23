% Test that p(_) :: foo :: bar :: baz is produced, not just p(_) :: foo.
% RUNS ON mzn2fzn
annotation foo;
annotation bar;
annotation baz;
ann: a = foo :: bar :: baz;
predicate p(var int: x);
constraint p(_) :: a;
solve satisfy;
