% RUNS ON mzn2fzn
%
% This test checks that mzn2fzn passes the cpviz annotations through
% to the generated FlatZinc.

array[1..5] of var 0..10: x 
   :: viz([
	viztype("vector"),
        vizpos(0, 2),
        vizwidth(5),
        vizheight(11),
        vizrange(0, 10)
   ])
   :: viz([
        viztype("vector_waterfall"),
        vizpos(8, 0),
        vizwidth(5),
        vizheight(15)
   ]);

solve satisfy;
