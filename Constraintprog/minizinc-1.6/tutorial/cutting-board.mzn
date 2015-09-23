% board cutting problem
int: n; % number of rectangles
set of int: rect = 1..n;
array[rect] of int: width;
array[rect] of int: height;
int: h; %height of board

int: max_width = sum (r in rect) (width[r]) :: is_output;
array[rect] of var 1..max_width: x :: is_output;
array[rect] of var 1..h: y :: is_output;
var 1..max_width: w :: is_output;

predicate 
non_overlap(int: w1, int: h1, var int: x1, var int: y1, int: w2, int: h2, var int: x2, var int: y2) = 
(x1 + w1 <= x2) \/ (x2 + w2 <= x1) \/ (y1 + h1 <= y2) \/ (y2 + h2 <= y1);

constraint forall (r in rect) (x[r]+width[r] <= w);
constraint forall (r in rect) (y[r]+height[r] <= h);
constraint forall (r,s in rect where r < s) (non_overlap(width[r], height[r], x[r], y[r], width[s], height[s], x[s], y[s]) );

solve minimize w;

output [show(w)];