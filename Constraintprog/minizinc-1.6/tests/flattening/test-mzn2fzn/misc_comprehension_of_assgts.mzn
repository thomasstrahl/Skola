var 1..3: x;
array [1..3] of var bool: ys = [x = i | i in 1..3];
solve satisfy;
