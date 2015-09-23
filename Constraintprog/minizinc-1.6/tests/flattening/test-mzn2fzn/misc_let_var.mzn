var 1..3: x1;
constraint let { var 1..3: x2 } in x2 > x1;
solve satisfy;
