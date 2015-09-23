var 1..3: x1;
constraint x1 = if 1 + 1 < 3 then 1 else 2 endif;
solve satisfy;
