var 1..3: x1;
constraint x1 = if x1 > 1 then 3 else 1 endif;
solve satisfy;
