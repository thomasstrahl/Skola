% RUNS ON mzn2fzn

array[1..6] of var int: x;

constraint trace("Nested (1) ",
                trace("Nested (2)",
                     trace("Nested (3)\n", true)));

constraint forall (i in 1 .. 5) (
    trace("Processing i = " ++ show(i) ++ "\n",
        x[i] < x[i + 1]
    )
);

solve satisfy;
