% RUNS ON mzn2fzn
% RUNS ON mzn2fzn_cd
array[1..5] of int: a1 = [i   | i in 1..5];             % [1,2,3,4,5]

array[1..9] of int: a2 = [i*j | i, j in 1..3];          % [1,2,3, 2,4,6, 3,6,9]

array[1..6] of int: a3 = [i*j | i in 1..2, j in 1..3];  % [1,2,3, 2,4,6]

array[1..6] of int: a4 = [i*j | i in 1..3, j in i..3];  % [1,2,3, 4,6, 9]

array[1..12] of int: a5= [i*j | i in 1..4, j in 1..4    % [2,4, 2,4,6,8,
                                                        %  6,12, 4,8,12,16]
                                where i mod 2 = 0 \/ j mod 2 = 0];

array[1..5] of int: a6 = [i | i in (3-2)..(4*5 - 15)];  % [1,2,3,4,5]


                                                % Numbers in parens are dups
                                                % and thus only appear once.

set of int: s0 = {i   | i in 1..5};             % {1,2,3,4,5}

set of int: s1 = {1   | i in 1..5};             % {1,(1),(1),(1),(1)}

set of int: s2 = {i*j | i, j in 1..3};          % {1,2,3, (2),4,6, (3),(6),9}

set of int: s3 = {i*j | i in 1..2, j in 1..3};  % {1,2,3, (2),4,6}

set of int: s4 = {i*j | i in 1..3, j in i..3};  % {1,2,3, 4,6, 9}

set of int: s5 = {i*j | i in 1..4, j in 1..4    % {2,4, (2),(4),6,8,
                                                %  (6),12, (4),(8),(12),16}
                        where i mod 2 = 0 \/ j mod 2 = 0};

solve satisfy;
