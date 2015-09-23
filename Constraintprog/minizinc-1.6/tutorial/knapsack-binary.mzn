int: n;
set of int: items=1..n;
int: capacity;

array[items] of int: profits;
array[items] of int: weights;

var set of items: knapsack;

constraint sum (i in knapsack) (weights[i]) <= capacity;

solve maximize sum (i in knapsack) (profits[i]) ;

output [show(knapsack)];

