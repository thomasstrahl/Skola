set of int: index = 1..3;
array[index] of 0.0..1.0: t = [1.0, 2.0, 3.0];
var 0.0..7.0: prod;
constraint prod = product(i in index)(t[i]);
solve satisfy;
