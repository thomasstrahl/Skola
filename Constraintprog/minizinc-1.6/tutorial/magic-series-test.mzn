int: n=4;
array[1..n] of int: t= [1,2,1,0];
array[1..n] of var 0..n: s;

constraint forall(i in 1..n) (
   s[i] = sum(j in 1..n)(bool2int(t[j]=i-1)));

solve satisfy;   