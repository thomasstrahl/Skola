int: m; % no of tasks
int n: % no. of workers

array[1..n] of set of 1..m: qualifications;
array[1..n] of int: cost;

var set of 1..n: workers;

constraint forall (t in 1..m) 
     (exists (w in 1..n) (w in workers /\ t in qualifications[w]));
     
