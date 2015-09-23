% the sum of booleans x = s
predicate bool_sum_eq(array[int] of var bool:x, int:s) =
     let { int: c = length(x) } in
     if s < 0 then false 
     else if s == 0 then forall(i in 1..c)(x[i] == false)
     else if s < c then
	  let { % cp = nearest power of 2 >= c
	        int: cp = pow(2,ceil(log2(int2float(c)))),
	        array[1..cp] of var bool:y, % y is padded version of x
		array[1..cp] of var bool:z  } in
	  forall(i in 1..c)(y[i] == x[i]) /\
	  forall(i in c+1..cp)(y[i] == false) /\
	  oesort(y, z) /\ z[s] == true /\ z[s+1] == false
     else if s == c then forall(i in 1..c)(x[i] == true)
     else false endif endif endif endif;

%% odd-even sort
%% y is the sorted version of x, all trues before falses
predicate oesort(array[int] of var bool:x, array[int] of var bool:y) =
  let { int: c = card(index_set(x)) } in
  if c == 1 then x[1] == y[1] 
  else if c == 2 then comparator(x[1],x[2],y[1],y[2])
  else
    let { 
      array[1..c div 2] of var bool:xf = [ x[i] | i in 1..c div 2],
      array[1..c div 2] of var bool:xl = [ x[i] | i in c div 2 +1..c],
      array[1..c div 2] of var bool:tf,
      array[1..c div 2] of var bool:tl } in
    oesort(xf,tf) /\ oesort(xl,tl) /\ oemerge(tf ++ tl, y)
  endif endif;

%% odd-even merge
%% y is the sorted version of x, all trues before falses
%% assumes first half of x is sorted, and second half of x
predicate oemerge(array[int] of var bool:x, array[int] of var bool:y) =
  let { int: c = card(index_set(x)) } in
  if c == 1 then x[1] == y[1] 
  else if c == 2 then comparator(x[1],x[2],y[1],y[2])
  else
    let { array[1..c div 2] of var bool:xo = 
            [ x[i] | i in 1..c where i mod 2 == 1],
          array[1..c div 2] of var bool:xe = 
            [ x[i] | i in 1..c where i mod 2 == 0],
          array[1..c div 2] of var bool:to,
          array[1..c div 2] of var bool:te } in
      oemerge(xo,to) /\ oemerge(xe,te) /\
      y[1] = to[1] /\
      forall(i in 1..c div 2 -1)(
        comparator(te[i],to[i+1],y[2*i],y[2*i+1])) /\ 
      y[c] = te[c div 2]
  endif endif));
 
% comparator o1 = max(i1,i2), o2 = min(i1,i2)
predicate comparator(var bool:i1,var bool:i2,var bool:o1,var bool:o2) =
	  (o1 = (i1 \/ i2)) /\ (o2 = (i1 /\ i2));