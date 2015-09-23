predicate boolsum(array[int] of var bool:x, int:s) =
	  let { int:c = length(x) } in
	  if c == 0 then s == 0
	  else if c == 1 then boolsum1(x,s)
	  else if c == 2 then boolsum2(x,s)
	  else if c == 3 then boolsum3(x,s)
	  else if c == 4 then boolsum4(x,s)
	  else false endif endif endif endif endif;

predicate boolsum1(array[int] of var bool:x, int:s) =
	  if s == 0 then x[1] == false 
	  else if s == 1 then x[1] == true 
	  else false endif endif;

predicate boolsum2(array[int] of var bool:x, int:s) =
	  if s == 0 then x[1] == false /\ x[2] == false
	  else if s == 1 then (x[1] == false /\ x[2] == true) \/ 
	       	              (x[1] == true /\ x[2] == false)
	  else if s == 2 then x[1] == true /\ x[2] == true 
	  else false endif endif endif;

predicate boolsum3(array[int] of var bool:x, int:s) =
	  if s == 0 then x[1] == false /\ x[2] == false /\ x[3] == false
	  else if s == 1 then (x[1] == false /\ x[2] == true /\ x[3] == false) \/ 
	       	              (x[1] == true /\ x[2] == false /\ x[3] == false) \/
	       	              (x[1] == false /\ x[2] == false /\ x[3] == true)
	  else if s == 2 then (x[1] == true /\ x[2] == true /\ x[3] == false) \/ 
	       	              (x[1] == true /\ x[2] == false /\ x[3] == true) \/
	       	              (x[1] == false /\ x[2] == true /\ x[3] == true)
	  else if s == 3 then x[1] == true /\ x[2] == true /\ x[3] == true
	  else false endif endif endif endif;

predicate boolsum4(array[int] of var bool:x, int:s) =
	  if s == 0 then x[1] == false /\ x[2] == false /\ x[3] == false /\ x[4] == false
	  else if s == 1 then (x[1] == false /\ x[2] == true /\ x[3] == false /\ x[4] == false) \/ 
	       	              (x[1] == true /\ x[2] == false /\ x[3] == false /\ x[4] == false) \/
	       	              (x[1] == false /\ x[2] == false /\ x[3] == false /\ x[4] == true) \/
	       	              (x[1] == false /\ x[2] == false /\ x[3] == true /\ x[4] == false)
	  else if s == 2 then (x[1] == true /\ x[2] == true /\ x[3] == false /\ x[4] == false) \/ 
	       	              (x[1] == true /\ x[2] == false /\ x[3] == true /\ x[4] == false) \/
	       	              (x[1] == true /\ x[2] == false /\ x[3] == false /\ x[4] == true) \/
	       	              (x[1] == false /\ x[2] == true /\ x[3] == true /\ x[4] == false) \/
	       	              (x[1] == false /\ x[2] == true /\ x[3] == false /\ x[4] == true) \/
	       	              (x[1] == false /\ x[2] == false /\ x[3] == true /\ x[4] == true)
	  else if s == 3 then (x[1] == true /\ x[2] == true /\ x[3] == true /\ x[4] == false) \/ 
	       	              (x[1] == true /\ x[2] == true /\ x[3] == false /\ x[4] == true) \/
	       	              (x[1] == true /\ x[2] == false /\ x[3] == true /\ x[4] == true) \/
	       	              (x[1] == false /\ x[2] == true /\ x[3] == true /\ x[4] == true)
	  else if s == 4 then x[1] == true /\ x[2] == true /\ x[3] == true /\ x[4] == true
	  else false endif endif endif endif endif;



	  