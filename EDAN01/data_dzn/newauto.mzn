include "globals.mzn";

int: del_add;
int: del_mul;
int: number_add;
int: number_mul;
int: n;
set of int : last;
set of int: add;
set of int: mul;

int: adds = card(add);
int: muls = card(mul);

array[1..n] of set of int: dependencies;

array[1..adds] of var 0..10000: add_start;
array[1..muls] of var 0..10000: mul_start;

var 0..10000: end;

array[1..adds] of int: add_duration = array1d(1..adds, [del_add, del_add, del_add, del_add, del_add, del_add, del_add, del_add, del_add, del_add, del_add, del_add]);
array[1..muls] of int: mul_duration = array1d(1..muls, [del_mul, del_mul, del_mul, del_mul, del_mul, del_mul, del_mul, del_mul, del_mul, del_mul, del_mul, del_mul, del_mul, del_mul, del_mul, del_mul]);
array[1..adds] of int: add_resources = array1d(1..adds, [1,1,1,1,1,1,1,1,1,1,1,1]);
array[1..muls] of int: mul_resources = array1d(1..muls, [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]);

array[1..n] of var int: start_times = array1d(1..n, [mul_start[1], mul_start[2], mul_start[3], mul_start[4], mul_start[5], mul_start[6], mul_start[7], mul_start[8], add_start[1], add_start[2], add_start[3], add_start[4], add_start[5], add_start[6], mul_start[9], mul_start[10], mul_start[11], mul_start[12], add_start[7], add_start[8], mul_start[13], mul_start[14], mul_start[15], mul_start[16], add_start[9], add_start[10], add_start[11], add_start[12]]);

constraint cumulative(add_start, add_duration, add_resources, number_add);
constraint cumulative(mul_start, mul_duration, mul_resources, number_mul);

constraint 
  start_times[1] + del_mul <= start_times[9] /\
  start_times[2] + del_mul <= start_times[9] /\
  start_times[3] + del_mul <= start_times[10] /\  
  start_times[4] + del_mul <= start_times[10] /\
  start_times[5] + del_mul <= start_times[11] /\
  start_times[6] + del_mul <= start_times[11] /\
  start_times[7] + del_mul <= start_times[12] /\
  start_times[8] + del_mul <= start_times[12] /\
  start_times[9] + del_add <= start_times[27] /\
  start_times[10] + del_add <= start_times[28] /\
  start_times[11] + del_add <= start_times[13] /\
  start_times[12] + del_add <= start_times[14] /\  
  start_times[13] + del_add <= start_times[16] /\
  start_times[13] + del_add <= start_times[17] /\
  start_times[14] + del_add <= start_times[15] /\
  start_times[14] + del_add <= start_times[18] /\
  start_times[15] + del_mul <= start_times[19] /\
  start_times[16] + del_mul <= start_times[19] /\
  start_times[17] + del_mul <= start_times[20] /\
  start_times[18] + del_mul <= start_times[20] /\
  start_times[19] + del_add <= start_times[23] /\
  start_times[19] + del_add <= start_times[22] /\
  start_times[20] + del_add <= start_times[24] /\
  start_times[20] + del_add <= start_times[21] /\
  start_times[21] + del_mul <= start_times[25] /\
  start_times[22] + del_mul <= start_times[25] /\
  start_times[23] + del_mul <= start_times[26] /\
  start_times[24] + del_mul <= start_times[26] /\
  start_times[25] + del_add <= start_times[27] /\
  start_times[26] + del_add <= start_times[28] /\
  start_times[27] + del_add <= end /\
  start_times[28] + del_add <= end;

solve minimize end;

output[ show (end)];
