include "globals.mzn";

int: adders;
int: mulers;

int: addTime;
int: mulTime;

set of int: TOTAL;

array[TOTAL] of var int: index;

set of int: numAdds;
set of int: numMuls;

array[numAdds] of var 0..1000: adds;
array[numMuls] of var 0..1000: muls;
			
var int: end;

array[numAdds] of int: addTimes;
array[numMuls] of int: mulTimes;


addTimes = [addTime,addTime,addTime,addTime
			,addTime,addTime,addTime,addTime
			,addTime,addTime,addTime,addTime];
			
mulTimes = [mulTime,mulTime,mulTime,mulTime
			,mulTime,mulTime,mulTime,mulTime
			,mulTime,mulTime,mulTime,mulTime
			,mulTime,mulTime,mulTime,mulTime];


array[numAdds] of var int: addCost;
array[numMuls] of int: mulCost;


constraint cumulative(adds, addTimes, addCost, adders);
constraint cumulative(muls, mulTimes, mulCost, mulers);

constraint (index[3] + mulTime <= index[10]);
constraint (index[4] + mulTime <= index[10]);

constraint (index[5] + mulTime <= index[11]);
constraint (index[6] + mulTime <= index[11]);

constraint (index[10] + addTime <= index[13]);

constraint (index[11] + addTime <= index[14]);

constraint (index[13] + addTime <= index[15]);

constraint (index[14] + addTime <= index[16]);

constraint (index[13] + addTime <= index[17]);

constraint (index[14] + addTime <= index[18]);

constraint (index[15] + mulTime <= index[19]);
constraint (index[16] + mulTime <= index[19]);

constraint (index[17] + mulTime <= index[20]);
constraint (index[18] + mulTime <= index[20]);

constraint (index[19] + addTime <= index[23]);

constraint (index[19] + addTime <= index[21]);

constraint (index[20] + addTime <= index[24]);

constraint (index[20] + addTime <= index[22]);

constraint (index[7] + mulTime <= index[12]);
constraint (index[8] + mulTime <= index[12]);

constraint (index[23] + mulTime <= index[26]);
constraint (index[24] + mulTime <= index[26]);

constraint (index[21] + mulTime <= index[25]);
constraint (index[22] + mulTime <= index[25]);

constraint (index[2] + mulTime <= index[9]);
constraint (index[1] + mulTime <= index[9]);

constraint (index[12] + addTime <= index[28]);
constraint (index[26] + addTime <= index[28]);

constraint (index[25] + addTime <= index[27]);
constraint (index[9] + addTime <= index[27]);

constraint (index[28] + addTime <= end);
constraint (index[27] + addTime <= end);

output ["EndTime", show(end)];
solve minimize end;