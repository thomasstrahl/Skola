% RUNS ON mzn2fzn

% Regression test for the bug fixed in r11607.
% mzn2fzn used to abort when flattening this model.

array [0..6] of var 0.0..500.0: TimeArray; %In minutes
array [0..6] of var 0..180: Laps;
array [0..6] of var {0,40,60,80}: Fuel;
var 0.0..500.0: FinishTime ;

predicate mytime(int:N, var float:FLaps, var float:Time) =
    ( Fuel[N]=80 /\  Time = 2.0 + (1.06*FLaps)  \/ 
      Fuel[N]=60 /\  Time = 2.0 + (1.04*FLaps)  \/
      Fuel[N]=40 /\  Time = 2.0 +  FLaps  \/
      Fuel[N]=0  /\  Time = 0.0 ) ;

constraint forall (N in 0..5) (
     mytime(N, int2float(Laps[N+1]-Laps[N]),TimeArray[N+1]-TimeArray[N])
);

solve satisfy;

output [
   "Fuel = ", show(Fuel), ";\n",
   "Laps = ", show(Laps), ";\n",
   "TimeArray = ", show(TimeArray), ";\n"
];
