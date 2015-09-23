% Baking cakes for the school fete (with data file)

float: flour; %no. cups of flour available
float: banana; %no. of bananas available
float: sugar; %no. cups of sugar available
float: butter; %no. cups of butter available
float: cocoa; %no. cups of cocoa available

var 0..floor(2.0*butter): b; % no. of banana cakes
var 0..floor(2.0*butter): c; % no. of chocolate cakes

% flour
constraint 2.0*int2float(b) + 1.5*int2float(c) <= flour;
% bananas
constraint 2.0*int2float(b)  <= banana;
% sugar
constraint 0.5*int2float(b) + 1.0*int2float(c) <= sugar;
% butter
constraint 0.5*int2float(b) + 0.5*int2float(c) <= butter;
% cocoa
constraint 0.5*int2float(c) <= cocoa;

% maximize our profit
solve maximize 4.0*int2float(b) + 4.5*int2float(c);

output ["no. of banana cakes = ", show(b), "\n",
           "no. of chocolate cake = ", show(c), "\n"];

  