% Baking cakes for the school fete (with data file)

int: flour;  %no. grams of flour available
int: banana; %no. of bananas available
int: sugar;  %no. grams of sugar available
int: butter; %no. grams of butter available
int: cocoa;  %no. grams of cocoa available

constraint assert(flour >= 0,"Invalid datafile: " ++
                  "Amount of flour is non-negative");
constraint assert(banana >= 0,"Invalid datafile: " ++
                  "Amount of banana is non-negative");
constraint assert(sugar >= 0,"Invalid datafile: " ++
                  "Amount of sugar is non-negative");
constraint assert(butter >= 0,"Invalid datafile: " ++
                  "Amount of butter is non-negative");
constraint assert(cocoa >= 0,"Invalid datafile: " ++
                  "Amount of cocoa is non-negative");

var float: b; % no. of banana cakes
var float: c; % no. of chocolate cakes

% flour
constraint 250.0*b + 200.0*c <= int2float(flour);
% bananas
constraint 2.0*b  <= int2float(banana);
% sugar
constraint 75.0*b + 150.0*c <= int2float(sugar);
% butter
constraint 100.0*b + 150.0*c <= int2float(butter);
% cocoa
constraint 75.0*c <= int2float(cocoa);

% maximize our profit
solve maximize 4.0*b + 4.5*c;

output ["no. of banana cakes = ", show(b), "\n",
        "no. of chocolate cakes = ", show(c), "\n"];