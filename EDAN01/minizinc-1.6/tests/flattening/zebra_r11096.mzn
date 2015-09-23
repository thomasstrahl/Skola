
% RUNS ON mzn2fzn

% mzn2fzn was erroneously introducing redundant set_in constraints during
% flattening.  This limited the solvers that this model could be run
% on to those that supported set constraints.  This was fixed in r11096-7.

include "globals.mzn";

set of int: Nationalities = 0..4;
int: English = 0;
int: Spanish = 1;
int: Ukrainian = 2;
int: Norwegian = 3;
int: Japanese = 4;
set of int: Colours = 0..4;
int: Red = 0;
int: Green = 1;
int: Ivory = 2;
int: Yellow = 3;
int: Blue = 4;
set of int: Animals = 0..4;
int: Dog = 0;
int: Fox = 1;
int: Horse = 2;
int: Zebra = 3;
int: Snails = 4;
set of int: Drinks = 0..4;
int: Coffee = 0;
int: Tea = 1;
int: Milk = 2;
int: OrangeJuice = 3;
int: Water = 4;
set of int: Cigarettes = 0..4;
int: OldGold = 0;
int: Kools = 1;
int: Chesterfields = 2;
int: LuckyStrike = 3;
int: Parliaments = 4;

set of int: Houses = 1..5;

array[Nationalities] of var Houses: nation;
array[Colours] of var Houses: colour;
array[Animals] of var Houses: animal;
array[Drinks] of var Houses: drink;
array[Cigarettes] of var Houses: smoke;

predicate nextto(var Houses:h1, var Houses:h2) = 
       h1 == h2 + 1 \/ h2 == h1 + 1;

predicate rightof(var Houses:h1, var Houses:h2) =
       h1 == h2 + 1;

predicate middle(var Houses:h) = h == 3;

predicate left(var Houses:h) = h = 1;

constraint
	all_different(nation) /\
	all_different(colour) /\
	all_different(animal) /\
	all_different(drink) /\
	all_different(smoke) /\
	nation[English] == colour[Red] /\
	nation[Spanish] == animal[Dog] /\
	drink[Coffee] == colour[Green] /\
        nation[Ukrainian] == drink[Tea] /\
        colour[Green] `rightof` colour[Ivory] /\
        smoke[OldGold] == animal[Snails] /\
        smoke[Kools] == colour[Yellow] /\
        middle(drink[Milk]) /\
        left(nation[Norwegian]) /\
        smoke[Chesterfields] `nextto` animal[Fox] /\
        smoke[Kools] `nextto` animal[Horse] /\
        smoke[LuckyStrike] == drink[OrangeJuice] /\
        nation[Japanese] == smoke[Parliaments] /\
        nation[Norwegian] `nextto` colour[Blue];

solve satisfy;
