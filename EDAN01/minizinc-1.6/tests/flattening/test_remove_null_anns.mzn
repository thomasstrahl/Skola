var 1..10: x;
% The null annotation should be removed entirely from the FlatZinc.
ann: search = int_search([x], first_fail, indomain_min, complete) :: null;
solve :: search satisfy;
