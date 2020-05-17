-module(second).
-export([hypotenuse/2, perimeter/2, area/2]).

% right-angled triangle
hypotenuse(A,B) ->
   math:sqrt(first:square(A) + first:square(B)).

% right-angled triangle, given: 2 short sides
perimeter(A,B) ->
    A+B+hypotenuse(A, B).

% right-angled triangle, given: 2 short sides
area(A,B) ->
    A*B/2.
