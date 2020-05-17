% chapter 2.8

-module(assignment).
-export([perimeter/1, area/1, enclose/1, bits/1, bits_direct/1]).
-export([perimeter_test/0, area_test/0, enclose_test/0, bits_test/0, bits_direct_test/0]).

perimeter({circle, {_X,_Y}, R}) -> 2 * math:pi() * R;
perimeter({triangle, {_X,_Y}, A,B,C}) -> A+B+C.

perimeter_test() ->
    18.84955592153876 = perimeter({circle, {0,0}, 3}),
    31.41592653589793 = perimeter({circle, {0,0}, 5}),
    12 = perimeter({triangle, {0,0}, 3,4,5}),
    26.5 = perimeter({triangle, {0,0}, 13,4.5,9}),
    pass.


area({triangle, {_X,_Y}, A, B, C}) ->
    S = (A+B+C)/2,
    math:sqrt(S*(S-A)*(S-B)*(S-C)).

area_test() ->
    6.0 = area({triangle, {0,0}, 3, 4, 5}),
    73.17914661978507 = area({triangle, {0,0}, 13, 13, 13}),
    pass.


enclose({circle, {X,Y}, R}) -> 
    {rectangle, {X,Y}, R*2, R*2}.

enclose_test() ->
   {rectangle, {5,7}, 6, 6} = enclose({circle, {5,7}, 3}),
   pass.


% tail recursion
bits(N) -> bits(N, 0).

bits(0, S) -> S;
bits(N, S) ->
    bits(N div 2, S + N rem 2).

bits_test() ->
    3 = bits(7),
    1 = bits(8),
    3 = bits(13),
    5 = bits(174),
    pass.


% direct recursion
bits_direct(0) -> 0;
bits_direct(N) ->  (N rem 2) + bits_direct(N div 2).

bits_direct_test() ->
    3 = bits_direct(7),
    1 = bits_direct(8),
    3 = bits_direct(13),
    5 = bits_direct(174),
    pass.

