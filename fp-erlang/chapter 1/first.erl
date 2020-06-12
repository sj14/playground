-module(first).
-export([double/1,treble/1,square/1,mult/2,mult_test/0,area/3]).

-spec mult(number(), number()) -> number().
mult(X,Y) ->
    X*Y.

mult_test() -> 
    10 = mult(2,5),
    pass.
 
-spec double(number()) -> number().
double(X) ->
    mult(2,X).

treble(X) ->
    mult(3,X).

square(X) ->
    X*X.

area(A,B,C) ->
    S = (A+B+C)/2,
    math:sqrt(S*(S-A)*(S-B)*(S-C)).
