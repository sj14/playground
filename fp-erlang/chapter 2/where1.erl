-module(where1).
-export([take/2]).

take(0,_Xs) ->
    [];
take(_N,[]) ->
    [];
take(N,[X|Xs]) when N>0 ->
    [X|take(N-1,Xs)].
