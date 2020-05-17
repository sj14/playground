-module(where2).
-export([nub/1,bun/1]).

% keep first occurrences
%
% nub([2,4,1,3,3,1]) = [2,4,1,3]

% keep last occurrences
%
% bun([2,4,1,3,3,1]) = [2,4,3,1]

nub([]) ->
    [];
nub([X|Xs]) ->
    [X|nub(removeAll(X,Xs))].

removeAll(_,[]) ->
     [];
removeAll(X,[X|Xs]) ->
    removeAll(X,Xs);
removeAll(X,[Y|Xs]) ->
    [Y | removeAll(X,Xs) ].

bun([]) ->
    [];
bun([X|Xs]) ->
    case member(X,Xs) of
	true ->
	    bun(Xs);
	false ->
	    [X|bun(Xs)]
    end.

member(_,[]) ->
    false;
member(X,[X|_Xs]) ->
    true;
member(X,[_Y|Xs]) ->
    member(X,Xs).
