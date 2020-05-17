-module(shapes).
-export([circles/1]).

circles([])     -> [] ; 

circles( [X | Xs] ) ->
    case X of
	{circle,{_,_},_}=C ->
	    [ C | circles(Xs) ];
	_ ->
	    circles(Xs)
end.
