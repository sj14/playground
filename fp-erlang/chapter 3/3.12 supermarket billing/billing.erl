-module(billing).
-export([bill/1, select_test/0]).

% TODO: 
% - price/100
% - formatting

bill([]) -> [];
bill([B | Bs]) -> bill([B | Bs], 0).
bill([], T) -> 
    io:format("Total: ~B~n",[T]);
bill([B | Bs], T) -> 
    {_, NAME, PRICE} = select(B),
    io:format("~s ~B~n", [NAME, PRICE]),
    TT = T + PRICE,
    bill(Bs, TT).


select(B) ->
    DB = [{4719, "Fish Fingers", 121},
          {5643, "Nappies", 1010},
          {3814, "Orange Jelly", 56},
          {1111, "Hula Hoops", 21},
          {1112, "Hula Hoops {Giant}", 133},
          {1234, "Dry Sherry, 1lt", 540}],
    get(B, DB).


select_test() -> 
    {4719, "Fish Fingers", 121} = select(4719),
    {1234, "Dry Sherry, 1lt", 540} = select(1234),
    pass.


get(B, [D | Ds]) ->
    {C, _, _} = D,
    case C == B of
        true -> D;
        _ -> get(B, Ds)
    end.

