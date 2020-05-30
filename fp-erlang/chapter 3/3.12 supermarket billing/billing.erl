-module(billing).
-export([bill/1, select_test/0]).

% TODO: 
% - headline
% - 2 digit precision

bill([]) -> [];
bill([B | Bs]) -> bill([B | Bs], 0).
bill([], T) -> 
    io:format("~n"),

    TT = "Total",
    io:format("~s",[TT]),
    dots(30-length(TT)),
    io:format("~f~n",[T/100]);
bill([B | Bs], T) -> 
    {_, NAME, PRICE} = select(B),
    io:format("~s", [NAME]),
    dots(30-length(NAME)),
    io:format("~f~n", [PRICE/100]),
    TT = T + PRICE,
    bill(Bs, TT).

dots(0) -> [];
dots(N) -> 
    io:format("."),
    dots(N-1).

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

