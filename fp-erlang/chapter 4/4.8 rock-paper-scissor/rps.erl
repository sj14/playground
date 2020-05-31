-module(rps).

-export([beat/1,
         beat_test/0,
         lose/1,
         result/2,
         result_test/0,
         tournament/2,
         tournament_test/0]).

beat(X) ->
    case X of
        rock -> paper;
        paper -> scissors;
        scissors -> rock
    end.

beat_test() ->
    paper = beat(rock),
    scissors = beat(paper),
    rock = beat(scissors),
    pass.

lose(X) ->
    case X of
        rock -> scissors;
        scissors -> paper;
        paper -> rock
    end.

result(X, X) -> draw;
result(X, Y) ->
    case lose(X) == Y of
        true -> win;
        _ -> case beat(X) == Y of 
            true -> lose;
            _ -> undefined
        end 
    end.

result_test() ->
    lose = result(rock, paper),
    win = result(rock, scissors),
    draw = result(rock, rock),
    draw = result(paper, paper),
    lose = result(paper, scissors),
    win = result(paper, rock),
    win = result(scissors, paper),
    draw = result(scissors, scissors),
    lose = result(scissors, rock),
    pass.

tournament(L, R) ->
    RR = lists:zipwith(fun (X, Y) -> result(X, Y) end,
                       L,
                       R),
    lists:foldl(fun (X, Sum) ->
                        case X of
                            win -> Sum + 1;
                            lose -> Sum - 1;
                            draw -> Sum
                        end
                end,
                0,
                RR).

tournament_test() ->
    -1 = tournament([rock, rock, paper, paper],
                    [rock, paper, scissors, rock]),
    -2 = tournament([scissors, rock, paper, paper],
                    [rock, paper, scissors, rock]),
    0 = tournament([rock, rock, scissors, paper],
                   [rock, paper, scissors, rock]),
    2 = tournament([rock, scissors, scissors, paper],
                   [rock, paper, scissors, rock]),
    pass.
