-module(rps).
-export([play/1,echo/1,play_two/3,rock/1,no_repeat/1,const/1,enum/1,cycle/1,rand/1,val/1,tournament/2,least_frequent/1,most_frequent/1]).

% start with e.g. rps:play(fun rps:echo/1).

%
% play one strategy against another, for N moves.
%

play_two(StrategyL,StrategyR,N) ->
    play_two(StrategyL,StrategyR,[],[],N).

% tail recursive loop for play_two/3
% 0 case computes the result of the tournament

% FOR YOU TO DEFINE
% REPLACE THE dummy DEFINITIONS

play_two(_,_,PlaysL,PlaysR,0) ->
   dummy;

play_two(StrategyL,StrategyR,PlaysL,PlaysR,N) ->
   dummy.

%
% interactively play against a strategy, provided as argument.
%

play(Strategy) ->
    io:format("Rock - paper - scissors~n"),
    io:format("Play one of rock, paper, scissors, ...~n"),
    io:format("... r, p, s, stop, followed by '.'~n"),
    play(Strategy,[]).

% tail recursive loop for play/1

play(Strategy,Moves) ->
    {ok,P} = io:read("Play: "),
    Play = expand(P),
    case Play of
	stop ->
	    io:format("Stopped~n");
	_    ->
	    Result = result(Play,Strategy(Moves)),
	    io:format("Result: ~p~n",[Result]),
	    play(Strategy,[Play|Moves])
    end.

%
% auxiliary functions
%

% transform shorthand atoms to expanded form
    
expand(r) -> rock;
expand(p) -> paper;		    
expand(s) -> scissors;
expand(X) -> X.

% result of one set of plays

result(rock,rock) -> draw;
result(rock,paper) -> lose;
result(rock,scissors) -> win;
result(paper,rock) -> win;
result(paper,paper) -> draw;
result(paper,scissors) -> lose;
result(scissors,rock) -> lose;
result(scissors,paper) -> win;
result(scissors,scissors) -> draw.

% result of a tournament

tournament(PlaysL,PlaysR) ->
    lists:sum(
      lists:map(fun outcome/1,
		lists:zipwith(fun result/2,PlaysL,PlaysR))).

outcome(win)  ->  1;
outcome(lose) -> -1;
outcome(draw) ->  0.

% transform 0, 1, 2 to rock, paper, scissors and vice versa.

enum(0) ->
    rock;
enum(1) ->
    paper;
enum(2) ->
    scissors.

val(rock) ->
    0;
val(paper) ->
    1;
val(scissors) ->
    2.

% give the play which the argument beats.

beats(rock) ->
    scissors;
beats(paper) ->
    rock;
beats(scissors) ->
    paper.

%
% strategies.
%
echo([]) ->
     paper;
echo([Last|_]) ->
    Last.

rock(_) ->
    rock.



% FOR YOU TO DEFINE
% REPLACE THE dummy DEFINITIONS

no_repeat([]) ->
    dummy;
no_repeat([X|_]) ->
    dummy.

const(Play) ->
    dummy.

cycle(Xs) ->
    enum(length(Xs) rem 3).

rand(_) ->
    enum(rand:uniform(2)).

% apply an analysis to the previous plays and choose the least frequent, 
% assuming that in the long run your opponent will play each choice equally
least_frequent(Xs) -> 
    FREQ_ROCK = count(rock, Xs),
    FREQ_PAPER = count(paper, Xs),
    FREQ_SCISSORS = count(scissors, Xs),

    io:format("rock: ~B~n", [FREQ_ROCK]),
    io:format("paper: ~B~n", [FREQ_PAPER]),
    io:format("scissors ~B~n", [FREQ_SCISSORS]),

    if ((FREQ_ROCK =< FREQ_PAPER) and (FREQ_ROCK =< FREQ_SCISSORS)) -> paper;
        ((FREQ_PAPER =< FREQ_ROCK) and (FREQ_PAPER =< FREQ_SCISSORS)) -> scissors;
        ((FREQ_SCISSORS =< FREQ_PAPER) and (FREQ_SCISSORS =< FREQ_ROCK)) -> rock;
        true -> rock
    end.

most_frequent(Xs) -> 
    FREQ_ROCK = count(rock, Xs),
    FREQ_PAPER = count(paper, Xs),
    FREQ_SCISSORS = count(scissors, Xs),

    io:format("rock: ~B~n", [FREQ_ROCK]),
    io:format("paper: ~B~n", [FREQ_PAPER]),
    io:format("scissors ~B~n", [FREQ_SCISSORS]),

    if ((FREQ_ROCK >= FREQ_PAPER) and (FREQ_ROCK >= FREQ_SCISSORS)) -> paper;
        ((FREQ_PAPER >= FREQ_ROCK) and (FREQ_PAPER >= FREQ_SCISSORS)) -> scissors;
        ((FREQ_SCISSORS >= FREQ_PAPER) and (FREQ_SCISSORS >= FREQ_ROCK)) -> rock;
        true -> rock
    end.


count(_, []) -> 0;
count(X, [X|XS]) -> 1 + count(X, XS);
count(X, [_|XS]) -> count(X, XS).
