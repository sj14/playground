-module(text).
-export([get_lines/2, remove_test/0, take_test/0]).

% TODO: misunderstood the task, linelength should not based on the number of words but characters, as anyone else would assume...
% TODO: fix output

% input a string (list ) of chars
% LINELEN -> max. number of word in a line
get_lines([], _) -> [];
get_lines(X, LINELEN) ->
    Ws = string:split(X, " ", all),
    LINES = get_lines(Ws, LINELEN, [])
    %output(LINES)
.

% output([]) -> [];
% output([X | Xs]) ->
%     io:format("~s", [X]),
%     output(Xs).

get_lines([], _, R) -> R;
get_lines(X, LINELEN, R) ->
    L = [take(LINELEN, X)],
    RR = R ++ L,
    Y = remove(LINELEN, X),
    get_lines(Y, LINELEN, RR).

% copy-paste from chapter 2
take(N, X) -> take(N, X, []).
take(0, _X, R) -> R;
take(_, [], R) -> R;
take(N, [X | Xs], R) -> take(N - 1, Xs, R ++ [X]).

take_test() ->
    [] = take(2, []),
    [] = take(0, "Hello"),
    "Hel" = take(3, "Hello"),
    "Hello" = take(9, "Hello"),
    pass.

% remove first N elements
remove(_, []) -> [];
remove(0, X) -> X;
remove(N, [_X | Xs]) -> remove(N - 1, Xs).

remove_test() ->
    [] = remove(2, []),
    [] = remove(0, []),
    ["what", "is", "los"] = remove(0,
                                   ["what", "is", "los"]),
    ["los"] = remove(2, ["what", "is", "los"]),
    [] = remove(5, ["what", "is", "los"]),
    pass.
