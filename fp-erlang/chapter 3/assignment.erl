% chapter 3.3

-module(assignment).

-export([member_test/0, test/0]).

test() ->
    split(index:get_file_contents("gettysburg-address.txt"), 1, []).

split([],_N,R) -> R;
split([L|Ls], N, R) -> 
    W = string:split(L, " ", all),
    RR = test(W, N, R),
    split(Ls, N+1, RR).

% L -> Line
% R -> List with Results
test([],_N,R) -> R;
test([W | Ws], N, R) -> 
    % io:format("~s~n",[W]),
    RR = add(W,N,R),
    test(Ws,N,RR).


add(W, N, R) ->
    case member(W, R) of 
        true -> R ++ [{entry, W, [N]}]; %add_line(W,L,R);
        false -> R ++ [{entry, W, [N]}]  % add word as entry with line
    end.

%check if it's the same line (continue) or add different line
% add_line(W, L, [R|Rs]) ->
%     case {entry, W, O} = get(W, R) of 
%         true -> same_line();
%         false -> 
%     end.

get(_X, []) -> [];
get(X, [{entry, W, _O} = E | Ys]) ->
    case X == W of
      true -> E;
      false -> get(X, Ys)
    end.

member(X, [{entry, W, _} | Ys]) ->
    case X == W of
      true -> true;
      false -> member(X, Ys)
    end;
member(_X, _) -> false.

member_test() ->
    false = member(anything, [{entry, somethingelse, []}]),
    false = member(anything, [{entry, somethingelse, [{4, 3}, {7, 7}]}]),
    true = member(word, [{entry, word, []}]),
    true = member(word, [{entry, word, [{4, 3}, {7, 7}]}]),
    pass.
