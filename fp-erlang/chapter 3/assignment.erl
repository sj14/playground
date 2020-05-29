% chapter 3.3

-module(assignment).

-export([member_test/0, test/0, get_test/0, drop_test/0, add_test/0]).

run() ->
    split(index:get_file_contents("gettysburg-address.txt"), 1, []).

split([],_N,R) -> R;
split([L|Ls], N, R) -> 
    W = string:split(L, " ", all),
    RR = run(W, N, R),
    split(Ls, N+1, RR).

% L -> Line
% R -> List with Results
run([],_N,R) -> R;
run([W | Ws], N, R) -> 
    % io:format("~s~n",[W]),
    RR = add(W,N,R),
    run(Ws,N,RR).


add(W, N, R) ->
    case member(W, R) of 
        true ->
            {entry, W, O} = get(W, R),      % get current element for current line numbers (O)
            RR = drop(W, R),                % drop current element from result
            RR ++ [{entry, W, O ++ [N]}];   % add current element to result with old line numbers an new line number
        false -> R ++ [{entry, W, [N]}]     % add word as entry with line
    end.

add_test() ->
    R = add(my_word, 1, [{1}]),
    {entry,my_word,[{1}]} = get(my_word, R),
    pass.

%check if it's the same line (continue) or add different line
% add_line(W, L, [R|Rs]) ->
%     case {entry, W, O} = get(W, R) of 
%         true -> same_line();
%         false -> 
%     end.

% drop word X from list of entries R.
drop(_X, []) -> [];
drop(X, R) -> drop(X, R, []).

drop(_X, [], R) -> R;
drop(X, [{entry, W, _O} = E | Ys], R) ->
    case X == W of
      true -> drop(X, Ys, R);
      false -> drop(X, Ys, R ++ [E])
    end.

drop_test() ->
    [] = drop(the_word, []),
    [] = drop(the_word, [{entry, the_word, [whatever]}]),
    [{entry, not_the_word, [whatever]}] = drop(the_word, [{entry, not_the_word, [whatever]}]),
    [{entry, not_the_word, [whatever]}] = drop(the_word, [{entry, the_word, [whatever]}, {entry, not_the_word, [whatever]}]),
    [{entry, not_the_word, [whatever]}] = drop(the_word, [{entry, not_the_word, [whatever]}, {entry, the_word, [whatever]}]),
    [{entry, not_the_word, [whatever]},{entry, not_the_word, [whatever]}] = drop(the_word, [{entry, not_the_word, [whatever]}, {entry, not_the_word, [whatever]}]),
    [{entry, not_the_word, [whatever]}, {entry, not_the_word, [whatever]}] = drop(the_word, [{entry, not_the_word, [whatever]}, {entry, the_word, [whatever]}, {entry, not_the_word, [whatever]}]),
    pass.

get(_X, []) -> [];
get(X, [{entry, W, _O} = E | Ys]) ->
    case X == W of
      true -> E;
      false -> get(X, Ys)
    end.

get_test() ->
    [] = get(the_word, []),
    [] = get(the_word, [{entry, not_the_word, [whatever]}]),
    {entry,the_word,[whatever]} = get(the_word, [{entry, the_word, [whatever]}]),
    {entry,the_word,[whatever]} = get(the_word, [{entry, not_the_word, [whatever]}, {entry, the_word, [whatever]}, {entry, not_the_word, [whatever]}]),
    pass.


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

test() ->
    member_test(),
    get_test(),
    drop_test(),
    pass.