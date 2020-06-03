% chapter 3.3

-module(assignment).
-export([run/0, member_test/0, test/0, get_test/0, drop_test/0, add_test/0]).

run() ->
    R = process(index:get_file_contents("gettysburg-address.txt"), 1, []),
    % sort alphabetically
    S = lists:sort(R),
    % filter words with less than 4 characters
    F = lists:filter(fun(X) -> {entry, W, _} = X, length(W) > 3 end, S),
    output(F).


% output word and line number
output([]) -> [];
output([X|Xs]) -> 
    {entry, WORD, LINES} = X,
    io:format("~s~w~n",[WORD, LINES]),
    output(Xs).


process([],_N,R) -> R;
process([L|Ls], N, R) -> 
    % split line into words on each space
    Ws = string:split(L, " ", all), 

    % Add words to index (Ws -> words; N -> line number; R -> result)
    RR = add(Ws, N, R),
    process(Ls, N+1, RR).

% W -> Word; N -> Line Number; R -> List with Results
add([],_N,R) -> R;
add(Ws, N, R) ->
    W = string:lowercase(hd(Ws)),

    case member(W, R) of 
        true ->
            % get current element for current line numbers (O)
            {entry, W, O} = get(W, R),          
            % drop current element from result and add current element
            % to result with old line numbers and new line number
            RR = drop(W, R) ++ [{entry, W, add_line(N, O)}]; 
        false -> 
            % add word as entry with line
            RR = R ++ [{entry, W, [{N,N}]}]
    end,
    add(tl(Ws),N,RR).

add_test() ->
    T1 = add([my_word], 1, []),
    {entry,my_word,[{1,1}]} = get(my_word, T1),

    T2 = add([my_word], 2, [{entry,my_word,[{1,1}]}]),
    {entry,my_word,[{1,2}]} = get(my_word, T2),

    T3 = add([my_word], 4, [{entry,not_my_word,[{6,6}]}, {entry,my_word,[{2,2}]}, {entry,abcd,[{3,3}]}]),
    {entry,my_word,[{2,2},{4,4}]} = get(my_word, T3),

    T4 = add([my_word], 4, [{entry,not_my_word,[{6,6}]}, {entry,my_word,[{2,3}]}, {entry,abcd,[{3,3}]}]),
    {entry,my_word,[{2,4}]} = get(my_word, T4),

    T5 = add([my_word], 11, [{entry,my_word,[{2,4}, {5,7}, {10,10}]}]),
    {entry,my_word,[{2,4}, {5,7}, {10,11}]} = get(my_word, T5),

    T6 = add([my_word], 12, [{entry,my_word,[{2,4}, {5,7}, {10,10}]}]),
    {entry,my_word,[{2,4}, {5,7}, {10,10}, {12,12}]} = get(my_word, T6),

    pass.

add_line(L, C) ->
    {FIRST, LAST} = lists:last(C),

    case LAST == L of
        true -> C; % same line number
        false -> 
            case LAST+1 == L of
                true ->  % subsequent line number
                    D = lists:droplast(C),
                    D ++ [{FIRST,L}];
                false -> % line number more than 1 line away after the previous line
                    C ++ [{L,L}]
            end
    end.

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
    add_test(),
    pass.