% chapter 3.3

-module(assignment).

-export([run/0, member_test/0, test/0, get_test/0, drop_test/0, add_test/0]).

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
    RR = add(W,N,R),
    run(Ws,N,RR).


add(W, N, R) ->
    case member(W, R) of 
        true ->
            {entry, W, O} = get(W, R),      % get current element for current line numbers (O)
            RR = drop(W, R),                % drop current element from result
            RR ++ [{entry, W, add_line(N, O)}];   % add current element to result with old line numbers an new line number
        false -> R ++ [{entry, W, [{N}]}]     % add word as entry with line
    end.

add_test() ->
    T1 = add(my_word, 1, []),
    {entry,my_word,[{1}]} = get(my_word, T1),

    T2 = add(my_word, 2, [{entry,my_word,[{1}]}]),
    {entry,my_word,[{1,2}]} = get(my_word, T2),

    T3 = add(my_word, 4, [{entry,not_my_word,[{6}]}, {entry,my_word,[{2}]}, {entry,abcd,[{3}]}]),
    {entry,my_word,[{2,4}]} = get(my_word, T3),

    T4 = add(my_word, 4, [{entry,not_my_word,[{6}]}, {entry,my_word,[{2,3}]}, {entry,abcd,[{3}]}]),
    {entry,my_word,[{2,4}]} = get(my_word, T4),

    pass.

add_line(L, C) ->
    case tuple_size(hd(C)) == 2 of
        true ->
            {_, last} = hd(C),
            [erlang:append_element(last,L)];
        false -> [erlang:append_element(hd(C),L)]
    end

    % case hd(C) > 0 of % because idk how to do `hd(c) == {_}`
    %     true -> 
    %         % has only one line, no range so far
    %         [erlang:append_element(hd(C),L)];
    %     false ->
    %         % should be a range now, e.g. {1,5}
    %         {first, _} = C,
    %         {first, L}
    % end

    % case is_list(c) of
    %     true -> 

    %         % we already have several lines
    %         case C + 1 == L of
    %             true -> 
    %                 % new line follows last line
    %                 {tl(C), L};
    %             false -> 
    %                 % new line more than 1 line after old line
    %                 {C, L}
    %         end;

    %     false -> 
    %         % only one line
    %         [{C,L}]
    % end
.


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