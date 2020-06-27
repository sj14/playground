-module(palindromes).
-export([server/0]).

rem_punct(String) -> lists:filter(fun (Ch) ->
                                      not(lists:member(Ch,"\"\'\t\n "))
                                    end,
                                    String).

to_small(String) -> lists:map(fun(Ch) ->
                                  case ($A =< Ch andalso Ch =< $Z) of
                                      true -> Ch+32;
                                      false -> Ch
                                   end
                                 end,
                                 String).

palindrome_check(String) ->
    Normalise = to_small(rem_punct(String)),
    lists:reverse(Normalise) == Normalise.

% c(palindromes).
% R = fun() -> receive X -> X after 100 -> io:format("timeout~n") end end. 
% Pid = spawn(palindromes, server, []).
% Pid ! {check, self(), "otto"}.
% R().
% Pid ! {check, self(), "frank"}.
% R().
% Pid ! {self(), stop}.
% R().
server() -> 
    receive
        {Pid, stop} ->
            Pid ! "stopped";
        {check, Pid, String} ->
            case palindrome_check(String) of
                 true -> 
                    Pid ! {result, String ++ " is a palindrome."};
                 _ -> 
                    Pid ! {result, String ++ " is not a palindrome."}
            end,
            server()
    end.