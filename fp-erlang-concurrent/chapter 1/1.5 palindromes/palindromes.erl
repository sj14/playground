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
% spawn(palindromes, server, []).
% <0.87.0> ! {check, self(), "otto"}.
% <0.87.0> ! {check, self(), "frank"}.
server() -> 
    receive
        {Pid, stop} ->
            Pid ! "stopped";
        {check, Pid, String} ->
            io:format("got message ~s~n", [String]),
            case palindrome_check(String) of
                 true -> 
                    Pid ! {result, io:format("~s is a palindrome~n", [String])};
                 false -> 
                    Pid ! {result, io:format("~s is not a palindrome~n", [String])}
            end,
            server()
    end.