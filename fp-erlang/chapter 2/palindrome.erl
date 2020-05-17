% chapter 2.24

-module(palindrome).
-export([palindrome/1, palindrome_test/0]).

palindrome([]) -> true;
palindrome([X|Xs]) ->  
    case string:to_lower(X) == string:to_lower(lists:last(Xs)) of 
        true -> palindrome(lists:droplast(Xs)); 
        false -> false
    end.

palindrome_test() ->
    true = palindrome("otto"),
    true = palindrome("Otto"),
    false = palindrome("Madam I\'m Adam"), % sorry, my definition of palindrome is different ;)
    false = palindrome("hello"),
    pass.