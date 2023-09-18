-module(sum_list).
-export([sum/3]).

sum(Numbers, Caller, Ref) ->
    spawn(fun() ->
        SleepTime = 500 + rand:uniform(3000), % Add a random delay.        
        timer:sleep(SleepTime),

        CrashChance = rand:uniform(10),
        if CrashChance =< 2  -> 
            exit({error, known_crash}); % 20% crash rate
        true -> 
            ok
        end,

        Result = compute_sum(Numbers, 0),
        Caller ! {result, Ref, Result} % send message to caller with the results.
    end).

% Basic recursion adding values from a list. 
compute_sum([], Total) -> Total;
compute_sum([Head | Tail], Total) ->
    compute_sum(Tail, Head + Total).
