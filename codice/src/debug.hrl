%%% ===============================================================================================
%%%  Debuggin module containing useful function for debugging
%%% ===============================================================================================




%%% ===============================================================================================
%%% macros
%%% ===============================================================================================
-define(FROM_PIDS_TO_STRING(Pids), lists:foldl(fun(X,Y) -> pid_to_list(X) ++ Y end, [], Pids)).


-define(FROM_NODES_TO_STRING(Pids), from_list_to_string(Pids)).


-define(FROM_TRANS_TO_STRING(Trans), from_list_to_string(Trans)).


-define(PID_STRING, pid_to_list(self())). 


-ifdef(debug).
    -define(DBG(Str), io:format("[" ++ date_and_time() ++ ";" ++ ?MODULE_STRING
        ++ ";" ++ integer_to_list(?LINE) ++ ";" ++ ?PID_STRING ++ "] " ++ Str
        ++ "~n")).
-else.
    -define(DBG(Str), ok).

-endif.




%%% ===============================================================================================
%%% utility functions
%%% ===============================================================================================

% Return a string representing actual date and time.
date_and_time() -> my_date() ++ ";" ++ my_time().


% Return a string representing actual time.
my_time() ->
    {H, M, S} = time(),
    io_lib:format('~2..0b:~2..0b:~2..0b', [H, M, S]).


% Return a string representing actual date.
my_date() ->
    {Y, M, D} = date(),
    io_lib:format('~2..0b/~2..0b/~4..0b', [D, M, Y]).


% Transform a list into a string.
from_list_to_string(List) -> lists:flatten(io_lib:format("~w", [List])).