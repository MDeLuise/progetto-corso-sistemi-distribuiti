%% @author Massimiliano De Luise
%% @author Massimo Comuzzo
%% @doc Module used for starting and stopping the application.
%% It create the starting peer and connect them as specified.
-module(peer_transactions_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("../src/config.hrl").

-define(USER_FILE, "user_functions").



%% @doc Create the specified peers and connect them.
start(_Type, _StartArgs) ->
    {ok, Starting_peers} = application:get_env(peer_transactions, starting_peers),

    Pids = lists:map(fun(P) -> peer:start(tuple_to_record(element(2, P)), [],
        to_fun_tuples(element(4, P)), to_fun_tuples(element(5, P))) end, Starting_peers),

    connectNeighbors(ids_to_pids(lists:map(fun(P) -> element(3, P) end, Starting_peers), Pids),
        Pids),

    To_return = support_server:start(lists:sublist(Pids, ?MAX_NODES_NUMBER)),
    
    {ok, To_return}.


%% @doc Stop the application.
stop(_State) -> ok.


%% @private
%% @doc Connect the created peers as user specified. 
connectNeighbors([], _) -> ok;
connectNeighbors([Pid_neighbors | Neighbors], [Pid | Pids]) ->
    peer:add_nodes(Pid, Pid_neighbors),
    connectNeighbors(Neighbors, Pids).


%% @private
%% @doc Given a list of list of id and a list of running processes Pids, it return a list of list
%% of Pid. Returned Pid in list n, position m is the mapped value of mth id of the nth list.
ids_to_pids(Ids, Pids) -> lists:map(fun(L) -> id_to_pid(L, Pids) end, Ids).


%% @private
%% @doc Given a list of id and a list of running processes Pids, it return a list of Pid. Returned
%% Pid in position n is the mapped value of nth id of the list.
id_to_pid(List, Pids) -> lists:map(fun(E) -> lists:nth(E + 1, Pids) end, List).


%% @private
%% @doc Given a list of list of string representing erlang functions, it return a list of list of
%% functions. Returned function in list n position m is the mapped value of nth string of the mth
%% list.
to_fun_tuples(List) -> lists:map(fun(T) -> to_fun_tuple(T) end, List).


%% @private
%% @doc Given a list of string representing erlang functions, it return a list of functions.
%% Returned function in position n is the mapped value of nth string of the list.
to_fun_tuple(Tuple) -> list_to_tuple(lists:map(fun(F) -> eval("fun(X) -> " ++ ?USER_FILE ++ ":"
    ++ F ++ "(X) end.") end, tuple_to_list(Tuple))).


%% @private
%% @doc Given a tuple return the variables record representing it.
tuple_to_record(Tupla) -> list_to_tuple([variables] ++ tuple_to_list(Tupla)).


%% @private
%% @doc Given a string representing an erlang function, it return the erlang function.
eval(Fun_string) ->
    {ok, Scanned, _} = erl_scan:string(Fun_string),
    {ok, Parsed} = erl_parse:parse_exprs(Scanned),
    {value, Fun, _ } = erl_eval:exprs(Parsed, []),
    Fun.