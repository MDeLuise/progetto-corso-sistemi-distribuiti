%% @author Massimiliano De Luise
%% @author Massimo Comuzzo
%% @doc Module representing the support server of the network.
%% It mantain a not updated view of the network, and sends nodes pid to who required them.


-module(support_server_node).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_info/2, handle_cast/2]).
-export([start/0, start/1, stop/0, add_nodes/1]).

-include("debug.hrl").
-include("peer_types.hrl").




%%% ======================================================================================
%%% start and stop API and functions
%%% ======================================================================================

%% @doc Create a new support_server process, the spawned support_server knows no nodes.
-spec start() -> pid().
start() -> start([]).


%% @doc Given a list of nodes, create a new support_server knowing the nodes. The support_server
%% Pid is returned.
-spec start(Nodes:: [node_pid()]) -> pid().
start(Nodes) ->
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, Nodes, []),
    Pid.


%% @doc Stop the running support_server.
-spec stop() -> ok.
stop() ->
    gen_server:call(?MODULE, stop),
    ok.


%% @doc Needed for the gen_server behaviour, create a new support_server and set a timer for
%% checking new nodes.
 -spec init(list(node_pid())) -> {ok, list(node_pid())}.
init(Nodes) ->
    ?DBG("starting gen_server with nodes: " ++ ?FROM_NODES_TO_STRING(Nodes)),
    erlang:send_after(?CHECK_NODE_EVERY, self(), time_to_check_nodes),
    {ok, Nodes}.


%% @doc Given a list of nodes, notify the running support_server about the existence of those
%% nodes.
-spec add_nodes([node_pid()]) -> ok.
add_nodes(Nodes) -> gen_server:cast(?MODULE, {nodes_response, Nodes}).




%%% ======================================================================================
%%% callback functions for gen_server
%%% ======================================================================================

%% @doc Needed for the gen_server behaviour, when a synchronous require is send to the
%% support_server, it response and then update its status if necessary.
%
% If a node send a nodes_request via call, then respond to it sending random nodes.
-spec handle_call(atom(), node_pid(), list(node_pid())) -> {reply | noreply | stop, any(), list(node_pid())}.
handle_call(nodes_request, From, Nodes) ->
    ?DBG("requested nodes (my nodes: " ++ ?FROM_NODES_TO_STRING(Nodes) ++ ")"),
    {reply, {nodes_response, choose_random_elements(?RESPONSE_NODE_NUMBER, Nodes)},
        merge_nodes([From], Nodes)};


% If a node send me a stop message, then exit.
handle_call(stop, _From, Nodes) ->
    ?DBG("stopping (my nodes was: " ++ ?FROM_NODES_TO_STRING(Nodes) ++ ")"),
    {stop, normal, ok, Nodes}.


%% @doc Needed for the gen_server behaviour, when a asynchronous require is send to the
%% support_server, it response and then update its status if necessary.
%
% If a node send a nodes_request via cast, then respond to it sending random nodes.
-spec handle_cast(any(), list(node_pid())) -> {noreply, any(), list(node_pid())}.
handle_cast({nodes_request, Pid}, Nodes) ->
    ?DBG("requested nodes (my nodes: " ++ ?FROM_NODES_TO_STRING(Nodes) ++ ")"),
    gen_server:cast(Pid, {nodes_response, choose_random_elements(?RESPONSE_NODE_NUMBER, Nodes)}),
    {noreply, merge_nodes([Pid], Nodes)};


% If a node send nodes_response, then update the status adding the new passing nodes.
handle_cast({nodes_response, New_nodes}, Nodes) ->
    ?DBG("received new neightbors: " ++ ?FROM_NODES_TO_STRING(New_nodes) ++ " (my new nodes: " ++
        ?FROM_NODES_TO_STRING(merge_nodes(New_nodes, Nodes)) ++ ")"),
    {noreply, merge_nodes(New_nodes, Nodes)};


% If a node send an unknown message via cast, then don't do anything.
handle_cast(_Request, State) ->
    ?DBG("received unknown message (my nodes: " ++ ?FROM_NODES_TO_STRING(State) ++ ")"),
    {noreply, State}.


%% @doc Needed for the gen_server behaviour, when a simple message is send to the support_server
%% it response and then update its status if necessary.
%
%% If the message is tagget with "time_to_check_nodes" atom, then it means that it's a message
%% sent from my self, then I call send_nodes_request/1.
%% @see support_server:send_nodes_request/1.
-spec handle_info(atom, list(node_pid())) -> {noreply, list(node_pid())}.
handle_info(time_to_check_nodes, Nodes) ->
    ?DBG("timeout, it's searching nodes time! (my nodes: " ++ ?FROM_NODES_TO_STRING(Nodes) ++ ")"),
    New_nodes = send_nodes_request(Nodes),
    erlang:send_after(?CHECK_NODE_EVERY, self(), time_to_check_nodes),
    {noreply, New_nodes};


% If a node send an unknown message, then don't do anything.
handle_info(_Message, State) -> {noreply, State}.




%%% ======================================================================================
%%% private functions
%%% ======================================================================================

%% @private
%% @doc Given a number n and a list of element, choose n element from the list.
-spec choose_random_elements(Num_of_element:: integer(), List:: [any()]) -> [any()].
choose_random_elements(Num_of_element, List) when length(List) =< Num_of_element -> List;
choose_random_elements(0, _) -> [];
choose_random_elements(Num_of_element, List) ->
    Chosen = lists:nth(rand:uniform(length(List)), List),
    [Chosen] ++ choose_random_elements(Num_of_element - 1, lists:delete(Chosen, List)).


%% @private
%% @doc Send nodes_request to random nodes, it's used for mantaining a not updated view of the
%% network.
%% @see peer:handle_cast/3 for detail about the responses given to this request.
-spec send_nodes_request(Nodes:: [any()]) -> [any()].
send_nodes_request([]) -> [];
send_nodes_request(Nodes) ->
    Send_to = hd(choose_random_elements(1, Nodes)),
    gen_server:cast(Send_to, {nodes_request, self()}),
    Is_alive = pong == net_adm:ping(element(2,Send_to)),
    if 
        Is_alive ->
            Nodes;
        true ->
            send_nodes_request(lists:delete(Send_to,Nodes))
    end.


%% @private
%% @doc Given two list of nodes, it merge them. If the maximum nodes number is reached, then the
%% oldest nodes are forgotten.
-spec merge_nodes(New_neighbors:: [node_pid()], Old_neighbors:: [node_pid()]) -> [node_pid()].
merge_nodes([], Neighbors) -> Neighbors;
merge_nodes([New_neighbor | Other], Neighbors) ->
    Neighborn_already_known = lists:member(New_neighbor,Neighbors),
    if
        Neighborn_already_known ->
            merge_nodes(Other, Neighbors);

        true ->
            if length(Neighbors) < ?MAX_NODES_NUMBER ->
                    merge_nodes(Other, Neighbors ++ [New_neighbor]);
                true ->
                    merge_nodes(Other, tl(Neighbors) ++ [New_neighbor])
            end
    end.