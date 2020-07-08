%% @author Massimiliano De Luise
%% @author Massimo Comuzzo
%% @doc Module representing the peer process of the network.
-module(peer_node).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_info/2, handle_cast/2]).
-export([start/1, start/2, start/4, start/5, stop/1, add_nodes/2, send/3, send/4]).
-export([add_trans/6]). % here just for test

-include("debug.hrl").
-include("peer_types.hrl").

-record(transaction, {id               :: trans_id_node(),
                      state = pending  :: trans_state(),
                      seq_number       :: trans_seq_node(),
                      guard            :: guard(),
                      command          :: command()
}).

-record(tran_aux_info, {asked_this = []                :: list(node_pid()),
                        max_prop_seq                   :: trans_seq_node(),
                        waiting_response = dict:new()  :: dict:dict(node_pid(), atom()),
                        current_seq                    :: trans_seq_node()
}).

-record(state, {nodes = []                   :: list(node_pid()),
                transactions = orddict:new() :: orddict:orddict(trans_seq_node(), #transaction{}),
                vars                         :: #variables{},
                sending_tran = nothing       :: {just, #transaction{}} | nothing,
                monitoring = []              :: list({node_pid(), #transaction{}}),
                monitored  = []              :: list(node_pid()),
                candidates = dict:new()      :: dict:dict(node_pid(), atom()),
                tran_infos = dict:new()      :: dict:dict(trans_id_node(), #tran_aux_info{}),
                init = []                    :: list({guard(), guard(), command()}),
                loop = []                    :: list({guard(), guard(), command()}),
                executed = 0                 :: integer(),
                supports = []                :: list(node_pid())
}).




%%% ======================================================================================
%%%  API to manage peer
%%% ======================================================================================

%% @doc Given a list of element, create a new peer process, the spawned peer knows no nodes and
%% have state equals to the given list.
-spec start(Vars:: #variables{}) -> pid().
start(Vars) -> start(Vars, []).


%% @doc Given a list of nodes, create a new peer process knowing the nodes. The peer Pid is
%% returned.
-spec start(Vars:: #variables{}, Nodes:: list(node_pid())) -> pid().
start(Vars, Nodes) ->
    process_flag(trap_exit, true), % here so we can kill the spawned peer froms shell without
                                   % being killed even our self
    {ok, Pid} = gen_server:start({local, peer}, ?MODULE, {#state{vars = Vars}, Nodes}, []),
    Pid.


-spec start(Vars:: #variables{}, Nodes:: list(node_pid()), Init:: [({guard(), guard(), command()}
    | { guard(), command()})], Loop:: [{guard(), guard(), command()} | {guard(), command()}])
    -> pid().
start(Vars, Nodes, Init, Loop) ->
    process_flag(trap_exit, true), % here so we can kill the spawned peer froms shell without
                                   % being killed even our self
    {ok, Pid} = gen_server:start({local, peer}, ?MODULE, {#state{
            vars = Vars,
            init = to_guarded(Init),
            loop = to_guarded(Loop)
        }, Nodes}, []),
    Pid.


-spec start(Vars:: #variables{}, Nodes:: list(node_pid()), Init:: [({guard(), guard(), command()} |
    { guard(), command()})], Loop:: [{guard(), guard(), command()} | {guard(), command()}],
        Supps:: [node_pid()]) -> pid().
start(Vars, Nodes, Init, Loop, Supps) ->
    process_flag(trap_exit, true), % here so we can kill the spawned peer froms shell without
                                   % being killed even our self
    {ok, Pid} = gen_server:start({local, peer}, ?MODULE, {#state{
            vars = Vars,
            init = to_guarded(Init),
            loop = to_guarded(Loop),
            supports = Supps
        }, Nodes}, []),
    Pid.


-spec stop(node_pid()) -> ok.
stop(Pid) -> gen_server:call(Pid, stop).


-spec add_nodes(Pid:: node_pid(), Nodes:: list(node_pid())) -> ok.
add_nodes(Pid, Nodes) -> gen_server:cast(Pid, {nodes_response, Nodes}).


-spec send(Pid:: node_pid(), Guard:: guard(), Command:: command()) -> ok.
send(Pid, Guard, Command) -> gen_server:cast(Pid, {create_transaction, Guard, Command}).

-spec send(Pid:: node_pid(), Avail:: guard(), Guard:: guard(), Command:: command()) -> ok.
send(Pid, Avail, Guard, Command) -> gen_server:cast(Pid, {init_append, Avail, Guard, Command}).


% here just for test
-spec add_trans(Pid:: node_pid(), Id:: trans_id_node(), Seq:: trans_seq_node(),
    Status:: trans_state(), Guard:: guard(), Command:: command()) -> ok.
add_trans(Pid, Id, Seq, Status, Guard, Command) ->
    gen_server:cast(Pid, {force_transaction, Id, Seq, Status, Guard, Command}).


init({State, Nodes}) ->
    ?DBG("starting gen_server (my nodes: " ++ ?FROM_NODES_TO_STRING(State#state.nodes) ++ ")"),
    process_flag(trap_exit, true),
    erlang:send_after(?CHECK_NEIGHBOR_EVERY, self(), check_neighbors_time),
    erlang:send_after(?GOSSIP_EVERY, self(), gossip_time),
    erlang:send_after(?SEND_EVERY, self(), send_time),
    {ok, State#state{nodes = merge_neighbors(Nodes, [])}}.




%%% ======================================================================================
%%% callback functions for gen_server
%%% ======================================================================================
handle_call(stop, _Reason, State) ->
    ?DBG("stopping (my nodes was: " ++ ?FROM_NODES_TO_STRING(State#state.nodes) ++ ")"),
    {stop, normal, ok, State}.


handle_cast({nodes_request, Pid}, State) ->
    ?DBG("received nodes_request (my nodes: " ++ ?FROM_NODES_TO_STRING(State#state.nodes) ++ ")"),
    gen_server:cast(Pid, {nodes_response, State#state.nodes}),
    {noreply, State};


handle_cast({nodes_response, New_neighbors}, State) ->
    ?DBG("received nodes_response (my new nodes: " ++
        ?FROM_NODES_TO_STRING(merge_neighbors(New_neighbors, State#state.nodes)) ++ ")"),
    {noreply, State#state{nodes = merge_neighbors(New_neighbors, State#state.nodes)}};


handle_cast({link_require, Pid}, State) ->
    Node_already_known = lists:member(Pid, State#state.nodes),
    New_neighbors =
        if
            Node_already_known ->
                State#state.nodes;
            
            true ->
                monitor_node(element(2, Pid), true),
                State#state.nodes ++ [Pid]
        end,
    ?DBG("received link_require from " ++ ?FROM_NODES_TO_STRING([Pid]) ++ " (my new nodes: " ++
        ?FROM_NODES_TO_STRING(New_neighbors) ++ ")"),
    {noreply, State#state{nodes = New_neighbors}};


handle_cast({gossip_me, Last_trans, Pid}, State) ->
    ?DBG("received gossip_me from " ++ ?FROM_NODES_TO_STRING([Pid]) ++
        "(my trans: " ++ ?FROM_TRANS_TO_STRING(trans_to_list(State#state.transactions)) ++ ")"),
    Trans_to_send = lists:filter(fun(T) ->
        T#transaction.seq_number > Last_trans#transaction.seq_number end, frozen_trans(State)),
    if
        length(Trans_to_send) > 0 ->
            ?DBG("sent gossip_you to " ++ ?FROM_NODES_TO_STRING([Pid])),
            gen_server:cast(Pid, {gossip_you, Trans_to_send});
        true ->
            ?DBG("can't gossip_you"),
            ok
    end,
    {noreply, State};


handle_cast({gossip_you, Trans}, State) ->
    {New_transactions, New_infos} = update_trans(Trans, State#state.transactions,
        State#state.tran_infos),
    Updated_state = execute_ready(State#state{
            transactions = New_transactions,
            tran_infos = New_infos
        }),
    ?DBG("received gossip_you (my new trans: " ++
        ?FROM_TRANS_TO_STRING(trans_to_list(New_transactions)) ++ ")"),
    {noreply, Updated_state};


handle_cast({let_me_gossip, Last_pending, Pid}, State) ->
    ?DBG("receive let_me_gossip from " ++ ?FROM_NODES_TO_STRING([Pid])),
    To_send = lists:filter(fun(T) ->
        T#transaction.id >= Last_pending#transaction.id end, frozen_trans(State)),
    if
        length(To_send) > 0 ->
            ?DBG("gossip_you sent to " ++ ?FROM_NODES_TO_STRING([Pid]) ++ ", sent: " ++
                ?FROM_TRANS_TO_STRING([To_send])),
            gen_server:cast(Pid, {gossip_you, To_send});
        true ->
            ?DBG("can't gossip_you")
    end,
    {noreply, State};


% here just for test
handle_cast({force_transaction, Id, Seq, Status, Guard, Command}, State) ->
    Tran = #transaction{
                         id = Id,
                         state = Status,
                         seq_number = Seq,
                         guard = Guard,
                         command = Command
            },
    if
        Status == confirmed ->
            erlang:send_after(?GOSSIP_EVERY, self(), gossip_time);
        true ->
            ok
    end,
    New_transactions = orddict:store(Seq, Tran, State#state.transactions),
    New_infos = dict:store(Id, #tran_aux_info{
            asked_this = [],
            max_prop_seq = Seq,
            waiting_response = dict:new(),
            current_seq = Seq
        }, State#state.tran_infos),
    {noreply, State#state{transactions = New_transactions, tran_infos = New_infos}};


handle_cast({init_append, Avail, Guard, Cmd}, State) ->
    {noreply, State#state{init = State#state.init ++ [{Avail, Guard, Cmd}]}};


handle_cast({create_transaction, Guard, Cmd}, State) ->
    My_seq = build_proposed_seq(State#state.transactions),
    Created_trans = #transaction{
                                 id = My_seq,
                                 seq_number = My_seq,
                                 guard = Guard,
                                 command = Cmd
    },
    ?DBG("created trans: " ++ ?FROM_TRANS_TO_STRING([Created_trans])),
    erlang:send_after(0, self(), search_monitors_time),
    {noreply, State#state{
            transactions = orddict:store(Created_trans#transaction.seq_number,
                    Created_trans, State#state.transactions),
            sending_tran = {just, Created_trans}
        }
    };


handle_cast({monitor_request, Pid, Trans}, State) ->
    ?DBG("received monitor_request from " ++ ?FROM_NODES_TO_STRING([Pid])),
    gen_server:cast(Pid, {monitor_response, yes, {peer, node()}}),
    New_monitoring = State#state.monitoring ++ [{Pid, Trans}], % if Pid crash, abort Trans
    {noreply, State#state{monitoring = New_monitoring}};


handle_cast({monitor_response, Outcome, Pid}, State) ->
    ?DBG("received monitor_response from " ++ ?FROM_NODES_TO_STRING([Pid])),
    New_monitored =
        case Outcome of
            yes ->
                [Pid|State#state.monitored]; %State#state.monitored ++ [Pid], for testing
            no ->
                State#state.monitored
        end,
    New_candidates = dict:erase(Pid, State#state.candidates),
    Num_waiting = dict:size(New_candidates),
    {just, Tran} = State#state.sending_tran,
    Seq = Tran#transaction.seq_number,
    New_infos =
        if
            Num_waiting == 0 ->
                if
                    length(New_monitored) >= ?MONITORS_NUM ->
                        send_to_nodes({propose_trans, {peer, node()}, Tran}, State#state.nodes),
                        dict:store(Tran#transaction.id, #tran_aux_info{
                                max_prop_seq = Seq,
                                waiting_response = build_pid_dict(State#state.nodes),
                                current_seq = Seq
                        }, State#state.tran_infos);
                    true ->
                        erlang:send_after(?CHECK_NEIGHBOR_EVERY, self(), search_monitors_time),
                        State#state.tran_infos
                end;
            true ->
                State#state.tran_infos
        end,
    {noreply, State#state{
            monitored = New_monitored,
            candidates = New_candidates,
            tran_infos = New_infos
        }
    };


handle_cast({propose_trans, Pid, Tran}, State) ->
    ?DBG("received propose_trans from " ++ ?FROM_NODES_TO_STRING([Pid]) ++ " for tran: " ++
        ?FROM_TRANS_TO_STRING([Tran])),
    Tran_known = dict:find(Tran#transaction.id, State#state.tran_infos),
    ?DBG("propose_trans: " ++ ?FROM_NODES_TO_STRING([Pid]) ++ ", " ++
        ?FROM_TRANS_TO_STRING([Tran])),
    {New_transactions, New_infos} =
        case Tran_known of
            {ok, Info} ->
                New_seq = max(Info#tran_aux_info.max_prop_seq, Tran#transaction.seq_number),
                Asker = [Pid|Info#tran_aux_info.asked_this],
                Waiting = dict:erase(Pid, Info#tran_aux_info.waiting_response),
                ?DBG("prop is old: " ++ lists:flatten(io_lib:format("~w",[tuple_to_list(
                    Info#tran_aux_info.max_prop_seq)])) ++ ", " ++  lists:flatten(io_lib:format(
                    "~w",[tuple_to_list(Tran#transaction.seq_number)])) ++ ", " ++
                    lists:flatten(io_lib:format("~w",[tuple_to_list(New_seq)]))),
                Empty_waiting = dict:is_empty(Waiting),
                New_askers =
                    if
                        Empty_waiting ->
                            send_to_nodes({trans_response, Tran#transaction.id,
                                {peer, node()}, New_seq}, Asker),
                            [];
                        true ->
                            Asker
                    end,
                {State#state.transactions, dict:store(Tran#transaction.id, Info#tran_aux_info{
                        asked_this  = New_askers,
                        max_prop_seq = New_seq,
                        waiting_response = Waiting
                    }, State#state.tran_infos)};
            error ->
                My_seq = build_proposed_seq(State#state.transactions),
                New_seq = max(My_seq, Tran#transaction.seq_number),
                ?DBG("prop is new: " ++ lists:flatten(io_lib:format("~w",[tuple_to_list(My_seq)]))
                    ++ ", " ++ lists:flatten(io_lib:format("~w",[tuple_to_list(
                    Tran#transaction.seq_number)])) ++ ", " ++ lists:flatten(io_lib:format("~w",
                    [tuple_to_list(New_seq)]))),
                if
                    length(State#state.nodes) == 1 ->
                        gen_server:cast(Pid,
                            {trans_response, Tran#transaction.id, {peer, node()}, New_seq});
                    true ->
                        send_to_nodes({propose_trans, {peer, node()},
                            Tran#transaction{seq_number = My_seq}},
                    lists:delete(Pid, State#state.nodes))
                end,
                {orddict:store(My_seq, Tran#transaction{seq_number = My_seq},
                    State#state.transactions),
                    dict:store(Tran#transaction.id, #tran_aux_info{
                            asked_this = [Pid],
                            max_prop_seq = New_seq,
                            waiting_response = dict:erase(Pid, build_pid_dict(State#state.nodes)),
                            current_seq = My_seq
                        }, State#state.tran_infos)}
        end,
    {noreply, State#state{transactions = New_transactions, tran_infos = New_infos}};


handle_cast({trans_response, TransID, Pid, Seq}, State) ->
    Old_info = dict:fetch(TransID, State#state.tran_infos),
    Old_seq = Old_info#tran_aux_info.max_prop_seq,
    New_seq = max(Old_seq, Seq),
    Old_tran = orddict:fetch(Old_info#tran_aux_info.current_seq, State#state.transactions),
    Required = lists:delete(Pid, Old_info#tran_aux_info.asked_this),
    New_info = Old_info#tran_aux_info{
            max_prop_seq = New_seq,
            waiting_response = dict:erase(Pid, Old_info#tran_aux_info.waiting_response),
            asked_this = Required
        },
    ?DBG("trans_response: " ++ ?FROM_NODES_TO_STRING([Pid]) ++ " for tran: " ++
        lists:flatten(io_lib:format("~w",[tuple_to_list(TransID)])) ++ ", seq received: " ++
            lists:flatten(io_lib:format("~w",[tuple_to_list(Seq)])) ++ ", " ++ lists:flatten(
                io_lib:format("~w",[tuple_to_list(Old_seq)])) ++ ", " ++ lists:flatten(
                io_lib:format("~w",[tuple_to_list(New_seq)]))),
    Empty_waiting = dict:is_empty(New_info#tran_aux_info.waiting_response),
    New_state =
        if
            Old_tran#transaction.state == confirmed ->
                State;

            Empty_waiting -> % must send back
                if
                    element(2, TransID) /= node() -> % not my transaction
                        send_to_nodes({trans_response, TransID, {peer,node()}, New_seq}, Required),
                        State#state{
                                tran_infos = dict:store(TransID,
                                    New_info#tran_aux_info{asked_this = []},
                                    State#state.tran_infos)
                            };
                    
                    true -> % if trans is mine and all neighbors answers
                        
                        if
                            length(State#state.monitored) == 1 ->
                                Confirmed_tran = Old_tran#transaction{
                                        state = confirmed,
                                        seq_number = New_seq
                                    },
                                gen_server:cast(hd(State#state.monitored),
                                    {trans_confirm, Confirmed_tran, {peer,node()}}),
                                erlang:send_after(?SEND_EVERY, self(), send_time),
                                execute_ready(State#state{
                                        transactions = orddict:store(
                                            New_seq, Confirmed_tran,
                                                orddict:erase(Old_info#tran_aux_info.current_seq,
                                                    State#state.transactions)),
                                        tran_infos = dict:store(TransID,
                                            New_info#tran_aux_info{current_seq = New_seq},
                                                State#state.tran_infos),
                                        sending_tran = nothing,
                                        monitored = []
                                    });
                            length(State#state.monitored) == 0 ->
                                Confirmed_tran = Old_tran#transaction{
                                        state = confirmed,
                                        seq_number = New_seq
                                    },
                                erlang:send_after(?SEND_EVERY, self(), send_time),
                                execute_ready(State#state{
                                        transactions = orddict:store(
                                            New_seq, Confirmed_tran,
                                                orddict:erase(Old_info#tran_aux_info.current_seq,
                                                    State#state.transactions)),
                                        tran_infos = dict:store(TransID,
                                            New_info#tran_aux_info{current_seq = New_seq},
                                                State#state.tran_infos),
                                        sending_tran = nothing,
                                        monitored = []
                                    });
                            true ->
                                send_to_nodes({trans_done, {peer,node()}}, tl(State#state.monitored)),
                                State#state{tran_infos = dict:store(TransID, New_info,
                                            State#state.tran_infos)}
                        end
                end;
            true ->
                State#state{tran_infos = dict:store(TransID, New_info, State#state.tran_infos)}
        end,
    {noreply, New_state};


handle_cast({trans_confirm, Tran, Pid}, State) ->
    ?DBG("received trans_confirm from " ++ ?FROM_NODES_TO_STRING([Pid]) ++ " for trans: " ++
        ?FROM_TRANS_TO_STRING([Tran])),
    Info = dict:fetch(Tran#transaction.id, State#state.tran_infos),
    Old_seq = Info#tran_aux_info.current_seq,
    New_seq = Tran#transaction.seq_number,
    {noreply, execute_ready(State#state{
            transactions = orddict:store(New_seq, Tran, orddict:erase(Old_seq,
                State#state.transactions)),
            tran_infos = dict:store(Tran#transaction.id, Info#tran_aux_info{current_seq = New_seq},
                    State#state.tran_infos),
            monitoring = lists:filter(fun(M) ->
                element(1, M) /= Pid end, State#state.monitoring)
        }
    )};


handle_cast({trans_ack, Pid}, State) ->
    ?DBG("received trans_ack from " ++ ?FROM_NODES_TO_STRING([Pid])),
    if
        length(State#state.monitored) == 2 -> % 2 because last monitor + who responds me now
            Id = (element(2, State#state.sending_tran))#transaction.id,
            Info = dict:fetch(Id, State#state.tran_infos),
            Old_seq = Info#tran_aux_info.current_seq,
            New_seq = Info#tran_aux_info.max_prop_seq,
            Updated_tran = orddict:fetch(Old_seq, State#state.transactions),
            Confirmed_tran = Updated_tran#transaction{
                    state = confirmed,
                    seq_number = New_seq
                },
            gen_server:cast(hd(lists:filter(fun(E) ->
                E /= Pid end, State#state.monitored)),
                    {trans_confirm, Confirmed_tran, {peer,node()}}),
            erlang:send_after(?SEND_EVERY, self(), send_time),
            {noreply, execute_ready(State#state{
                    transactions = orddict:store(New_seq, Confirmed_tran, orddict:erase(Old_seq,
                        State#state.transactions)),
                    tran_infos = dict:store(Id, Info#tran_aux_info{current_seq = New_seq},
                        State#state.tran_infos),
                    sending_tran = nothing,
                    monitored = []
                }
            )};
        
        true ->
            {noreply, State#state{monitored = lists:delete(Pid, State#state.monitored)}}
    end;


handle_cast({trans_done, Pid}, State) ->
    ?DBG("received trans_done from " ++ ?FROM_NODES_TO_STRING([Pid])),
    gen_server:cast(Pid, {trans_ack, {peer,node()}}),
    {noreply, State#state{
            monitoring = lists:filter(fun(M) ->
                element(1, M) /= Pid end, State#state.monitoring)
        }
    };



handle_cast(_Request, State) ->
    ?DBG("received unknown message (my nodes: " ++ ?FROM_NODES_TO_STRING(State#state.nodes) ++
        ")"),
    {noreply, State}.


handle_info(check_neighbors_time, State) ->
    if
        length(State#state.nodes) < ?MIN_NEIGHBORS ->
            ?DBG("timeout, it's searching neighbors time!(my nodes: " ++
                ?FROM_NODES_TO_STRING(State#state.nodes) ++ ")"),
            send_to_nodes({nodes_request, {peer, node()}}, State#state.nodes
                ++ State#state.supports),
            erlang:send_after(?CHECK_NEIGHBOR_EVERY, self(), check_neighbors_time);
        true ->
            ok
    end,
    {noreply, State};

handle_info(send_time, State) ->
    Avail_init = lists:dropwhile(fun(S) -> not_avail(S, State#state.vars) end, State#state.init),
    Avail_loop = lists:dropwhile(fun(S) -> not_avail(S, State#state.vars) end, State#state.loop),
    {Init, Loop} =
        if
            length(Avail_init) > 0 ->
                {_Avail, Guard, Command} = hd(Avail_init),
                send({peer,node()}, Guard, Command),
                {tl(Avail_init), State#state.loop};
            length(Avail_loop) > 0 ->
                {_Avail, Guard, Command} = hd(Avail_loop),
                send({peer,node()}, Guard, Command),
                Num_drop = length(State#state.loop) - length(Avail_loop),
                Dropped = lists:sublist(State#state.loop, Num_drop),
                Tail = Dropped ++ [hd(Avail_loop)],
                {[], (tl(Avail_loop)) ++ Tail};
            true ->
                erlang:send_after(?SEND_EVERY, self(), send_time),
                {[], State#state.loop}
        end,
    {noreply, State#state{init = Init, loop = Loop}};


handle_info(gossip_time, State) ->
    ?DBG("timeout, it's gossip time! (my trans: " ++
        ?FROM_TRANS_TO_STRING(trans_to_list(State#state.transactions)) ++ ")"),
    Size = orddict:size(State#state.transactions),
    if
        Size > 0 ->
            Confirmed_trans = frozen_trans(State),
            if
                length(Confirmed_trans) > 0 ->
                    Last_tran = lists:last(Confirmed_trans),
                    lists:foreach(fun(P) ->
                        gen_server:cast(P, {gossip_me, Last_tran, {peer, node()}}),
                            ?DBG("gossip_me sent to " ++ atom_to_list(element(2,P))) end,
                                State#state.nodes);
                true ->
                    First_pending = hd(trans_to_list(State#state.transactions)),
                    lists:foreach(fun(P) ->
                        gen_server:cast(P, {let_me_gossip, First_pending, {peer, node()}}),
                        ?DBG("let_me_gossip sent to " ++ atom_to_list(element(2,P))) end,
                            State#state.nodes)
            end;
        true ->
            ?DBG("can't gossip, I know 0 transactions"),
            ok
    end,
    erlang:send_after(?GOSSIP_EVERY, self(), gossip_time),
    {noreply, State};


handle_info({nodedown, Node}, State) ->
    ?DBG(atom_to_list(Node) ++ " interrupted link with me"),
    Pid = {peer, Node},
    Was_monitored_by_me = find_in_list(fun(M) -> element(1, M) == Pid end, State#state.monitoring),
    {New_monitoring, New_transactions} = 
        case Was_monitored_by_me of
            
            {value, {Pid, Trans}} ->
                {lists:delete({Pid, Trans}, State#state.monitoring),
                    orddict:map(fun(_K, T) ->
                        Is_to_abort = T#transaction.id == Trans#transaction.id,
                            if
                                Is_to_abort ->
                                    T#transaction{state = aborted};
                                true ->
                                    T
                            end
                        end, State#state.transactions)};
            false ->
                {State#state.monitoring, State#state.transactions}
        end,
    
    if
        (length(State#state.nodes) - 1) < ?MIN_NEIGHBORS ->
            ?DBG("it's checking neighbors time!"),
            send_to_nodes({nodes_request, {peer,node()}}, lists:delete(Pid, State#state.nodes)),
            erlang:send_after(?CHECK_NEIGHBOR_EVERY, self(), check_neighbors_time);
        true ->
            ok
    end,
    reap_crashed_responses(State#state.tran_infos, Pid),
    Was_candidate = dict:is_key(Pid, State#state.candidates),
    if
        Was_candidate ->
            gen_server:cast(self(), {monitor_response, no, Pid});
        true ->
            ok
    end,
    {noreply, State#state{
            nodes = lists:delete(Pid, State#state.nodes),
            monitoring = New_monitoring,
            transactions = New_transactions
        }
    };


handle_info(search_monitors_time, State) ->
    {just, Created_trans} = State#state.sending_tran,
    Candidates =
        if
            length(State#state.nodes) >= ?MONITORS_NUM ->
                Not_monitoring = lists:subtract(State#state.nodes, State#state.monitored),
                send_to_nodes({monitor_request, {peer, node()}, Created_trans}, Not_monitoring),
                build_pid_dict(Not_monitoring);
            true ->
                ?DBG("too few neighbors"),
                erlang:send_after(?CHECK_NEIGHBOR_EVERY, self(), search_monitors_time), % retry
                State#state.candidates
        end,
    {noreply, State#state{candidates = Candidates}};


% here just for test
handle_info({give_me_all_transactions, Pid}, State) ->
    Trans_to_send = lists:map(fun(T)->
        {transaction,
        T#transaction.id,
        T#transaction.state,
        T#transaction.seq_number,
        T#transaction.guard,
        T#transaction.command} end, trans_to_list(State#state.transactions)),
    Pid ! {give_you_all_transactions, Trans_to_send},
    {noreply, State};


% here just for test
handle_info({give_me_all_neighbors, Pid}, State) ->
    Pid ! {give_you_all_neighbors, State#state.nodes},
    {noreply, State};


% here just for test
handle_info({give_me_all_variables, Pid}, State) ->
    Pid ! {give_you_all_variables, State#state.vars},
    {noreply, State};


% here just for test
handle_info({give_me_all_executeds, Pid}, State) ->
    Pid ! {give_you_all_executeds, State#state.executed},
    {noreply, State};


% here just for test
handle_info(crash, _State) ->
    42 + "goodbye";


handle_info(_Message, State) ->
    ?DBG("received unknown message"),
    {noreply, State}.




%%% ======================================================================================
%%% private functions
%%% ======================================================================================
-spec merge_neighbors(New_neighbor:: [node_pid()], Old_neighbor:: [node_pid()]) -> [node_pid()].
merge_neighbors([], Neighbors) -> Neighbors;
merge_neighbors([New_neighbor | Other], Neighbors) ->
    Neighbor_already_known = lists:member(New_neighbor, Neighbors) or (element(2, New_neighbor) == node()),
    if
        length(Neighbors) >= ?MIN_NEIGHBORS ->
            Neighbors;

        Neighbor_already_known ->
            merge_neighbors(Other, Neighbors);

        true ->
            Node = element(2, New_neighbor),
            Is_alive = pong == net_adm:ping(Node),
            if Is_alive ->
                    ?DBG("passed node " ++ ?FROM_NODES_TO_STRING([New_neighbor]) ++
                        " alive"),
                    monitor_node(Node, true),
                    gen_server:cast(New_neighbor, {link_require, {peer, node()}}),
                    merge_neighbors(Other, [New_neighbor|Neighbors]);
                true ->
                    ?DBG("passed node " ++ ?FROM_NODES_TO_STRING([New_neighbor]) ++
                        " crashed"),
                    merge_neighbors(Other, Neighbors)
            end
    end.


-spec build_proposed_seq(Dict:: orddict:orddict(trans_seq_node(), #transaction{})) -> trans_seq_node().
build_proposed_seq(Trans) ->
    case trans_to_list(Trans) of
        [] ->
            {0, node()};
        List ->
            Last = lists:last(List),
            {element(1,Last#transaction.seq_number) + 1, node()}
    end.


%% @doc Returns the list of the first frozen transactions
%% @return a list containing the first ordered transactions that are frozen
-spec frozen_trans(State:: #state{}) -> [#transaction{}].
frozen_trans(State) ->
    lists:takewhile(fun(T) ->
        T#transaction.state /= pending end, trans_to_list(State#state.transactions)).


-spec trans_to_list(Dict:: orddict:orddict(trans_seq_node(), #transaction{})) -> [#transaction{}].
trans_to_list(Dict) ->
    lists:map(fun(E) -> element(2, E) end, orddict:to_list(Dict)).


-spec not_avail(Send:: {guard(), guard(), command()}, Vars:: #variables{}) -> boolean().
not_avail({Avail, _G, _C}, Vars) ->
    not Avail(Vars).


%% @doc if Sends contains sends where the availability condition is unspecified it converts them
%% to guarded sends that are always enabled
-spec to_guarded(Sends:: [({guard(), guard(), command()} | { guard(), command()})]) ->
    [{guard(), guard(), command()}].
to_guarded(Sends) ->
    lists:map(fun(S) -> guard_send(S) end, Sends).


%@doc if Send is a send where the availability condition is unspecified it converts it to a guarded send that is always enabled
-spec guard_send(Send:: ({guard(), guard(), command()} | { guard(), command()})) ->
    {guard(), guard(), command()}.
guard_send(Send) ->
    case Send of
        {Guard, Command} ->
            {?ALWAYS, Guard, Command};
        _Else ->
            Send
    end.


%% @doc if there are frozen transactions that were not already executed it applies them to the
%% state and returns the result.
%% @return a state where the state variables and the number of executed transactions are update
%%         accordingly 
-spec execute_ready(State:: #state{}) -> #state{}.
execute_ready(State) ->
    Frozen = frozen_trans(State),
    if
        length(Frozen) > State#state.executed ->
            Todo = lists:nthtail(State#state.executed, Frozen),
            New_vars = lists:foldl(fun(T, V) ->
                apply_tran(T, V) end, State#state.vars, Todo),
            State#state{executed = length(Frozen), vars = New_vars};
        true ->
            State
    end.


%% @doc Applies Tran to Vars and returns the updated state variables.
%% REQUIRE state(Tran) != pending
%% @param Tran transaction to apply
%% @param Vars current state variables
%% @return Vars if state(Tran) == aborted or if not Vars |= guard(Tran) else apply(command(Tran),
%%         Vars)
-spec apply_tran(Tran:: #transaction{}, Vars:: #variables{}) -> #variables{}.
apply_tran(Tran, Vars) ->
    case Tran#transaction.state of
        aborted ->
            Vars;
        confirmed ->
            Guard = Tran#transaction.guard,
            Command = Tran#transaction.command,
            Executable = Guard(Vars),
            if
                Executable ->
                    Command(Vars);
                true ->
                    Vars
            end
    end.


-spec update_trans(New_trans:: [#transaction{}], My_trans:: orddict:orddict(trans_seq_node(), #transaction{}), Infos:: dict:dict(trans_id_node(), #tran_aux_info{})) -> {orddict:orddict(trans_seq_node(), #transaction{}), dict:dict(trans_id_node(), #tran_aux_info{})}.
update_trans([], My_trans, Infos) -> {My_trans, Infos};
update_trans([Head|Tail], My_trans, Infos) ->
    Id = Head#transaction.id,
    New_seq = Head#transaction.seq_number,
    Found = dict:find(Id, Infos),
    case Found of
        {ok, Head_info} ->
            Old_seq = Head_info#tran_aux_info.current_seq,
            update_trans(
                Tail,
                orddict:store(New_seq, Head, orddict:erase(Old_seq, My_trans)),
                dict:store(Id, Head_info#tran_aux_info{current_seq = New_seq}, Infos));
        error ->
            update_trans(
                Tail,
                orddict:store(New_seq, Head, My_trans),
                dict:store(Id, #tran_aux_info{
                        asked_this = [],
                        max_prop_seq = New_seq,
                        waiting_response = dict:new(),
                        current_seq = New_seq
                    }, Infos))
    end.


%% @doc sends a cast with Msg to every node in Nodes.
%% @param Msg message to send
%% @param Nodes list of receiving pids
-spec send_to_nodes(Msg:: any(), Nodes:: [node_pid()]) -> ok.
send_to_nodes(Msg, Nodes) ->
    lists:foreach(fun(P) -> gen_server:cast(P, Msg) end, Nodes).


%% @doc Given a list of pids returns a dict where the keys are the elements of the list.
-spec build_pid_dict(Pids:: list(node_pid())) -> dict:dict(node_pid(), atom()).
build_pid_dict(Pids) ->
    dict:from_list(lists:map(fun(P)->{P, null} end, Pids)).


-spec reap_crashed_responses(Trans:: dict:dict(trans_id_node(), #tran_aux_info{}), Pid:: node_pid()) -> ok.
reap_crashed_responses(Trans, Pid) ->
    dict:fold(fun(A,B,C)-> check_and_reap(A,B,C) end, Pid, Trans),
    ok.


-spec check_and_reap(TransID:: trans_id_node(), Tran:: #tran_aux_info{}, Pid:: node_pid()) -> node_pid().
check_and_reap(TransID, Tran, Pid) ->
    Is_waiting_crashed = error /= dict:find(Pid, Tran#tran_aux_info.waiting_response),
    if
        Is_waiting_crashed ->
            gen_server:cast(self(), {trans_response, TransID, Pid, {-1, Pid}});
        true ->
            ok
    end,
    Pid.


% For OTP <= 21
-spec find_in_list(Fun:: function(), List:: list(any())) -> {value, any()} | false.
find_in_list(_Fun, []) -> false;
find_in_list(Fun, [First | Other]) ->
    Is_this = Fun(First),
    if
        Is_this ->
            {value, First};
        true ->
            find_in_list(Fun, Other)
    end.