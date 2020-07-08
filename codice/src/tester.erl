%% @author Massimiliano De Luise
%% @author Massimo Comuzzo
%% @doc Testing module containing useful function for testing.
%% The module contains test generator functions and simple test functions. The test generator
%% functions are nedded for removing the default test timeout of eunit (5 seconds).
-module(tester).

-include_lib("eunit/include/eunit.hrl").
-include("config.hrl").



%%% ===============================================================================================
%%% test generator functions (nedded for removing the default for eunit test timeout of 5 seconds)
%%% ===============================================================================================
% timeout for single test = 10 min (60000 ms)

%% @doc Test to verify the correctness of the created protocol. Description: Simple neighbors test
%% within initially processes doesn't known each other, there's a support_server which help the
%% connection.
neighbors1_test_() -> {"Simple neighbors test within initially processes doesn't known each other,
                        there's a support_server which help the connection.",
                       {timeout, 60000, [fun neighbors1/0]}}.

%% @doc Test to verify the correctness of the created protocol. Description: Neighbors test with
%% crash of one process.
neighbors2_test_() -> {"Neighbors test with crash of one process.",
                       {timeout, 60000, [fun neighbors2/0]}}.



%% @doc Test to verify the correctness of the created protocol. Description: Some processes are in
%% the same network and have a partial view of the transactions, through gossiping they must reach
%% a shared consistent view.
gossip1_test_() -> {"Some processes are in the same network and have a partial view of the
                    transactions, through gossiping they must reach a shared consistent view.",
                    {timeout, 60000, [fun gossip1/0]}}.

%% @doc Test to verify the correctness of the created protocol. Description: Some processes are in
%% the same network and have a partial view of the transactions, one of them crash. Through
%% gossiping they must reach a shared consistent view.
gossip2_test_() -> {"Some processes are in the same network and have a partial view of the
                    transactions, one of them crash. Through gossiping they must reach a shared
                    consistent view.",
                    {timeout, 60000, [fun gossip2/0]}}.



%% @doc Test to verify the correctness of the created protocol. Description: Some processes are in
%% the same network, one of them want to create a transaction. They must reach an agreement about
%% sequence number of transaction, and in the end all of them must be notify about it.
isis1_test_() -> {"Some processes are in the same network, one of them want to create a
                  transaction. They must reach an agreement about sequence number of transaction,
                  and in the end all of them must be notify about it.",
                  {timeout, 60000, [fun isis1/0]}}.

%% @doc Test to verify the correctness of the created protocol. Description: Some processes are in
%% the same network, two of them want to create a transaction at the same time. They must reach an
%% agreement about sequence number of transactions, and in the end all processes must be notify
%% about the two transactions.
isis2_test_() -> {"Some processes are in the same network, two of them want to create a
                  transaction at the same time. They must reach an agreement about sequence number
                  of transactions, and in the end all processes must be notify about the two
                  transactions.",
                  {timeout, 60000, [fun isis2/0]}}.

%% @doc Test to verify the correctness of the created protocol. Description: Some processes are in
%% the same network, all of them want to create a transaction at the same time. They must reach an
%% agreement about sequence number of transactions, and in the end all processes must have the
%% same transactions sequence.
isis3_test_() -> {"Some processes are in the same network, all of them want to create a
                  transaction at the same time. They must reach an agreement about sequence number
                  of transactions, and in the end all processes must have the same transactions
                  sequence.",
                  {timeout, 60000, [fun isis3/0]}}.

%% @doc Test to verify the correctness of the created protocol. Description: Some processes are in
%% the same network, two of them want to create a transaction at the same time. They must reach an
%% agreement about sequence number of transactions, and in the end all processes must be notify
%% about the two transactions.
isis4_test_() -> {"Some processes are in the same network, two of them want to create a
                  transaction at the same time. They must reach an agreement about sequence number
                  of transactions, and in the end all processes must be notify about the two
                  transactions.",
                  {timeout, 60000, [fun isis4/0]}}.

%% @doc Test to verify the correctness of the created protocol. Description: Some processes are in
%% the same network, two of them want to create a transaction at the same time. They must reach an
%% agreement about sequence number of transactions, and in the end all processes must be notify
%% about the two transactions.
isis5_test_() -> {"Some processes are in the same network, two of them want to create a
                  transaction at the same time. They must reach an agreement about sequence number
                  of transactions, and in the end all processes must be notify about the two
                  transactions.",
                  {timeout, 60000, [fun isis5/0]}}.

%% @doc Test to verify the correctness of the created protocol. Description: Some processes are in
%% the same network and one of them want to create a transaction. When it start the transaction,
%% before it will be confirmed, a new process connect to the first. The transaction must be
%% confirmed, and in the end all processes must be notify about the transaction.
isis6_test_() -> {"Some processes are in the same network and one of them want to create a
                  transaction. When it start the transaction, before it will be confirmed, a new
                  process connect to the first. The transaction must be confirmed, and in the end
                  all processes must be notify about the transaction.",
                  {timeout, 60000, [fun isis6/0]}}.



%% @doc Test to verify the correctness of the created protocol. Description: Some processes are in
%% the same network and one of them want to create a transaction. When it start the transaction,
%% before it will be confirmed, the process crash. The transaction must be aborted, and in the end
%% all processes must be notify about the transaction.
isis_crash1_test_() -> {"Some processes are in the same network and one of them want to create a
                        transaction. When it start the transaction, before it will be confirmed,
                        the process crash. The transaction must be aborted, and in the end all
                        processes must be notify about the transaction.",
                        {timeout, 60000, [fun isis_crash1/0]}}.

%% @doc Test to verify the correctness of the created protocol. Description: Some processes are in
%% the same network and one of them want to create a transaction. When it start the transaction,
%% before it will be confirmed, a neighbor of first process crash. The transaction must be
%% confirmed, and in the end all processes must be notify about the transaction.
isis_crash2_test_() -> {"Some processes are in the same network and one of them want to create a
                        transaction. When it start the transaction, before it will be confirmed,
                        a neighbor of first process crash. The transaction must be confirmed, and
                        in the end all processes must be notify about the transaction.",
                        {timeout, 60000, [fun isis_crash2/0]}}.

%% @doc Test to verify the correctness of the created protocol. Description: Some processes are in
%% the same network and one of them want to create a transaction. When the transaction is confirmed
%% the sender crash. In the end all processes must be notify about the transaction.
isis_crash3_test_() -> {"Some processes are in the same network and one of them want to create a
                        transaction. When the transaction is confirmed the sender crash. In the
                        end all processes must be notify about the transaction.",
                        {timeout, 60000, [fun isis_crash3/0]}}.



%% @doc Test to verify the correctness of the created protocol. Description: Tests within
%% initially processes doesn't known each other, there's a support_server which help the
%% connection. Three processes want create a transaction at the same time. They must reach an
%% agreement about sequence number of transactions, and in the end all processes must be notify
%% about the three transactions. This test contain condition of neighbors, gossip and isis tests.
integration1_test_() -> {"Tests within initially processes doesn't known each other, there's a
                         support_server which help the connection. Three processes want create a
                         transaction at the same time. They must reach an agreement about sequence
                         number of transactions, and in the end all processes must be notify about
                         the three transactions. This test contain condition of neighbors, gossip
                         and isis tests.",
                         {timeout, 60000, [fun integration1/0]}}.

%% @doc Test to verify the correctness of the created protocol. Description: Tests within
%% initially processes doesn't known each other, there's a support_server which help the
%% connection. Three processes want create a transaction at the same time, and 4 processes crash.
%% The remaining processes must reach an agreement about sequence number of transactions, and in
%% the end all remaining processes must be notify about the three transactions. This test contain %% condition of neighbors, gossip and isis tests.
integration2_test_() -> {"Tests within initially processes doesn't known each other, there's a
                         support_server which help the connection. Three processes want create a
                         transaction at the same time, and 4 processes crash. The remaining
                         processes must reach an agreement about sequence number of transactions,
                         and in the end all remaining processes must be notify about the three
                         transactions. This test contain condition of neighbors, gossip and isis
                         tests.",
                         {timeout, 60000, [fun integration2/0]}}.

%% @doc Test to check the correctness of the protocol created. Description: Some processes are in
%% the same network, two of them have an init section containing one send operation.
%% They must reach an agreement on the sequence numbers of the transactions, and in the end all
%% processes must be notified about the two transactions.
init1_test_() -> {"Some processes are in the same network, two of them have an init section
                   containing a send operation. They must reach an agreement on the sequence
                   numbers of the transactions, and in the end all processes must be notified
                   about the two transactions.",
                         {timeout, 60000, [fun init1/0]}}.


%% @doc Test to check the correctness of the protocol created. Description: Some processes are in
%% the same network, two of them have an init section containing two send operations.
%% They must reach an agreement on the sequence numbers of the transactions, and in the end all
%% processes must be notified about the four transactions.
init2_test_() -> {"Some processes are in the same network, two of them have an init section
                   containing two send operations. They must reach an agreement on the sequence
                   numbers of the transactions, and in the end all processes must be notified
                   about the four transactions.",
                         {timeout, 60000, [fun init2/0]}}.

%% @doc Test to check the correctness of the protocol created. Description: Some processes are in
%% the same network, two of them have an init section containing three send operations.
%% They must reach an agreement on the sequence numbers of the transactions, and in the end all
%% processes must be notified about the x transactions and must execute them atomically.
init3_test_() -> {"Some processes are in the same network, two of them have an init section
                   containing three send operations. They must reach an agreement on the sequence
                   numbers of the transactions, and in the end all processes must be notified
                   about the six transactions and must execute them atomically",
                         {timeout, 60000, [fun init3/0]}}.

%% @doc Test to check the correctness of the protocol created. Description: Some processes are in
%% the same network, two of them have an loop section containing the same send operation.
%% They must reach an agreement on the sequence numbers of the transactions, and in the end all
%% processes must execute the transactions and reach the same values for the state variables.
loop_exec1_test_() -> {"Some processes are in the same network, two of them have an loop section
                        containing the same send operation. They must reach an agreement on the
                        sequence numbers of the transactions, and in the end all processes must
                        execute the transactions and reach the same values for the state variables.",
                        {timeout, 60000, [fun loop_exec1/0]}}.


%% @doc Test to check the correctness of the protocol created. Description: Some processes are in
%% the same network, one of them have an loop section containing a guarded send.
%% They must reach an agreement on the sequence numbers of the five transactions, and in the end
%% all processes must execute the transactions and reach the same values for the state variables.
loop_exec2_test_() -> {"Some processes are in the same network, one of them have an loop section
                        containing a guarded send. They must reach an agreement on the
                        sequence numbers of the five transactions, and in the end all processes
                        must execute the transactions and reach the same values for the state variables.",
                        {timeout, 60000, [fun loop_exec2/0]}}.


%%% ===============================================================================================
%%% test functions
%%% ===============================================================================================


%%% =========================================
%%% neighbors tests
%%% =========================================

neighbors1() ->
    ?debugMsg("started neighbors1 test at " ++ my_time()),
    Pid1 = peer:start(?DEFAULT_VARS),
    Pid2 = peer:start(?DEFAULT_VARS),
    Pid3 = peer:start(?DEFAULT_VARS),
    Pid4 = peer:start(?DEFAULT_VARS),
    Pid5 = peer:start(?DEFAULT_VARS),
    Pid6 = peer:start(?DEFAULT_VARS),
    support_server:start([Pid1, Pid2, Pid3]),
    
    timer:sleep(?CHECK_NEIGHBOR_EVERY * 10), % needed for letting communicate the processes
    Nodes = request_to_all(give_me_all_neighbors, [Pid1, Pid2, Pid3, Pid4, Pid5, Pid6], 6,
        give_you_all_neighbors),

    peer:stop(Pid1), % just to not let them crash (because tester exits)
    peer:stop(Pid2),
    peer:stop(Pid3),
    peer:stop(Pid4),
    peer:stop(Pid5),
    peer:stop(Pid6),
    support_server:stop(),

    ?assert(lists:any(fun(N) -> length(N) >= ?MIN_NEIGHBORS end, Nodes)).


neighbors2() ->
    ?debugMsg("started neighbors2 test at " ++ my_time()),
    Pid1 = peer:start(?DEFAULT_VARS, []),
    Pid2 = peer:start(?DEFAULT_VARS, [Pid1]),
    Pid3 = peer:start(?DEFAULT_VARS, [Pid2]),
    Pid4 = peer:start(?DEFAULT_VARS, [Pid1, Pid3]),

    timer:sleep(2000), % needed for letting communicate the processes
    error_logger:tty(false), % disable so error report of Pid4 crash doesn't popup
    Pid4 ! crash,

    timer:sleep(?CHECK_NEIGHBOR_EVERY * 3), % needed for letting communicate the processes
    Nodes = request_to_all(give_me_all_neighbors, [Pid1, Pid2, Pid3], 3,
        give_you_all_neighbors),

    peer:stop(Pid1), % just to not let them crash (because tester exits)
    peer:stop(Pid2),
    peer:stop(Pid3),

    ?assertNot(is_process_alive(Pid4)),
    ?assert(lists:any(fun(N) -> length(N) >= ?MIN_NEIGHBORS end, Nodes)).


%%% =========================================
%%% gossip tests
%%% =========================================
gossip1() ->
    ?debugMsg("started gossip1 test at " ++ my_time()),
    Pid1 = peer:start(?DEFAULT_VARS),
    Pid2 = peer:start(?DEFAULT_VARS,[Pid1]),
    
    Guard_fun = fun(_X) -> true end,
    Cmd_fun = fun(X) -> X end,
    
    peer:add_trans(Pid1, {0, Pid1}, {0, Pid1}, confirmed, Guard_fun, Cmd_fun),
    peer:add_trans(Pid2, {0, Pid1}, {0, Pid1}, confirmed, Guard_fun, Cmd_fun),
    peer:add_trans(Pid1, {1, Pid2}, {1, Pid1}, pending, Guard_fun, Cmd_fun),
    peer:add_trans(Pid2, {1, Pid2}, {2, Pid2}, confirmed, Guard_fun, Cmd_fun),
    peer:add_trans(Pid1, {2, Pid1}, {30, Pid1}, confirmed, Guard_fun, Cmd_fun),
    
    timer:sleep(?GOSSIP_EVERY * 8), % needed for letting communicate the processes
    Trans = request_to_all(give_me_all_transactions, [Pid1, Pid2], 2, give_you_all_transactions),
    
    peer:stop(Pid1), % just to not let them crash (because tester exits)
    peer:stop(Pid2),
    
    ?assertEqual(hd(Trans), lists:nth(2, Trans)),
    ?assertEqual([
                   {transaction, {0, Pid1}, confirmed, {0, Pid1}, Guard_fun, Cmd_fun},
                   {transaction, {1, Pid2}, confirmed, {2, Pid2}, Guard_fun, Cmd_fun},
                   {transaction, {2, Pid1}, confirmed, {30, Pid1}, Guard_fun, Cmd_fun}
                  ],
                hd(Trans)
    ).


gossip2() ->
    ?debugMsg("started gossip2 test at " ++ my_time()),
    Pid1 = peer:start(?DEFAULT_VARS),
    Pid2 = peer:start(?DEFAULT_VARS, [Pid1]),
    Pid3 = peer:start(?DEFAULT_VARS, [Pid1]),
    Pid4 = peer:start(?DEFAULT_VARS, [Pid2, Pid3]),
    
    Guard_fun = fun(_X) -> true end,
    Cmd_fun = fun(X) -> X end,
    
    peer:add_trans(Pid1, {0, Pid1}, {0, Pid4}, confirmed, Guard_fun, Cmd_fun),
    peer:add_trans(Pid1, {1, Pid2}, {1, Pid1}, pending, Guard_fun, Cmd_fun),
    peer:add_trans(Pid1, {2, Pid1}, {2, Pid1}, confirmed, Guard_fun, Cmd_fun),
    peer:add_trans(Pid1, {3, Pid4}, {3, Pid1}, pending, Guard_fun, Cmd_fun),

    peer:add_trans(Pid2, {0, Pid1}, {0, Pid2}, pending, Guard_fun, Cmd_fun),
    peer:add_trans(Pid2, {1, Pid2}, {1, Pid4}, confirmed, Guard_fun, Cmd_fun),
    peer:add_trans(Pid2, {2, Pid1}, {2, Pid2}, pending, Guard_fun, Cmd_fun),
    peer:add_trans(Pid2, {3, Pid4}, {3, Pid2}, pending, Guard_fun, Cmd_fun),

    peer:add_trans(Pid3, {0, Pid1}, {0, Pid3}, pending, Guard_fun, Cmd_fun),
    peer:add_trans(Pid3, {1, Pid2}, {1, Pid3}, pending, Guard_fun, Cmd_fun),
    peer:add_trans(Pid3, {2, Pid1}, {2, Pid3}, pending, Guard_fun, Cmd_fun),
    peer:add_trans(Pid3, {3, Pid4}, {3, Pid3}, pending, Guard_fun, Cmd_fun),

    peer:add_trans(Pid4, {0, Pid1}, {0, Pid4}, pending, Guard_fun, Cmd_fun),
    peer:add_trans(Pid4, {1, Pid2}, {1, Pid4}, pending, Guard_fun, Cmd_fun),
    peer:add_trans(Pid4, {2, Pid1}, {2, Pid4}, pending, Guard_fun, Cmd_fun),
    peer:add_trans(Pid4, {3, Pid4}, {3, Pid4}, confirmed, Guard_fun, Cmd_fun),
    
    timer:sleep(?GOSSIP_EVERY * 2), % needed for letting communicate the processes
    error_logger:tty(false), % disable so error report of Pid3 crash doesn't popup
    Pid3 ! crash,

    timer:sleep(?GOSSIP_EVERY * 5), % needed for letting communicate the processes
    Trans = request_to_all(give_me_all_transactions, [Pid1, Pid2, Pid4], 3,
        give_you_all_transactions),
    
    peer:stop(Pid1), % just to not let them crash (because tester exits)
    peer:stop(Pid2),
    peer:stop(Pid4),
    
    ?assertNot(is_process_alive(Pid3)),
    ?assertEqual(hd(Trans), lists:nth(2, Trans)),
    ?assertEqual(lists:nth(2, Trans), lists:nth(3, Trans)),
    ?assertEqual([
                   {transaction, {0, Pid1}, confirmed, {0, Pid4}, Guard_fun, Cmd_fun},
                   {transaction, {1, Pid2}, confirmed, {1, Pid4}, Guard_fun, Cmd_fun},
                   {transaction, {2, Pid1}, confirmed, {2, Pid1}, Guard_fun, Cmd_fun},
                   {transaction, {3, Pid4}, confirmed, {3, Pid4}, Guard_fun, Cmd_fun}
                  ],
                  hd(Trans)
    ).


%%% =========================================
%%% isis tests
%%% =========================================
isis1() ->
    ?debugMsg("started isis1 test at " ++ my_time()),
    Pid1 = peer:start(?DEFAULT_VARS),
    Pid2 = peer:start(?DEFAULT_VARS, [Pid1]),
    Pid3 = peer:start(?DEFAULT_VARS, [Pid1]),

    Guard_fun = fun(_X) -> true end,
    Cmd_fun = fun(X) -> X end,
    peer:send(Pid1, Guard_fun, Cmd_fun),

    timer:sleep(?GOSSIP_EVERY * 4), % needed for letting communicate the processes

    Trans = request_to_all(give_me_all_transactions, [Pid1, Pid2, Pid3], 3,
        give_you_all_transactions),
    
    peer:stop(Pid1), % just to not let them crash (because tester exits)
    peer:stop(Pid2),
    peer:stop(Pid3),
    
    ?assertEqual(hd(Trans), lists:nth(2, Trans)),
    ?assertEqual(lists:nth(2, Trans), lists:nth(3, Trans)),
    ?assertEqual([
                   {transaction, {0, Pid1}, confirmed, {0, Pid3}, Guard_fun, Cmd_fun}
                  ],
                  hd(Trans)
    ).


isis2() ->
    ?debugMsg("started isis2 test at " ++ my_time()),
    Pid1 = peer:start(?DEFAULT_VARS),
    Pid2 = peer:start(?DEFAULT_VARS, [Pid1]),
    Pid3 = peer:start(?DEFAULT_VARS, [Pid1, Pid2]),

    Guard_fun = fun(_X) -> true end,
    Cmd_fun = fun(X) -> X end,
    peer:send(Pid1, Guard_fun, Cmd_fun),
    peer:send(Pid3, Guard_fun, Cmd_fun),

    timer:sleep(?GOSSIP_EVERY * 10), % needed for letting communicate the processes

    Trans = request_to_all(give_me_all_transactions, [Pid1, Pid2, Pid3], 3,
        give_you_all_transactions),
    
    peer:stop(Pid1), % just to not let them crash (because tester exits)
    peer:stop(Pid2),
    peer:stop(Pid3),
    
    ?assertEqual(hd(Trans), lists:nth(2, Trans)),
    ?assertEqual(lists:nth(2, Trans), lists:nth(3, Trans)),

    ?assertEqual(2, length(hd(Trans))),

    ?assert(all_confirmed(hd(Trans))),
    ?assert(all_id_unique(hd(Trans))),
    ?assert(all_seq_unique(hd(Trans))).


isis3() ->
    ?debugMsg("started isis3 test at " ++ my_time()),
    Pid1 = peer:start(?DEFAULT_VARS),
    Pid2 = peer:start(?DEFAULT_VARS, [Pid1]),
    Pid3 = peer:start(?DEFAULT_VARS, [Pid1]),
    Pid4 = peer:start(?DEFAULT_VARS, [Pid2, Pid3]),

    Guard_fun = fun(_X) -> true end,
    Cmd_fun = fun(X) -> X end,

    peer:send(Pid1, Guard_fun, Cmd_fun),
    peer:send(Pid2, Guard_fun, Cmd_fun),
    peer:send(Pid3, Guard_fun, Cmd_fun),
    peer:send(Pid4, Guard_fun, Cmd_fun),

    timer:sleep(?GOSSIP_EVERY * 20), % needed for letting communicate the processes

    Trans = request_to_all(give_me_all_transactions, [Pid1, Pid2, Pid3, Pid4], 4,
        give_you_all_transactions),
    
    peer:stop(Pid1), % just to not let them crash (because tester exits)
    peer:stop(Pid2),
    peer:stop(Pid3),
    peer:stop(Pid4),
    
    ?assertEqual(hd(Trans), lists:nth(2, Trans)),
    ?assertEqual(lists:nth(2, Trans), lists:nth(3, Trans)),
    ?assertEqual(lists:nth(3, Trans), lists:nth(4, Trans)),

    ?assertEqual(4, length(hd(Trans))),

    ?assert(all_confirmed(hd(Trans))),
    ?assert(all_id_unique(hd(Trans))),
    ?assert(all_seq_unique(hd(Trans))).


isis4() ->
    ?debugMsg("started isis4 test at " ++ my_time()),
    Pid1 = peer:start(?DEFAULT_VARS),
    Pid2 = peer:start(?DEFAULT_VARS, [Pid1]),
    Pid3 = peer:start(?DEFAULT_VARS, [Pid1]),
    Pid4 = peer:start(?DEFAULT_VARS, [Pid2, Pid3]),
    Pid5 = peer:start(?DEFAULT_VARS, [Pid4]),
    Pid6 = peer:start(?DEFAULT_VARS, [Pid5]),

    Guard_fun = fun(_X) -> true end,
    Cmd_fun = fun(X) -> X end,

    peer:send(Pid1, Guard_fun, Cmd_fun),
    peer:send(Pid6, Guard_fun, Cmd_fun),

    timer:sleep(?GOSSIP_EVERY * 20), % needed for letting communicate the processes

    Trans = request_to_all(give_me_all_transactions, [Pid1, Pid2, Pid3, Pid4, Pid5, Pid6], 6,
        give_you_all_transactions),
    
    peer:stop(Pid1), % just to not let them crash (because tester exits)
    peer:stop(Pid2),
    peer:stop(Pid3),
    peer:stop(Pid4),
    peer:stop(Pid5),
    peer:stop(Pid6),
    
    ?assertEqual(hd(Trans), lists:nth(2, Trans)),
    ?assertEqual(lists:nth(2, Trans), lists:nth(3, Trans)),
    ?assertEqual(lists:nth(3, Trans), lists:nth(4, Trans)),
    ?assertEqual(lists:nth(4, Trans), lists:nth(5, Trans)),
    ?assertEqual(lists:nth(5, Trans), lists:nth(6, Trans)),

    ?assertEqual(2, length(hd(Trans))),

    ?assert(all_confirmed(hd(Trans))),
    ?assert(all_id_unique(hd(Trans))),
    ?assert(all_seq_unique(hd(Trans))).


isis5() ->
    ?debugMsg("started isis5 test at " ++ my_time()),
    Pid1 = peer:start(?DEFAULT_VARS,[]),
    Pid2 = peer:start(?DEFAULT_VARS,[Pid1]),
    Pid3 = peer:start(?DEFAULT_VARS,[Pid2]),
    Pid4 = peer:start(?DEFAULT_VARS,[Pid3]),
    Pid5 = peer:start(?DEFAULT_VARS,[Pid4]),
    Pid6 = peer:start(?DEFAULT_VARS,[Pid3]),

    Guard_fun = fun(_X) -> true end,
    Cmd_fun = fun(X) -> X end,

    peer:send(Pid6, Guard_fun, Cmd_fun),
    peer:send(Pid1, Guard_fun, Cmd_fun),

    timer:sleep(?GOSSIP_EVERY * 30), % needed for letting communicate the processes

    Trans = request_to_all(give_me_all_transactions, [Pid1, Pid2, Pid3, Pid4, Pid5, Pid6], 6,
        give_you_all_transactions),
    
    peer:stop(Pid1), % just to not let them crash (because tester exits)
    peer:stop(Pid2),
    peer:stop(Pid3),
    peer:stop(Pid4),
    peer:stop(Pid5),
    peer:stop(Pid6),
    
    ?assertEqual(hd(Trans), lists:nth(2, Trans)),
    ?assertEqual(lists:nth(2, Trans), lists:nth(3, Trans)),
    ?assertEqual(lists:nth(3, Trans), lists:nth(4, Trans)),
    ?assertEqual(lists:nth(4, Trans), lists:nth(5, Trans)),
    ?assertEqual(lists:nth(5, Trans), lists:nth(6, Trans)),

    ?assertEqual(2, length(hd(Trans))),

    ?assert(all_confirmed(hd(Trans))),
    ?assert(all_id_unique(hd(Trans))),
    ?assert(all_seq_unique(hd(Trans))).


isis6() ->
    ?debugMsg("started isis6 test at " ++ my_time()),
    Pid1 = peer:start(?DEFAULT_VARS),
    Pid2 = peer:start(?DEFAULT_VARS, [Pid1]),
    Pid3 = peer:start(?DEFAULT_VARS, [Pid1, Pid2]),

    Guard_fun = fun(_X) -> true end,
    Cmd_fun = fun(X) -> X end,

    peer:send(Pid1, Guard_fun, Cmd_fun),
    Pid4 = peer:start(?DEFAULT_VARS, [Pid1]),

    timer:sleep(?GOSSIP_EVERY * 5), % needed for letting communicate the processes

    Trans = request_to_all(give_me_all_transactions, [Pid1, Pid2, Pid3, Pid4], 4,
        give_you_all_transactions),

    peer:stop(Pid1), % just to not let them crash (because tester exits)
    peer:stop(Pid2),
    peer:stop(Pid3),
    peer:stop(Pid4),

    ?assertEqual(hd(Trans), lists:nth(2, Trans)),
    ?assertEqual(lists:nth(2, Trans), lists:nth(3, Trans)),
    ?assertEqual(lists:nth(3, Trans), lists:nth(4, Trans)),

    ?assertEqual(1, length(hd(Trans))),

    ?assert(element(3, hd(hd(Trans))) == confirmed).


isis_crash1() ->
    ?debugMsg("started isis_crash1 test at " ++ my_time()),
    Pid1 = peer:start(?DEFAULT_VARS),
    Pid2 = bully:start(Pid1, before_trans_done, 5000),
    Pid3 = peer:start(?DEFAULT_VARS, [Pid1]),
    Pid4 = peer:start(?DEFAULT_VARS, [Pid3]),
    Pid5 = peer:start(?DEFAULT_VARS, [Pid4]),

    Guard_fun = fun(_X) -> true end,
    Cmd_fun = fun(X) -> X end,

    error_logger:tty(false), % disable so error report of Pid1 crash doesn't popup
    peer:send(Pid1, Guard_fun, Cmd_fun),

    timer:sleep(?GOSSIP_EVERY * 7), % needed for letting communicate the processes

    Trans = request_to_all(give_me_all_transactions, [Pid3, Pid4, Pid5], 3,
        give_you_all_transactions),

    bully:stop(Pid2), % just to not let them crash (because tester exits)
    peer:stop(Pid3),
    peer:stop(Pid4),
    peer:stop(Pid5),

    ?assertNot(is_process_alive(Pid1)),

    ?assertEqual(hd(Trans), lists:nth(2, Trans)),
    ?assertEqual(lists:nth(2, Trans), lists:nth(3, Trans)),

    ?assertEqual(1, length(hd(Trans))),

    ?assert(element(3, hd(hd(Trans))) == aborted).


isis_crash2() ->
    ?debugMsg("started isis_crash2 test at " ++ my_time()),
    Pid1 = peer:start(?DEFAULT_VARS),
    Pid2 = peer:start(?DEFAULT_VARS, [Pid1]),
    Pid3 = peer:start(?DEFAULT_VARS, [Pid1, Pid2]),
    Pid4 = peer:start(?DEFAULT_VARS, [Pid1]),

    Guard_fun = fun(_X) -> true end,
    Cmd_fun = fun(X) -> X end,

    peer:send(Pid1, Guard_fun, Cmd_fun),

    timer:sleep(50),

    error_logger:tty(false), % disable so error report of Pid4 crash doesn't popup
    Pid4 ! crash,

    timer:sleep(?GOSSIP_EVERY * 3), % needed for letting communicate the processes

    Trans = request_to_all(give_me_all_transactions, [Pid1, Pid2, Pid3], 3,
        give_you_all_transactions),

    peer:stop(Pid1), % just to not let them crash (because tester exits)
    peer:stop(Pid2),
    peer:stop(Pid3),

    ?assertNot(is_process_alive(Pid4)),

    ?assertEqual(hd(Trans), lists:nth(2, Trans)),
    ?assertEqual(lists:nth(2, Trans), lists:nth(3, Trans)),

    ?assertEqual(1, length(hd(Trans))),

    ?assert(element(3, hd(hd(Trans))) == confirmed).


isis_crash3() ->
    ?debugMsg("started isis_crash3 test at " ++ my_time()),
    Pid1 = peer:start(?DEFAULT_VARS),
    Pid2 = peer:start(?DEFAULT_VARS, [Pid1]),
    Pid3 = bully:start(Pid1, after_trans_done, 5000),
    Pid4 = peer:start(?DEFAULT_VARS, [Pid2]),
    Pid5 = peer:start(?DEFAULT_VARS, [Pid4]),

    Guard_fun = fun(_X) -> true end,
    Cmd_fun = fun(X) -> X end,

    error_logger:tty(false), % disable so error report of Pid1 crash doesn't popup
    peer:send(Pid1, Guard_fun, Cmd_fun),

    timer:sleep(?GOSSIP_EVERY * 7), % needed for letting communicate the processes

    Trans = request_to_all(give_me_all_transactions, [Pid2, Pid4, Pid5], 3,
        give_you_all_transactions),

    peer:stop(Pid2), % just to not let them crash (because tester exits)
    bully:stop(Pid3),
    peer:stop(Pid4),
    peer:stop(Pid5),

    ?assertNot(is_process_alive(Pid1)),

    ?assertEqual(hd(Trans), lists:nth(2, Trans)),
    ?assertEqual(lists:nth(2, Trans), lists:nth(3, Trans)),

    ?assertEqual([ % 4242 because bully always respond this seq_num for transactions
                   {transaction, {0, Pid1}, confirmed, {4242, Pid3}, Guard_fun, Cmd_fun}
                  ],
                  hd(Trans)
    ).


%%% =========================================
%%% integration tests
%%% =========================================
integration1() ->
    ?debugMsg("started integration1 test at " ++ my_time()),
    Pid1 = peer:start(?DEFAULT_VARS, []),
    Pid2 = peer:start(?DEFAULT_VARS, []),
    Pid3 = peer:start(?DEFAULT_VARS, []),
    Pid4 = peer:start(?DEFAULT_VARS, []),
    Pid5 = peer:start(?DEFAULT_VARS, []),
    support_server:start([Pid1, Pid2, Pid3, Pid4, Pid5]),

    Guard_fun = fun(_X) -> true end,
    Cmd_fun = fun(X) -> X end,

    peer:send(Pid1, Guard_fun, Cmd_fun),
    peer:send(Pid3, Guard_fun, Cmd_fun),
    peer:send(Pid5, Guard_fun, Cmd_fun),

    error_logger:tty(false), % disable so error report of Pid2 crash doesn't popup
    Pid2 ! crash,

    timer:sleep(?GOSSIP_EVERY * 18),

    Nodes = request_to_all(give_me_all_neighbors, [Pid1, Pid3, Pid4, Pid5], 4,
        give_you_all_neighbors),
    Trans = request_to_all(give_me_all_transactions, [Pid1, Pid3, Pid4, Pid5], 4,
        give_you_all_transactions),
    
    peer:stop(Pid1), % just to not let them crash (because tester exits)
    peer:stop(Pid3),
    peer:stop(Pid4),
    peer:stop(Pid5),
    support_server:stop(),

    ?assertNot(is_process_alive(Pid2)),

    ?assert(lists:any(fun(N) -> length(N) >= ?MIN_NEIGHBORS end, Nodes)),
    
    ?assertEqual(hd(Trans), lists:nth(2, Trans)),
    ?assertEqual(lists:nth(2, Trans), lists:nth(3, Trans)),
    ?assertEqual(lists:nth(3, Trans), lists:nth(4, Trans)),

    ?assertEqual(3, length(hd(Trans))),

    ?assert(all_confirmed(hd(Trans))),
    ?assert(all_id_unique(hd(Trans))),
    ?assert(all_seq_unique(hd(Trans))).


integration2() ->
    ?debugMsg("started integration2 test at " ++ my_time()),
    Pid1 = peer:start(?DEFAULT_VARS, []),
    Pid2 = peer:start(?DEFAULT_VARS, []),
    Pid3 = peer:start(?DEFAULT_VARS, []),
    Pid4 = peer:start(?DEFAULT_VARS, []),
    Pid5 = peer:start(?DEFAULT_VARS, []),
    Pid6 = peer:start(?DEFAULT_VARS, []),
    Pid7 = peer:start(?DEFAULT_VARS, []),
    Pid8 = peer:start(?DEFAULT_VARS, []),
    Pid9 = peer:start(?DEFAULT_VARS, []),
    Pid10 = peer:start(?DEFAULT_VARS, []),
    Pid11 = peer:start(?DEFAULT_VARS, []),
    Pid12 = peer:start(?DEFAULT_VARS, []),
    Pid13 = peer:start(?DEFAULT_VARS, []),
    Pid14 = peer:start(?DEFAULT_VARS, []),
    Pid15 = peer:start(?DEFAULT_VARS, []),
    Pid16 = peer:start(?DEFAULT_VARS, []),
    Pid17 = peer:start(?DEFAULT_VARS, []),
    Pid18 = peer:start(?DEFAULT_VARS, []),
    Pid19 = peer:start(?DEFAULT_VARS, []),
    Pid20 = peer:start(?DEFAULT_VARS, []),
    support_server:start([Pid1, Pid2, Pid19, Pid20]),

    Guard_fun = fun(_X) -> true end,
    Cmd_fun = fun(X) -> X end,

    timer:sleep(?CHECK_NEIGHBOR_EVERY * 7), % needed for letting communicate the processes
    
    peer:send(Pid1, Guard_fun, Cmd_fun),
    peer:send(Pid3, Guard_fun, Cmd_fun),
    peer:send(Pid5, Guard_fun, Cmd_fun),

    % crash of all the processes initially known by the support_server
    error_logger:tty(false), % disable so error report of crashes doesn't popup
    Pid1 ! crash,
    Pid2 ! crash,
    Pid19 ! crash,
    Pid20 ! crash,

    timer:sleep(?GOSSIP_EVERY * 10), % needed for letting communicate the processes

    Nodes = request_to_all(give_me_all_neighbors, [Pid3, Pid4, Pid5, Pid6, Pid7, Pid8, Pid9, Pid10,
        Pid11, Pid12, Pid13, Pid14, Pid15, Pid16, Pid17, Pid18], 16,
        give_you_all_neighbors),
    Trans = request_to_all(give_me_all_transactions, [Pid3, Pid4, Pid5, Pid6, Pid7, Pid8, Pid9,
        Pid10, Pid11, Pid12, Pid13, Pid14, Pid15, Pid16, Pid17, Pid18], 16,
        give_you_all_transactions),
    
    peer:stop(Pid3), % just to not let them crash (because tester exits)
    peer:stop(Pid4),
    peer:stop(Pid5),
    peer:stop(Pid6),
    peer:stop(Pid7),
    peer:stop(Pid8),
    peer:stop(Pid9),
    peer:stop(Pid10),
    peer:stop(Pid11),
    peer:stop(Pid12),
    peer:stop(Pid13),
    peer:stop(Pid14),
    peer:stop(Pid15),
    peer:stop(Pid16),
    peer:stop(Pid17),
    peer:stop(Pid18),
    support_server:stop(),

    ?assertNot(is_process_alive(Pid1)),
    ?assertNot(is_process_alive(Pid2)),
    ?assertNot(is_process_alive(Pid19)),
    ?assertNot(is_process_alive(Pid20)),

    ?assert(lists:any(fun(N) -> length(N) >= ?MIN_NEIGHBORS end, Nodes)),
    
    Confirmed_trans = lists:map(fun
        (Trans_list) -> lists:filter(fun(T) -> element(2, T) == confirmed end, Trans_list) end,
            Trans),
    ?assert(all_equals(Confirmed_trans)),

    ?assert(length(hd(Trans)) =< 3),

    ?assert(all_id_unique(hd(Trans))),
    ?assert(all_seq_unique(hd(Trans))).

init1() ->
    ?debugMsg("started init1 test at " ++ my_time()),

    Guard_fun = fun(_X) -> true end,
    Cmd_fun = fun(X) -> X end,

    Pid1 = peer:start(?DEFAULT_VARS,[],[{?ALWAYS, Guard_fun, Cmd_fun}],[]),
    Pid2 = peer:start(?DEFAULT_VARS,[Pid1]),
    Pid3 = peer:start(?DEFAULT_VARS,[Pid2]),
    Pid4 = peer:start(?DEFAULT_VARS,[Pid3]),
    Pid5 = peer:start(?DEFAULT_VARS,[Pid4]),
    Pid6 = peer:start(?DEFAULT_VARS,[Pid3],[{?ALWAYS, Guard_fun, Cmd_fun}],[]),

    timer:sleep(?GOSSIP_EVERY * 25), % needed for letting communicate the processes

    Trans = request_to_all(give_me_all_transactions, [Pid1, Pid2, Pid3, Pid4, Pid5, Pid6], 6,
        give_you_all_transactions),

    peer:stop(Pid1), % just to not let them crash (because tester exits)
    peer:stop(Pid2),
    peer:stop(Pid3),
    peer:stop(Pid4),
    peer:stop(Pid5),
    peer:stop(Pid6),

    ?assertEqual(hd(Trans), lists:nth(2, Trans)),
    ?assertEqual(lists:nth(2, Trans), lists:nth(3, Trans)),
    ?assertEqual(lists:nth(3, Trans), lists:nth(4, Trans)),
    ?assertEqual(lists:nth(4, Trans), lists:nth(5, Trans)),
    ?assertEqual(lists:nth(5, Trans), lists:nth(6, Trans)),

    ?assertEqual(2, length(hd(Trans))),

    ?assert(all_confirmed(hd(Trans))),
    ?assert(all_id_unique(hd(Trans))),
    ?assert(all_seq_unique(hd(Trans))).


init2() ->
    ?debugMsg("started init2 test at " ++ my_time()),

    Guard_fun = fun(_X) -> true end,
    Cmd_fun = fun(X) -> X end,

    Pid1 = peer:start(?DEFAULT_VARS,[],[{?ALWAYS, Guard_fun, Cmd_fun},{?ALWAYS, Guard_fun, Cmd_fun}],[]),
    Pid2 = peer:start(?DEFAULT_VARS,[Pid1]),
    Pid3 = peer:start(?DEFAULT_VARS,[Pid2]),
    Pid4 = peer:start(?DEFAULT_VARS,[Pid3]),
    Pid5 = peer:start(?DEFAULT_VARS,[Pid4]),
    Pid6 = peer:start(?DEFAULT_VARS,[Pid3],[{?ALWAYS, Guard_fun, Cmd_fun},{?ALWAYS, Guard_fun, Cmd_fun}],[]),

    timer:sleep(?GOSSIP_EVERY * 25), % needed for letting communicate the processes

    Trans = request_to_all(give_me_all_transactions, [Pid1, Pid2, Pid3, Pid4, Pid5, Pid6], 6,
        give_you_all_transactions),

    peer:stop(Pid1), % just to not let them crash (because tester exits)
    peer:stop(Pid2),
    peer:stop(Pid3),
    peer:stop(Pid4),
    peer:stop(Pid5),
    peer:stop(Pid6),

    ?assertEqual(hd(Trans), lists:nth(2, Trans)),
    ?assertEqual(lists:nth(2, Trans), lists:nth(3, Trans)),
    ?assertEqual(lists:nth(3, Trans), lists:nth(4, Trans)),
    ?assertEqual(lists:nth(4, Trans), lists:nth(5, Trans)),
    ?assertEqual(lists:nth(5, Trans), lists:nth(6, Trans)),

    ?assertEqual(4, length(hd(Trans))),

    ?assert(all_confirmed(hd(Trans))),
    ?assert(all_id_unique(hd(Trans))),
    ?assert(all_seq_unique(hd(Trans))).

init3() ->
    ?debugMsg("started init3 test at " ++ my_time()),

    Guard_fun = fun(_X) -> true end,
    Cmd_fun1 = fun(V) -> V#variables{y = V#variables.y + 1} end,
    Cmd_fun2 = fun(V) -> V#variables{y = V#variables.y * 3} end,

    Pid1 = peer:start(?DEFAULT_VARS,[],[{?ALWAYS, Guard_fun, Cmd_fun1},{?ALWAYS, Guard_fun, Cmd_fun1},{?ALWAYS, Guard_fun, Cmd_fun1}],[]),
    Pid2 = peer:start(?DEFAULT_VARS,[Pid1]),
    Pid3 = peer:start(?DEFAULT_VARS,[Pid2]),
    Pid4 = peer:start(?DEFAULT_VARS,[Pid3]),
    Pid5 = peer:start(?DEFAULT_VARS,[Pid4]),
    Pid6 = peer:start(?DEFAULT_VARS,[Pid3],[{?ALWAYS, Guard_fun, Cmd_fun2},{?ALWAYS, Guard_fun, Cmd_fun2},{?ALWAYS, Guard_fun, Cmd_fun2}],[]),

    timer:sleep(?GOSSIP_EVERY * 25), % needed for letting communicate the processes

    Trans = request_to_all(give_me_all_transactions, [Pid1, Pid2, Pid3, Pid4, Pid5, Pid6], 6,
        give_you_all_transactions),

    Vars = request_to_all(give_me_all_variables, [Pid1, Pid2, Pid3, Pid4, Pid5, Pid6], 6,
        give_you_all_variables),

    Execs = request_to_all(give_me_all_executeds, [Pid1, Pid2, Pid3, Pid4, Pid5, Pid6], 6,
        give_you_all_executeds),

    peer:stop(Pid1), % just to not let them crash (because tester exits)
    peer:stop(Pid2),
    peer:stop(Pid3),
    peer:stop(Pid4),
    peer:stop(Pid5),
    peer:stop(Pid6),

    ?assertEqual(hd(Trans), lists:nth(2, Trans)),
    ?assertEqual(lists:nth(2, Trans), lists:nth(3, Trans)),
    ?assertEqual(lists:nth(3, Trans), lists:nth(4, Trans)),
    ?assertEqual(lists:nth(4, Trans), lists:nth(5, Trans)),
    ?assertEqual(lists:nth(5, Trans), lists:nth(6, Trans)),

    ?assertEqual(6, length(hd(Trans))),
    ?assert(lists:all(fun(X) -> X == 6 end, Execs)),

    ?assert(all_confirmed(hd(Trans))),
    ?assert(all_id_unique(hd(Trans))),
    ?assert(all_seq_unique(hd(Trans))),


    ?assertEqual(hd(Vars), lists:nth(2, Vars)),
    ?assertEqual(lists:nth(2, Vars), lists:nth(3, Vars)),
    ?assertEqual(lists:nth(3, Vars), lists:nth(4, Vars)),
    ?assertEqual(lists:nth(4, Vars), lists:nth(5, Vars)),
    ?assertEqual(lists:nth(5, Vars), lists:nth(6, Vars)),

    ?assert(0 =< (hd(Vars))#variables.y),
    ?assert(81 >= (hd(Vars))#variables.y).

    loop_exec1() ->
    ?debugMsg("started loop_exec1 test at " ++ my_time()),

    Guard_fun = fun(V) -> V#variables.x < 5 end,
    Cmd_fun = fun(V) -> V#variables{x = V#variables.x + 1} end,

    Pid1 = peer:start(?DEFAULT_VARS,[],[],[{?ALWAYS, Guard_fun, Cmd_fun}]),
    Pid2 = peer:start(?DEFAULT_VARS,[Pid1]),
    Pid3 = peer:start(?DEFAULT_VARS,[Pid2]),
    Pid4 = peer:start(?DEFAULT_VARS,[Pid3]),
    Pid5 = peer:start(?DEFAULT_VARS,[Pid4]),
    Pid6 = peer:start(?DEFAULT_VARS,[Pid3],[],[{?ALWAYS, Guard_fun, Cmd_fun}]),

    timer:sleep(?GOSSIP_EVERY * 25), % needed for letting communicate the processes

    Vars = request_to_all(give_me_all_variables, [Pid1, Pid2, Pid3, Pid4, Pid5, Pid6], 6,
        give_you_all_variables),

    peer:stop(Pid1), % just to not let them crash (because tester exits)
    peer:stop(Pid2),
    peer:stop(Pid3),
    peer:stop(Pid4),
    peer:stop(Pid5),
    peer:stop(Pid6),

    ?assertEqual(hd(Vars), lists:nth(2, Vars)),
    ?assertEqual(lists:nth(2, Vars), lists:nth(3, Vars)),
    ?assertEqual(lists:nth(3, Vars), lists:nth(4, Vars)),
    ?assertEqual(lists:nth(4, Vars), lists:nth(5, Vars)),
    ?assertEqual(lists:nth(5, Vars), lists:nth(6, Vars)),

    ?assertEqual(5, (hd(Vars))#variables.x),
    ?assertEqual(0, (hd(Vars))#variables.y).

loop_exec2() ->
    ?debugMsg("started loop_exec2 test at " ++ my_time()),

    All = ?ALWAYS,
    Guard_fun = fun(V) -> V#variables.x < 5 end,
    Cmd_fun = fun(V) -> V#variables{x = V#variables.x + 1} end,

    Pid1 = peer:start(?DEFAULT_VARS,[],[],[{Guard_fun, All, Cmd_fun}]),
    Pid2 = peer:start(?DEFAULT_VARS,[Pid1]),
    Pid3 = peer:start(?DEFAULT_VARS,[Pid2]),
    Pid4 = peer:start(?DEFAULT_VARS,[Pid3]),
    Pid5 = peer:start(?DEFAULT_VARS,[Pid4]),
    Pid6 = peer:start(?DEFAULT_VARS,[Pid3],[],[]),

    timer:sleep(?GOSSIP_EVERY * 25), % needed for letting communicate the processes

    Vars = request_to_all(give_me_all_variables, [Pid1, Pid2, Pid3, Pid4, Pid5, Pid6], 6,
        give_you_all_variables),

    peer:stop(Pid1), % just to not let them crash (because tester exits)
    peer:stop(Pid2),
    peer:stop(Pid3),
    peer:stop(Pid4),
    peer:stop(Pid5),
    peer:stop(Pid6),

    ?assertEqual(hd(Vars), lists:nth(2, Vars)),
    ?assertEqual(lists:nth(2, Vars), lists:nth(3, Vars)),
    ?assertEqual(lists:nth(3, Vars), lists:nth(4, Vars)),
    ?assertEqual(lists:nth(4, Vars), lists:nth(5, Vars)),
    ?assertEqual(lists:nth(5, Vars), lists:nth(6, Vars)),

    ?assertEqual(5, (hd(Vars))#variables.x),
    ?assertEqual(0, (hd(Vars))#variables.y).


%%% ===============================================================================================
%%% support functions
%%% ===============================================================================================

%% @private
%% @doc Send a specified message to all indicated processes.
request_to_all(Req, [Node | Other], Num, Res) ->
    Node ! {Req, self()},
    request_to_all(Req, Other, Num, Res);
request_to_all(_Req, [], Num, Res) -> collect_all_responses(Num, Res, []).


%% @private
%% @doc Collect specified responses from a inserted number of processes.
collect_all_responses(Num, Responses, Received) ->
    if
        Num == 0 ->
            Received;
        true ->
            receive
                {Responses, Value} ->
                    collect_all_responses(Num - 1, Responses, Received ++ [Value]);
                _ ->
                    collect_all_responses(Num, Responses, Received)
            after 120000 -> % 2 minute
                    error("Missing " ++ integer_to_list(Num) ++ " responses")
            end
    end.


%% @private
%% @doc Verify if all passed transactions are confirmed.
all_confirmed(Trans) -> lists:any(fun(T) -> element(3, T) == confirmed end, Trans).


%% @private
%% @doc Verify if all passed transactions have unique ids.
all_id_unique([]) -> true;
all_id_unique([First | Other]) ->
    Is_unique = not lists:any(fun(T) -> element(2, T) == element(2, First) end, Other),
    if
        Is_unique ->
            all_id_unique(Other);
        true ->
            false
     end.


%% @private
%% @doc Verify if all passed transactions have unique sequence number.
all_seq_unique([]) -> true;
all_seq_unique([First | Other]) ->
    Is_unique = not lists:any(fun(T) -> element(4, T) == element(4, First) end, Other),
    if
        Is_unique ->
            all_seq_unique(Other);
        true ->
            false
     end.


%% @private
%% @doc Verify if all passed transactions are equal.
all_equals([]) -> true; % because the function can be called directly with [] as argument
all_equals([_ | []]) -> true;
all_equals([First | Other]) ->
    if
        First == hd(Other) ->
            all_equals(Other);
        true ->
            false
    end.


%% @private
%% @doc Return a string representing actual date and time. 
my_time() ->
    {H, M, S} = time(),
    io_lib:format('~2..0b:~2..0b:~2..0b', [H, M, S]).