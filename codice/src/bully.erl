%% @author Massimiliano De Luise
%% @author Massimo Comuzzo
%% @doc Module used for testing, pretends to be a process monitor of process who wants to create a
%% transaction, and then make it crash (before or after the transaction is done).
-module(bully).

-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2]).
-export([start/3, stop/1]).

-include("debug.hrl").

-type(bully_status() :: {before_monitoring | before_trans_done | after_trans_done, integer()}).




%% @doc Create a new bully process. When_make_sender_crash indicates when the sender process must
%% crash, it can be before become a monitor to it (before_monitoring), after became its monitor
%% but before the transaction is done (before_trans_done), or after after the transaction is done
%% (before_trans_done).
-spec start(Pid:: pid(), When_make_sender_crash:: before_monitoring | before_trans_done |
    after_trans_done, Timer:: integer()) -> pid().
start(Pid, When_make_sender_crash, Timer) ->
    process_flag(trap_exit, true),
    {ok, Result} = gen_server:start_link(?MODULE, {Pid, When_make_sender_crash, Timer}, []),
    Result.


%% @doc Needed for the gen_server behaviour, create a new bully process and make it link to one
%% other process (that one that will be the sender for a transaction).
%% @see peer:handle_cast/3 for detail about the action done by the peer after receiving a
%% link_require request.
-spec init({pid(), integer(), Timer:: integer()}) -> {ok, bully_status()}.
init({Pid, When_make_sender_crash, Timer}) ->
    ?DBG("starting bully"),
    process_flag(trap_exit, true),
    gen_server:cast(Pid, {link_require, self()}),
    {ok, {Pid, When_make_sender_crash, Timer}}.


%% @doc stop the running bully process with specified Pid.
-spec stop(pid()) -> ok.
stop(Pid) -> gen_server:call(Pid, stop).
    

%% @doc Needed for the gen_server behaviour, when a asynchronous require is send to the
%% support_server, it response and then update its status if necessary.
%
% If receiving monitor_request from process initially given, then after a timer:
% * if When_make_sender_crash == before_monitoring, then make it crash sending it crash atom
% * if When_make_sender_crash /= before_monitoring, then respond monitor_response to it
-spec handle_cast(any(), bully_status()) -> {noreply, bully_status()}.
handle_cast({monitor_request, Pid, _}, {Pid, When_make_sender_crash, Timer}) ->
    ?DBG("received monitor_request"),
    timer:sleep(Timer),
    if
        When_make_sender_crash == before_monitoring ->
            Pid ! crash;
        true ->
            gen_server:cast(Pid, {monitor_response, yes, self()})
    end,
    {noreply, {Pid, When_make_sender_crash, Timer}};


% If receiving propose_trans then after a timer:
% * if When_make_sender_crash == before_trans_done and the message was send from process initially
%   given, then make it crash sending it crash atom
% * if When_make_sender_crash /= before_monitoring, then respond monitor_response to it
handle_cast({propose_trans, Asker, Trans}, {Pid, When_make_sender_crash, Timer}) ->
    ?DBG("received propose_trans"),
    timer:sleep(Timer),
    if
        (When_make_sender_crash == before_trans_done) and (Asker == Pid) ->
            Pid ! crash;
        true ->
            gen_server:cast(Asker, {trans_response, element(2, Trans), self(), {4242, self()}})
    end,
    {noreply, {Pid, When_make_sender_crash, Timer}};


% If receiving trans_done from process initially given (thus I'm not the last monitor of that
% process), then after a timer:
% * if When_make_sender_crash == after_trans_done and the message was send from process initially
%   given, then make it crash sending it crash atom
% * if When_make_sender_crash /= after_trans_done, then respond monitor_response to it
handle_cast({trans_done, Pid}, {Pid, When_make_sender_crash, Timer}) ->
    gen_server:cast(Pid, {trans_ack, self()}),
    timer:sleep(Timer),
    if
        When_make_sender_crash == after_trans_done ->
            Pid ! crash;
        true ->
            ok
    end,
    {noreply, {Pid, When_make_sender_crash, Timer}};


% If receiving trans_confirm from process initially given (thus I'm the last monitor of that
% process), then after a timer:
% * if When_make_sender_crash == after_trans_done and the message was send from process initially
%   given, then make it crash sending it crash atom
% * if When_make_sender_crash /= after_trans_done, then respond monitor_response to it
handle_cast({trans_confirm, _Tran, Pid}, {Pid, When_make_sender_crash, Timer}) ->
    timer:sleep(Timer),
    if
        When_make_sender_crash == after_trans_done ->
            Pid ! crash;
        true ->
            ok
    end,
    {noreply, {Pid, When_make_sender_crash, Timer}};


% If a node send an unknown message via cast, then don't do anything.
handle_cast(_Msg, {Pid, When_make_sender_crash, Timer}) ->
    ?DBG("received unknown message"),
    {noreply, {Pid, When_make_sender_crash, Timer}}.


% If a node send me a stop message, then exit.
-spec handle_call(stop, pid(), bully_status()) -> {stop, normal, ok, bully_status()}.
handle_call(stop, _Pid, {Pid, When_make_sender_crash, Timer}) ->
    ?DBG("stopping"),
    {stop, normal, ok, {Pid, When_make_sender_crash, Timer}}.


% If a node send an unknown message, then don't do anything.
-spec handle_info(any(), bully_status()) -> {noreply, bully_status()}.
handle_info(_,  {Pid, When_make_sender_crash, Timer}) ->
    {noreply, {Pid, When_make_sender_crash, Timer}}.