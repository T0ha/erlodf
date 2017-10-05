%%%-------------------------------------------------------------------
%%% @author ins
%%% @copyright (C) 2017, ins
%%% @doc
%%%
%%% @end
%%% Created : 2017-09-29 11:35:09.047211
%%%-------------------------------------------------------------------
-module(erlodf_document_sup).

-behaviour(supervisor).

-define(CHILD(Id, Mod, Type, Args),
        {Id, {Mod, start_link, Args},
         permanent, 2000, Type, [Mod]}).

%% API
-export([
         start_link/1,
         add_files/2,
         get_document_pid/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Path) ->
    supervisor:start_link(?MODULE, [Path]).

add_files(Pid, Files) ->
    Children = ?CHILD(Files, erlodf_files_sup, supervisor, [Files]),
    %lager:info("Starting sup: ~p", [Children]),
    supervisor:start_child(Pid, Children).

get_document_pid(PID) ->
    {_, DPID, worker, [erlodf_document]} = lists:keyfind([erlodf_document], 4, supervisor:which_children(PID)),
    DPID.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([Path]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    AChild = ?CHILD(Path, erlodf_document, worker, [Path, self()]),

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



