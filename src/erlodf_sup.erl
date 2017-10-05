%%%-------------------------------------------------------------------
%% @doc erlodf top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('erlodf_sup').

-behaviour(supervisor).

%% API
-export([
         start_link/0,
         open_document/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(Id, Mod, Type, Args),
        {Id, {Mod, start_link, Args},
         permanent, 2000, Type, [Mod]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

open_document(Fname) ->
    supervisor:start_child(?MODULE, [Fname]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    erlodf_documents = ets:new(erlodf_documents, [set, named_table, public]),
    {ok, { {simple_one_for_one, 0, 1}, [?CHILD(undefined, erlodf_document_sup, supervisor, [])]} }.

%%====================================================================
%% Internal functions
%%====================================================================
