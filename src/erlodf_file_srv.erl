%%%-------------------------------------------------------------------
%%% @author ins
%%% @copyright (C) 2017, ins
%%% @doc
%%%
%%% @end
%%% Created : 2017-09-29 11:07:43.355120
%%%-------------------------------------------------------------------
-module(erlodf_file_srv).

-behaviour(gen_server).

%% API
-export([
         start_link/1,
         tag/2,
         update/2,
         flash/1,
         save/1
        ]).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("odf.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(FileTuple) ->
    gen_server:start_link(?MODULE, [FileTuple], []).

tag(PID, Tag) ->
    gen_server:call(PID, {tag, Tag}).

update(PID, Node) ->
    gen_server:call(PID, {update, Node}).

flash(PID) ->
    gen_server:call(PID, flash).

save(PID) -> 
    gen_server:call(PID, save, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([{Filename, Data}]) ->
    {XML, _R} = xmerl_scan:string(binary_to_list(Data)),
    {ok, #odf_file{
            name=Filename,
            data=Data,
            xml=XML
           }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({update, Node}, _From, State) ->
    {reply, Node, handle_update(Node, State)};
handle_call(flash, _From, State) ->
    {reply, ok, handle_flash(State)};
handle_call({tag, TagName}, _From, State) ->
    {reply, handle_tag(TagName, State), State};
handle_call(save, _From, State) ->
    {reply, handle_save(State), State};
handle_call(Request, _From, State) ->
    lager:warning("Wrong call ~p in ~p", [Request, ?MODULE]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({update, Node}, State) ->
    {noreply, handle_update(Node, State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_tag(TagName, #odf_file{xml=XML}) ->
    {ok, xmerl_xpath:string(TagName, XML)}.

handle_update(Node, #odf_file{xml=XML}=State) ->
    XML1 = erlodf_xml:update_tree(XML, Node),
    State#odf_file{xml=XML1, modified=true}.

handle_flash(#odf_file{xml=XML}=State) ->
    Data = unicode:characters_to_binary(xmerl:export_simple([XML], xmerl_xml)),
    {XML2, _R} = xmerl_scan:string(binary_to_list(Data)),
    State#odf_file{xml=XML2, modified=false, data=Data}.

handle_save(#odf_file{name=Filename, modified=false, data=Data}) ->
    {Filename, Data};
handle_save(#odf_file{name=Filename, data=Data, modified=true, xml=XML}) ->
    Binary = unicode:characters_to_binary(xmerl:export_simple([XML], xmerl_xml)),
    save_debug(XML, Filename, Binary, Data),
    {Filename, Binary}.

-ifndef(DEBUG).
save_debug(XML, Filename, Binary, Data) ->
    {ok, F} = file:open("content.erl", [write]),
    io:format(F, "~p", [XML]),
    file:close(F),
    file:write_file("out/" ++ Filename, Binary),
    file:write_file("in/" ++ Filename, Data).
-else.
save_debug(_XML, _Filename, _Binary, _Data) ->
    ok.
-endif.



