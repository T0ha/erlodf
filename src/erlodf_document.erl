%%%-------------------------------------------------------------------
%%% @author ins
%%% @copyright (C) 2017, ins
%%% @doc
%%%
%%% @end
%%% Created : 2017-09-28 15:06:40.259372
%%%-------------------------------------------------------------------
-module(erlodf_document).

-behaviour(gen_server).

%% API
-export([
         start_link/2,
         body/1,
         update_body/2,
         style/1,
         save/1,
         close/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-include("odf.hrl").


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
start_link(Filename, Sup) ->
    gen_server:start_link(?MODULE, [Filename, Sup], []).

body(Pid) -> 
    gen_server:call(Pid, body).

update_body(_Pid, []) -> 
    [];
update_body(Pid, Node) -> 
    gen_server:call(Pid, {update_body, Node}).

style(Pid) -> 
    gen_server:call(Pid, style).

save(Pid) -> 
    {ok, {_, Binary}} = gen_server:call(Pid, save),
    Binary.

close(Pid) -> 
    gen_server:cast(Pid, close).

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
init([<<"PK", _/bytes>> = Data, Parent]) ->
    Filename = crypto:hash(sha256, Data),
    ets:insert(erlodf_documents, {Filename, self()}),
    {ok, #odf_document{data=Data,
                       format=zip,
                       document_sup=Parent}, 0};

init([Filename, Parent]) ->
    ets:insert(erlodf_documents, {Filename, self()}),
    case filename:extension(Filename) of
        [$., $f |_ ] -> 
            {ok, #odf_document{path=Filename,
                               format=xml,
                               document_sup=Parent}, 0};
        _ ->
            {ok, #odf_document{path=Filename,
                               format=zip,
                               document_sup=Parent}, 0}
    end.

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
handle_call({update_body, Node}, _From, #odf_document{document_sup=DocumentSup,
                                                      files_sup=FilesSup}=State) ->
    PID = get_file("content.xml", DocumentSup, FilesSup),
    Reply = erlodf_file_srv:update(PID, Node),
    {reply, Reply, State};
handle_call(save, _From, #odf_document{path=Filename, 
                                       files=OldFiles,
                                       files_sup=FilesSup}=State) ->

    FilePs = supervisor:which_children(FilesSup),
    NewFiles = lists:keysort(1, [erlodf_file_srv:save(Pid) || {_, Pid, worker, _} <- FilePs]),
    Files = lists:ukeymerge(1, NewFiles, lists:keysort(1, OldFiles)),
    %io:format("FIles: ~p", [[X || {X, _} <- Files]]),
    Zip = zip:create(Filename, Files, [memory]),
    {reply, Zip, State};

handle_call(body, _From, #odf_document{document_sup=DocumentSup, 
                                       files_sup=FilesSup}=State) ->
    PID = get_file("content.xml", DocumentSup, FilesSup),
    Body = erlodf_file_srv:tag(PID, "office:body"),
    {reply, Body, State};

handle_call(style, _From, #odf_document{document_sup=DocumentSup, 
                                        files_sup=FilesSup}=State) ->
    PID = get_file("style.xml", DocumentSup, FilesSup),
    Body = erlodf_file_srv:tag(PID, "office:document-styles"),
    {reply, Body, State};
handle_call(_Request, _From, State) ->
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
handle_cast(close, #odf_document{document_sup=DocumentSup,
                                 files_sup=FilesSup}=State) ->
    supervisor:terminate_child(erlodf_sup, DocumentSup), 
    supervisor:delete_child(erlodf_sup, DocumentSup), 
    {stop, normal, State};
handle_cast({update_body, Node}, #odf_document{document_sup=DocumentSup,
                                               files_sup=FilesSup}=State) ->
    PID = get_file("content.xml", DocumentSup, FilesSup),
    erlodf_file_srv:update(PID, Node),
    {noreply, State};
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
handle_info(timeout, #odf_document{path=Data,
                                   data=undefined,
                                   document_sup=Parent,
                                   format=zip}=State) ->
    open(Data, State);

handle_info(timeout, #odf_document{path=undefined,
                                   data=Data,
                                   document_sup=Parent,
                                   format=zip}=State) ->
    open(Data, State);
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
open(Data, #odf_document{document_sup=Parent}=State) ->
    Targets = ["content.xml", "styles.xml", "meta.xml", "settings.xml"],
    {ok, AllFiles} = zip:extract(Data, [memory]), %, {file_list, Targets}]),
    Files = [F || {N, _} = F <- AllFiles, lists:member(N, Targets)],
    %lager:info("Extracted: ~p", [Files]),
    {ok, Sup} = erlodf_document_sup:add_files(Parent, Files),
    %lager:info("Started: ~p", [Sup]),
    {noreply, State#odf_document{
                files=AllFiles,
                files_sup=Sup
               }}.
get_file(Fname, DocumentSup, FilesSup) ->
    Files = supervisor:which_children(FilesSup),
    {{file, _, Fname}, PID, worker, _} = lists:keyfind({file, DocumentSup, Fname}, 1, Files),
    PID.
