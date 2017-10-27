%%%-------------------------------------------------------------------
%%% @author ins
%%% @copyright (C) 2017, ins
%%% @doc
%%%
%%% @end
%%% Created : 2017-10-05 13:15:05.861063
%%%-------------------------------------------------------------------
-module(erlodf_spreadsheet).

%% API
-export([
         sheets/1,
         sheet_names/1,
         sheet/2,
         cell/2,
         cell/3,
         get_cell/3,
         set_cell/5
        ]).


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
sheets(PID) ->
    {ok, [Body]} = erlodf_document:body(PID),
    xmerl_xpath:string("//table:table", Body).

sheet_names(PID) ->
    {ok, [Body]} = erlodf_document:body(PID),
    Sheets = xmerl_xpath:string("//table:table/@table:name", Body),
    [S#xmlAttribute.value || S <- Sheets].

sheet(PID, N) when is_integer(N) ->
    lists:nth(N, sheets(PID));
sheet(PID, Name) when is_list(Name) ->
    {ok, [Body]} = erlodf_document:body(PID),
    [Sheet] = xmerl_xpath:string(lists:flatten(io_lib:format("//table:table[@table:name='~s']", [Name])), Body),
    Sheet.

-spec cell(pid(), Sheet, {R, C}) -> #xmlElement{} when 
      Sheet :: string(),
      R :: non_neg_integer(),
      C :: non_neg_integer().
cell(PID, Sheet, RC) ->
    cell(sheet(PID, Sheet), RC).

-spec cell(Sheet, {R, C}) -> #xmlElement{} when 
      Sheet :: #xmlElement{},
      R :: non_neg_integer(),
      C :: non_neg_integer().
cell(Sheet, [RL, CN]) -> 
    [RU] = string:uppercase([RL]),
    RC = {CN - $0, RU - $A + 1},
    cell(Sheet, RC);
cell(Sheet, {R, C}) -> 
    Row = lists:nth(R, xmerl_xpath:string(".//table:table-row", Sheet)),
    lists:nth(C, xmerl_xpath:string(".//table:table-cell", Row)).

get_cell(PID, Sheet, RC) ->
    Cell = cell(PID, Sheet, RC),
    erlodf_xml:value(Cell).

set_cell(PID, Sheet, Cell, Value, Type) ->
    Cell0 = cell(PID, Sheet, Cell),
    Cell1 = erlodf_xml:update_value(Cell0, Value, Type),
    erlodf_document:update_body(PID, Cell1).
