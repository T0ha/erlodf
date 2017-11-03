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
         cell/3,
         get_cell/3,
         set_cell/4,
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

-spec cell(pid(), Sheet, RC) -> #xmlElement{} when 
      Sheet :: string(),
      RC :: {R, C} | string(),
      R :: non_neg_integer(),
      C :: non_neg_integer().
cell(PID, Sheet, [RL | CN]) -> 
    [RU] = string:uppercase([RL]),
    RC = {list_to_integer(CN), RU - $A + 1},
    cell(PID, Sheet, RC);
cell(PID, SheetName, {R, C}) -> 
    Sheet = sheet(PID, SheetName),
    Row = fix_multiple(
            get_nth_with_repeated('table:number-rows-repeated', xmerl_xpath:string(".//table:table-row", Sheet), R), 
            PID),
    fix_multiple(
      get_nth_with_repeated('table:number-columns-repeated', xmerl_xpath:string(".//table:table-cell | //table:covered-table-cell", Row), C),
      PID).

get_cell(PID, Sheet, RC) ->
    Cell = cell(PID, Sheet, RC),
    erlodf_xml:value(Cell).

set_cell(PID, Sheet, Cell, Value) ->
    set_cell(PID, Sheet, Cell, Value, text).

set_cell(PID, Sheet, Cell, Value, Type) ->
    Cell0 = cell(PID, Sheet, Cell),
    Cell1 = erlodf_xml:update_value(Cell0, Value, Type),
    erlodf_document:update_body(PID, Cell1),
    PID.

get_nth_with_repeated(Tag, Nodes, R) -> 
    lists:foldl(
      fun(Current, N) when is_integer(N), N >= R ->
              Current;
         (_, #xmlElement{}=New) ->
              New;
         (_, New) when is_list(New) ->
              New;
         (#xmlElement{pos=XMLPos}=Current, N) -> 
              Repeated = list_to_integer(
                           erlodf_xml:attribute(Tag, Current, "1")),

              case N + Repeated of
                  Next when Next < R ->
                      io:format("Next1: ~p~n", [Next]),
                      Next;
                  Next when Next == R ->
                      io:format("Next2: ~p~n", [Next]),
                      Current;
				  Next when Next > R ->
                      io:format("Next3: ~p~n", [Next]),
                      Pos = lists:zip(lists:seq(0, 2), [ R - N - 1, 1, Repeated - R + N - 1 ]),
                      lists:map(
                        fun({P, Rpt}) ->
                                PStr = integer_to_list(Rpt),
                                erlodf_xml:update_attribute(Tag, Current#xmlElement{pos=XMLPos + P}, PStr)
                        end,
                        Pos)
              end
      end,
      0,
      Nodes).

fix_multiple(Nodes, PID) when is_list(Nodes) ->
    lists:foreach(fun(Node) ->
                          erlodf_document:update_body(PID, Node)
                  end,
                  Nodes),
    lists:nth(2, Nodes);
fix_multiple(#xmlElement{}=Node, _PID) ->
    Node.
