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
         row/3,

         copy_row/4,
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

-spec row(pid(), Sheet, R) -> #xmlElement{} when 
      Sheet :: string(),
      R :: non_neg_integer().
row(PID, SheetName, R) -> 
    Sheet = sheet(PID, SheetName),
    Formulas0 = xmerl_xpath:string("//table:table-cell[@table:formula]", Sheet),
    Formulas1 = [erlodf_xml:update_value(Cell, "") || Cell <- Formulas0],
    Formulas2 = [erlodf_xml:update_attribute('office:value', Cell, "") || Cell <- Formulas1],
    [erlodf_document:update_body(PID, Formula) || Formula <- Formulas2],
    get_nth_with_repeated(Sheet, 'table:number-rows-repeated', ".//table:table-row", R, PID).


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
    Row = row(PID, SheetName, R),
    get_nth_with_repeated(Row, 'table:number-columns-repeated', ".//table:table-cell | //table:covered-table-cell", C, PID).

-spec copy_row(PID, SheetName, RowNumber, RowsToAdd) ->  PID when
      PID :: pid(),
      SheetName :: string() | non_neg_integer(),
      RowNumber :: non_neg_integer(),
      RowsToAdd :: non_neg_integer().
copy_row(PID, SheetName, RowNumber, RowsToAdd) -> 
    Row0 = row(PID, SheetName, RowNumber),
    Rows = lists:duplicate(RowsToAdd + 1, Row0),
    erlodf_document:update_body(PID, Rows),
    PID.

get_cell(PID, Sheet, RC) ->
    Cell = cell(PID, Sheet, RC),
    Type = erlodf_xml:attribute('office:value-type', Cell, "text"),
    {ok, Val} = erlodf_xml:value(Cell),
    {ok, erlodf_xml:get_with_type(Val, Type)}.

set_cell(PID, Sheet, Cell, Value) when is_float(Value); is_integer(Value) ->
    set_cell(PID, Sheet, Cell, Value, float);
set_cell(PID, Sheet, Cell, Value) when is_boolean(Value) ->
    set_cell(PID, Sheet, Cell, Value, boolean);
set_cell(PID, Sheet, Cell, Value) ->
    set_cell(PID, Sheet, Cell, Value, string).

set_cell(PID, Sheet, Cell, Value, Type) ->
    Cell0 = cell(PID, Sheet, Cell),
    Cell1 = erlodf_xml:update_value(Cell0, Value),
    Cell2 = erlodf_xml:update_attribute('office:value-type', Cell1, Type),
    Cell3 = erlodf_xml:update_attribute('calcext:value-type', Cell2, Type),
    erlodf_document:update_body(PID, update_cell_value(Cell3, Value, Type)),
    PID.

get_nth_with_repeated(BaseNode, Tag, XPath, R, PID) -> 
    Nodes0 = lists:keysort(#xmlElement.pos,
                           xmerl_xpath:string(XPath, BaseNode)),    
    Nodes1 = update_tree(
               fix_pos(
                 maybe_add_node(
                   lists:foldl(fun(C, A) -> unpack(C, A, Tag, R) end, [], Nodes0),
                  R)),
              Nodes0, PID),
    lists:nth(R, Nodes1).

unpack(Current, Acc, _Tag, R) when length(Acc) >= R ->
   [Current | Acc];
unpack(Current, Acc, Tag, _R) ->
    Repeated = list_to_integer(
                 erlodf_xml:attribute(Tag, Current, "1")),
    CurrentU = erlodf_xml:update_attribute(Tag, Current, "1"),
    lists:duplicate(Repeated, CurrentU) ++ Acc.
   
maybe_add_node(Nodes, Length) when length(Nodes) >= Length ->
    Nodes;
maybe_add_node([Node | _] = Nodes, Length) ->
    lists:duplicate(Length - length(Nodes), Node) ++ Nodes.

fix_pos(Nodes0) ->
    [Node0|_] = Nodes = lists:reverse(Nodes0),
    Pos0 = Node0#xmlElement.pos,
    [Node#xmlElement{pos=Pos} || {Pos, Node} <- lists:zip(lists:seq(Pos0, Pos0 - 1 + length(Nodes)), Nodes)].

filter_nodes(Nodes, Nodes0) ->
    NodeSet = sets:from_list(Nodes0),
    [N || N <- Nodes, not sets:is_element(N, NodeSet)].

update_tree(Nodes, Nodes0, _PID) when length(Nodes0) == length(Nodes) ->
    Nodes;
update_tree(Nodes, Nodes0, PID) ->
    Nodes1 = filter_nodes(Nodes, Nodes0),
    erlodf_document:update_body(PID, Nodes1),
    Nodes.

update_cell_value(Cell, Value, float) ->
    Value1 = lists:flatten(io_lib:format("~p", [Value])),
    erlodf_xml:update_attribute('office:value', Cell, Value1);
update_cell_value(Cell, Value, boolean) ->
    erlodf_xml:update_attribute('office:boolean-value', Cell, Value);
update_cell_value(Cell, Value, string) ->
    erlodf_xml:update_attribute('office:string-value', Cell, Value);
update_cell_value(Cell, _Value, _) ->
    Cell.
