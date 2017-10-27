-module(spreadsheet_SUITE).
-export([
         all/0,
         init_per_testcase/2,
         end_per_testcase/2,
         sheets_test/1,
         sheet_by_number_test/1,
         sheet_by_name_test/1,
         cell_tuple_test/1,
         cell_letter_test/1,
         get_cell_test/1,
         change_cell_test/1,
         set_cell_test/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("xmerl/include/xmerl.hrl").

all() ->
    [
     sheets_test,
     sheet_by_name_test,
     sheet_by_number_test,
     cell_tuple_test,
     cell_letter_test,
     get_cell_test,
     change_cell_test,
     set_cell_test
    ].

init_per_testcase(_, C) ->
    application:ensure_all_started(erlodf),
    C.

end_per_testcase(_, C) ->
    application:stop(erlodf),
    C.

sheets_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, Document} = erlodf:open(Path),
    [Sheet] = erlodf_spreadsheet:sheets(Document).
    %?assert(is_record(xmlElement, Sheet)).

sheet_by_name_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, Document} = erlodf:open(Path),
    Sheet = erlodf_spreadsheet:sheet(Document, "tradeHistory-5"),
    ?assertEqual(Sheet#xmlElement.name, 'table:table').
    

sheet_by_number_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, Document} = erlodf:open(Path),
    Sheet = erlodf_spreadsheet:sheet(Document, 1),
    ?assertEqual(Sheet#xmlElement.name, 'table:table').


cell_tuple_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, Document} = erlodf:open(Path),
    Sheet = erlodf_spreadsheet:sheet(Document, 1),
    Cell = erlodf_spreadsheet:cell(Sheet, {1,2}),
    {ok, Text} = erlodf_xml:value(Cell),
    ?assertEqual('table:table-cell', Cell#xmlElement.name),
    ?assertEqual("Market", Text).

cell_letter_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, Document} = erlodf:open(Path),
    Sheet = erlodf_spreadsheet:sheet(Document, 1),
    Cell = erlodf_spreadsheet:cell(Sheet, "B3"),
    {ok, Text} = erlodf_xml:value(Cell),
    ?assertEqual('table:table-cell', Cell#xmlElement.name),
    ?assertEqual("XMR/BTC", Text).

get_cell_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, Document} = erlodf:open(Path),
    {ok, Text} = erlodf_spreadsheet:get_cell(Document, 1, "B2"),
    ?assertEqual("BTC/USDT", Text).

change_cell_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, Document} = erlodf:open(Path),
    Sheet = erlodf_spreadsheet:sheet(Document, 1),
    Cell0 = erlodf_spreadsheet:cell(Sheet, {1,2}),
    {ok, Text0} = erlodf_xml:value(Cell0),
    ?assertEqual('table:table-cell', Cell0#xmlElement.name),
    ?assertEqual("Market", Text0),
    Document = erlodf_spreadsheet:set_cell(Document, 1, {1, 2}, "Check Me", text),
    {ok, Text1} = erlodf_spreadsheet:get_cell(Document, 1, {1, 2}),
    ?assertEqual("Check Me", Text1).

set_cell_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, Document} = erlodf:open(Path),
    Sheet = erlodf_spreadsheet:sheet(Document, 1),
    Cell0 = erlodf_spreadsheet:cell(Sheet, "A5"),
    empty = erlodf_xml:value(Cell0),
    ?assertEqual('table:table-cell', Cell0#xmlElement.name),
    %?assertEqual("", Text0),
    Document = erlodf_spreadsheet:set_cell(Document, 1, "A5", "Check Me", text),
    {ok, Text1} = erlodf_spreadsheet:get_cell(Document, 1, "A5"),
    ?assertEqual("Check Me", Text1).
