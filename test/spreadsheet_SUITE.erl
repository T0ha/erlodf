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
         get_merged_cols_cell_test/1,
         get_packed_cols_cell_test/1,
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
     get_merged_cols_cell_test,
     get_packed_cols_cell_test,
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
    Cell = erlodf_spreadsheet:cell(Document, 1, {1,2}),
    {ok, Text} = erlodf_xml:value(Cell),
    ?assertEqual('table:table-cell', Cell#xmlElement.name),
    ?assertEqual("Market", Text).

cell_letter_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, Document} = erlodf:open(Path),
    Cell = erlodf_spreadsheet:cell(Document, 1, "B3"),
    {ok, Text} = erlodf_xml:value(Cell),
    ?assertEqual('table:table-cell', Cell#xmlElement.name),
    ?assertEqual("XMR/BTC", Text).

get_cell_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, Document} = erlodf:open(Path),
    {ok, Text} = erlodf_spreadsheet:get_cell(Document, 1, "B2"),
    ?assertEqual("BTC/USDT", Text).

get_merged_cols_cell_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, Document} = erlodf:open(Path),
    {ok, Text} = erlodf_spreadsheet:get_cell(Document, 1, "D9"),
    ?assertEqual("3", Text).

get_packed_cols_cell_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, Document} = erlodf:open(Path),
    {ok, Text} = erlodf_spreadsheet:get_cell(Document, 1, "H9"),
    ?assertEqual("H9", Text).

change_cell_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, Document} = erlodf:open(Path),
    Cell0 = erlodf_spreadsheet:cell(Document, 1, {1,2}),
    {ok, Text0} = erlodf_xml:value(Cell0),
    ?assertEqual('table:table-cell', Cell0#xmlElement.name),
    ?assertEqual("Market", Text0),
    Document = erlodf_spreadsheet:set_cell(Document, 1, {1, 2}, "Check Me", text),
    Binary = erlodf:save(Document),
    %ok=erlodf:close(Document),

    {ok, Document1} = erlodf:open(Binary),
    {ok, Text1} = erlodf_spreadsheet:get_cell(Document1, 1, {1, 2}),
    ?assertEqual("Check Me", Text1).

set_cell_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, Document} = erlodf:open(Path),
    Cell0 = erlodf_spreadsheet:cell(Document, 1, "F8"),
    empty = erlodf_xml:value(Cell0),
    ?assertEqual('table:table-cell', Cell0#xmlElement.name),
    %?assertEqual("", Text0),
    Document = erlodf_spreadsheet:set_cell(Document, 1, "F8", "Check Me", text),
    {ok, Text1} = erlodf_spreadsheet:get_cell(Document, 1, "F8"),
    ?assertEqual("Check Me", Text1).
