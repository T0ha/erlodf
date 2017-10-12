-module(spreadsheet_SUITE).
-export([
         all/0,
         init_per_testcase/2,
         end_per_testcase/2,
         sheets_test/1,
         sheet_by_number_test/1,
         sheet_by_name_test/1,
         row_test/1,
         cell_tuple_test/1,
         cell_letter_test/1
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
     cell_letter_test
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

row_test(C) ->
    ok.

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
    Cell = erlodf_spreadsheet:cell(Sheet, "B2"),
    {ok, Text} = erlodf_xml:value(Cell),
    ?assertEqual('table:table-cell', Cell#xmlElement.name),
    ?assertEqual("BTC/USDT", Text).
