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
         get_cell_with_type_test/1,
         get_merged_cols_cell_test/1,
         get_packed_cols_cell_test/1,
         change_cell_test/1,
         set_cell_test/1,
         set_cell_with_type_test/1,
         set_pdcked_cell_test/1,
         copy_empty_row_test/1,
         copy_row_test/1
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
     get_cell_with_type_test,
     get_merged_cols_cell_test,
     get_packed_cols_cell_test,
     change_cell_test,
     set_pdcked_cell_test,
     set_cell_with_type_test,
     set_cell_test,
     copy_empty_row_test,
     copy_row_test
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
    Sheets = erlodf_spreadsheet:sheets(Document),
    ?assertEqual(3, length(Sheets)).

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

get_cell_with_type_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, Document} = erlodf:open(Path),
    {ok, Text} = erlodf_spreadsheet:get_cell(Document, 1, "F2"),
    ?assertEqual(0.00095774, Text).

get_merged_cols_cell_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, Document} = erlodf:open(Path),
    {ok, Text} = erlodf_spreadsheet:get_cell(Document, 1, "D9"),
    ?assertEqual(3, Text).

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
    Binary = erlodf:save(Document),
    %ok=erlodf:close(Document),

    {ok, Document1} = erlodf:open(Binary),

    {ok, Text1} = erlodf_spreadsheet:get_cell(Document1, 1, "F8"),
    ?assertEqual("Check Me", Text1).

set_cell_with_type_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, Document} = erlodf:open(Path),
    Cell0 = erlodf_spreadsheet:cell(Document, 1, "F8"),
    empty = erlodf_xml:value(Cell0),
    ?assertEqual('table:table-cell', Cell0#xmlElement.name),
    Document = erlodf_spreadsheet:set_cell(Document, 1, "F8", 1.8, float),
    Binary = erlodf:save(Document),

    {ok, Document1} = erlodf:open(Binary),

    {ok, Text1} = erlodf_spreadsheet:get_cell(Document1, 1, "F8"),
    ?assertEqual(1.8, Text1).

set_pdcked_cell_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, Document} = erlodf:open(Path),
    Cell0 = erlodf_spreadsheet:cell(Document, 1, "G9"),
    empty = erlodf_xml:value(Cell0),
    ?assertEqual('table:table-cell', Cell0#xmlElement.name),
    %?assertEqual("", Text0),
    Document = erlodf_spreadsheet:set_cell(Document, 1, "G9", "Check Me", text),
    Binary = erlodf:save(Document),
    %ok=erlodf:close(Document),

    {ok, Document1} = erlodf:open(Binary),

    {ok, Text1} = erlodf_spreadsheet:get_cell(Document1, 1, "G9"),
    ?assertEqual("Check Me", Text1).

copy_row_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, Document} = erlodf:open(Path),
    Document = erlodf_spreadsheet:copy_row(Document, 2, 3, 3),
    Binary = erlodf:save(Document),
    {ok, Document1} = erlodf:open(Binary),
    
    % Test if rows before and after are correct (not overwrittten)
    {ok, A7} = erlodf_spreadsheet:get_cell(Document1, 2, "A8"),
    ?assertEqual("4A", A7),

    {ok, A2} = erlodf_spreadsheet:get_cell(Document1, 2, "A2"),
    ?assertEqual("2A", A2),

    % Check if all data copied correctly
    {ok, A3} = erlodf_spreadsheet:get_cell(Document1, 2, "A3"),
    ?assertEqual("3A", A3),

    {ok, B4} = erlodf_spreadsheet:get_cell(Document1, 2, "B4"),
    ?assertEqual("3B", B4),

    {ok, C5} = erlodf_spreadsheet:get_cell(Document1, 2, "C5"),
    ?assertEqual("3C",C5),
    
    % Change copied cells
    erlodf_spreadsheet:set_cell(Document1, 2, "B4", "B4"),
    erlodf_spreadsheet:set_cell(Document1, 2, "B5", "B5"),

    {ok, N_B4} = erlodf_spreadsheet:get_cell(Document1, 2, "B4"),
    ?assertEqual("B4", N_B4),

    {ok, N_B5} = erlodf_spreadsheet:get_cell(Document1, 2, "B5"),
    ?assertEqual("B5", N_B5).

copy_empty_row_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, Document} = erlodf:open(Path),
    Document = erlodf_spreadsheet:copy_row(Document, 3, 2, 3),
    Binary = erlodf:save(Document),
    {ok, Document1} = erlodf:open(Binary),
    
    % Test if rows before and after are correct (not overwrittten)
    {ok, A6} = erlodf_spreadsheet:get_cell(Document1, 3, "A6"),
    ?assertEqual("A3", A6),

    {ok, A1} = erlodf_spreadsheet:get_cell(Document1, 3, "A1"),
    ?assertEqual("A1", A1),

    % Check if all data copied correctly
    A2 = erlodf_spreadsheet:get_cell(Document1, 3, "A2"),
    ?assertEqual(empty, A2),

    B3 = erlodf_spreadsheet:get_cell(Document1, 3, "B3"),
    ?assertEqual(empty, B3),

    B4 = erlodf_spreadsheet:get_cell(Document1, 3, "B4"),
    ?assertEqual(empty, B4),

    C5 = erlodf_spreadsheet:get_cell(Document1, 3, "C5"),
    ?assertEqual(empty,C5),
    
    % Change copied cells
    erlodf_spreadsheet:set_cell(Document1, 3, "B4", "B4N"),
    erlodf_spreadsheet:set_cell(Document1, 3, "B5", "B5N"),

    {ok, N_B4} = erlodf_spreadsheet:get_cell(Document1, 3, "B4"),
    ?assertEqual("B4N", N_B4),

    {ok, N_B5} = erlodf_spreadsheet:get_cell(Document1, 3, "B5"),
    ?assertEqual("B5N", N_B5).
