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
         copy_row_test/1,
         flash_formula_test/1
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
     copy_row_test,
     flash_formula_test
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
    ?assertEqual(4, length(Sheets)).

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
    ?assertEqual("BTC/USDT", Text),

    {ok, Text1} = erlodf_spreadsheet:get_cell(Document, 3, "B3"),
    ?assertEqual("3B", Text1).

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
    Document = erlodf_spreadsheet:set_cell(Document, 1, "F8", "Check Me", text),
    Document = erlodf_spreadsheet:set_cell(Document, 3, "B3", "After Empty", text),
    Binary = erlodf:save(Document),
    %file:write_file("out.ods", Binary),
    %ok=erlodf:close(Document),

    {ok, Document1} = erlodf:open(Binary),

    {ok, Text1} = erlodf_spreadsheet:get_cell(Document1, 1, "F8"),
    ?assertEqual("Check Me", Text1),

    {ok, Text2} = erlodf_spreadsheet:get_cell(Document1, 3, "B3"),
    ?assertEqual("After Empty", Text2).

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
    file:write_file("out.ods", Binary),
    {ok, Document1} = erlodf:open(Binary),
    
    % Test if rows before and after are correct (not overwrittten)
    {ok, A7} = erlodf_spreadsheet:get_cell(Document1, 2, "A7"),
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
    ?assertEqual("3A", A6),

    {ok, A1} = erlodf_spreadsheet:get_cell(Document1, 3, "A1"),
    ?assertEqual("1A", A1),

    % Check if all data copied correctly
    {ok, A2} = erlodf_spreadsheet:get_cell(Document1, 3, "A2"),
    ?assertEqual("", A2),

    {ok, B3} = erlodf_spreadsheet:get_cell(Document1, 3, "B3"),
    ?assertEqual("", B3),

    {ok, B4} = erlodf_spreadsheet:get_cell(Document1, 3, "B4"),
    ?assertEqual("", B4),

    {ok, C5} = erlodf_spreadsheet:get_cell(Document1, 3, "C5"),
    ?assertEqual("",C5),
    
    % Change copied cells
    erlodf_spreadsheet:set_cell(Document1, 3, "B4", "B4N"),
    erlodf_spreadsheet:set_cell(Document1, 3, "B5", "B5N"),
    erlodf_spreadsheet:set_cell(Document1, 3, "B6", "B6N"),

    {ok, N_B4} = erlodf_spreadsheet:get_cell(Document1, 3, "B4"),
    ?assertEqual("B4N", N_B4),

    {ok, N_B5} = erlodf_spreadsheet:get_cell(Document1, 3, "B5"),
    ?assertEqual("B5N", N_B5),

    {ok, N_B6} = erlodf_spreadsheet:get_cell(Document1, 3, "B6"),
    ?assertEqual("B6N", N_B6).


flash_formula_test(C) -> 
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, Document} = erlodf:open(Path),

    erlodf_spreadsheet:set_cell(Document, "Formulas", "B6", 100),
    erlodf_spreadsheet:set_cell(Document, "Formulas", "B7", 223),

    Binary = erlodf:save(Document),
    {ok, Document1} = erlodf:open(Binary),

    {ok, ""} = erlodf_spreadsheet:get_cell(Document1, "Formulas", "C6"),
    {ok, ""} = erlodf_spreadsheet:get_cell(Document1, "Formulas", "C7"),

    % Bug found in production
    {ok, ""} = erlodf_spreadsheet:get_cell(Document1, "Formulas", "D2"),
    {ok, ""} = erlodf_spreadsheet:get_cell(Document1, "Formulas", "E2"),
    {ok, ""} = erlodf_spreadsheet:get_cell(Document1, "Formulas", "F2"),
    {ok, ""} = erlodf_spreadsheet:get_cell(Document1, "Formulas", "G2"),
    {ok, ""} = erlodf_spreadsheet:get_cell(Document1, "Formulas", "H2"),
    {ok, ""} = erlodf_spreadsheet:get_cell(Document1, "Formulas", "I2").



