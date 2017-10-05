-module(file_SUITE).
-export([
         all/0,
         init_per_testcase/2,
         end_per_testcase/2,
         open_file_test/1,
         open_data_test/1,
         save_test/1
        ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     open_file_test,
     open_data_test,
     save_test
    ].

init_per_testcase(_, C) ->
    application:ensure_all_started(erlodf),
    C.

end_per_testcase(_, C) ->
    application:stop(erlodf),
    C.

open_file_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, PID} = erlodf:open(Path),
    [{Path, PID}] = ets:lookup(erlodf_documents, Path).

open_data_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, Data} = file:read_file(Path),
    {ok, PID} = erlodf:open(Data),
    [{_, PID}] = ets:match_object(erlodf_documents, {'_', PID}).

save_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, _Data} = file:read_file(Path),
    {ok, PID} = erlodf:open(Path),
    erlodf:save(PID).


