-module(document_SUITE).
-export([
         all/0,
         init_per_testcase/2,
         end_per_testcase/2,
         style_test/1,
         body_test/1
        ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     body_test
     %style_test
    ].

init_per_testcase(_, C) ->
    application:ensure_all_started(erlodf),
    C.

end_per_testcase(_, C) ->
    application:stop(erlodf),
    C.

body_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, PID} = erlodf:open(Path),
    {ok, Body} = erlodf_document:body(PID).

style_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/test.ods",
    {ok, PID} = erlodf:open(Path),
    {ok, Style} = erlodf_document:style(PID).



