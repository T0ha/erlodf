-module(xml_SUITE).
-export([
         all/0,
         init_per_testcase/2,
         end_per_testcase/2,
         value_test/1,
         update_value_test/1,
         update_tree_test/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("xmerl/include/xmerl.hrl").

all() ->
    [
     value_test,
     update_value_test,
     update_tree_test
    ].

init_per_testcase(_, C) ->
    application:ensure_all_started(erlodf),
    C.

end_per_testcase(_, C) ->
    application:stop(erlodf),
    C.

value_test(C) ->
    Path = proplists:get_all_values(data_dir, C) ++ "/meta.xml",
    {XML, _} = xmerl_scan:file(Path),
    [Node|_] = xmerl_xpath:string("//meta:editing-duration", XML),
    {ok, Text} = erlodf_xml:value(Node),
    ?assertEqual("PT23M9S", Text).

update_value_test(C) -> 
    Path = proplists:get_all_values(data_dir, C) ++ "/meta.xml",
    {XML, _} = xmerl_scan:file(Path),
    [Node|_] = xmerl_xpath:string("//meta:editing-duration", XML),
    NodeNew = erlodf_xml:update_value(Node, "Test Value"),
    {ok, Text} = erlodf_xml:value(NodeNew),
    ?assertEqual("Test Value", Text).



update_tree_test(C) -> 
    Path = proplists:get_all_values(data_dir, C) ++ "/meta.xml",
    {XML, _} = xmerl_scan:file(Path),
    [Node0|_] = xmerl_xpath:string("//meta:editing-duration", XML),
    {ok, Text0} = erlodf_xml:value(Node0),
    ?assertEqual("PT23M9S", Text0),
    Node1 = erlodf_xml:update_value(Node0, "Test Value"),
    Tree = erlodf_xml:update_tree(XML, Node1),
    ?assertEqual(XML#xmlElement.name, Tree#xmlElement.name),
    ?assertEqual(length(XML#xmlElement.content), length(Tree#xmlElement.content)),
    ?assertNotEqual(XML#xmlElement.content, Tree#xmlElement.content),

    [Meta0|_] = xmerl_xpath:string("//office:meta", XML),
    [Meta1|_] = xmerl_xpath:string("//office:meta", Tree),

    ?assertEqual(Meta0#xmlElement.name, Meta1#xmlElement.name),
    ?assertEqual(length(Meta0#xmlElement.content), length(Meta1#xmlElement.content)),
    ?assertNotEqual(Meta0#xmlElement.content, Meta1#xmlElement.content),

    [Node2, Node3] = xmerl_xpath:string("//meta:editing-duration", Tree),
    {ok, Text1} = erlodf_xml:value(Node2),
    {ok, Text2} = erlodf_xml:value(Node3),
    [_] = xmerl_xpath:string("//test", Tree),
    ?assertNotEqual("PT23M9S", Text1),
    ?assertEqual("PT23M9S2", Text2),
    ?assertEqual("Test Value", Text1).

