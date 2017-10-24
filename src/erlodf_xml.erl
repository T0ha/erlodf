%%%-------------------------------------------------------------------
%%% @author Anton Shvein
%%% @copyright (C) 2017, ins
%%% @doc
%%%
%%% @end
%%% Created : 2017-09-29 11:07:43.355120
%%%-------------------------------------------------------------------
-module(erlodf_xml).

-include("odf.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-export([
         value/1,
         attribute/2,
         update_value/2,
         update_value/3,
         update_tree/2,
         update_tree/3
        ]).

value(#xmlText{value=V}) ->
    {ok, V};
value(#xmlElement{content=Childs}) ->
    value(Childs);
value([Child]) ->
    value(Child);
value([]) ->
    empty;
value(_) ->
    {error, "Bad input"}.

update_value(Node, Value) ->
    update_value(Node, Value, text).

update_value(#xmlText{}=Node, Value, Type) ->
    Node#xmlText{value=Value, type=Type};
update_value(#xmlElement{content=Childs}=Node, Value, Type) ->
    Node#xmlElement{content=update_value(Childs, Value, Type)};
update_value([Child], Value, Type) ->
    [update_value(Child, Value, Type)];
update_value([], Value, Type) ->
    update_value(#xmlText{}, Value, Type);
update_value(_Nodes, _Value, _Type) ->
    {error, "Bad input"}.

update_tree(XML, #xmlElement{parents=Parent}=Node) ->
    update_tree(XML, Node, lists:reverse(Parent)).

update_tree(#xmlElement{name=Tag}, #xmlElement{name=Tag}=Node, []) ->
    Node;
update_tree(#xmlElement{content=Content}=XML, #xmlElement{pos=N}=Node, []) ->
    XML#xmlElement{content=replace_nth(Content, N, Node, [])};
update_tree(#xmlElement{name=Tag}=XML, Node, [{Tag, _N}|Rest]) ->
    update_tree(XML, Node, Rest);
update_tree(#xmlElement{content=Content}=XML, Node, [{_SubTag, N}|Rest]) ->
    Content1 = replace_nth(Content, N, Node, Rest),
    XML#xmlElement{content=Content1}.

replace_nth([Content], 1, Node, Rest) ->
    [update_tree(Content, Node, Rest)];
replace_nth([H|Content], 1, Node, Rest) ->
    [update_tree(H, Node, Rest) | Content];
replace_nth([H|Content], N, Node, Rest) ->
    [H | replace_nth(Content, N - 1, Node, Rest)].

attribute(XML, AttrName) ->
    ok.