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
         get_with_type/2,
         attribute/2,
         attribute/3,
         update_attribute/3,
         update_value/2,
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

get_with_type(Value, "float") ->
    case re:run(Value, ",|\\.") of
        {match, _} ->
            list_to_float(Value);
        nomatch ->
            list_to_integer(Value)
    end;
get_with_type(Value, "boolean") ->
    list_to_atom(Value);
get_with_type(Value, _) ->
    Value.

update_value(#xmlText{}=Node, Value) when is_binary(Value); is_list(Value) ->
    Node#xmlText{value=Value};
update_value(#xmlText{}=Node, Value) ->
    Value1 = string:replace(lists:flatten(io_lib:format("~p", [Value])), ".", ","),
    Node#xmlText{value=Value1};
update_value(#xmlElement{content=Childs}=Node, Value) ->
    Node#xmlElement{content=update_value(Childs, Value)};
update_value([Child], Value) ->
    [update_value(Child, Value)];
update_value([], Value) ->
    Node = #xmlElement{name='text:p',
                       content=[#xmlText{}]},
    update_value([Node], Value);
update_value(_Nodes, _Value) ->
    {error, "Bad input"}.

update_tree(XML, #xmlElement{}=Node) ->
    update_tree(XML, [Node]);
update_tree(XML, [#xmlElement{parents=Parent}|_]=Nodes) ->
    update_tree(XML, Nodes, lists:reverse(Parent)).

update_tree(#xmlElement{name=Tag}, [#xmlElement{name=Tag}|_]=Nodes, []) ->
    Nodes;
update_tree(#xmlElement{content=Content}=XML, Nodes, []) ->
    %io:format("Content: ~p, Nodes: ~p~n", [Content, Nodes]),
    Content1 = lists:ukeymerge(#xmlElement.pos, Nodes, Content),
    XML#xmlElement{content=Content1};
update_tree(#xmlElement{name=Tag}=XML, Nodes, [{Tag, _N}|Rest]) ->
    update_tree(XML, Nodes, Rest);
update_tree(#xmlElement{content=Content}=XML, Nodes, [{_SubTag, N}|Rest]) ->
    %io:format("Parent: ~p~n", [Rest]),
    Content1 = lists:flatten(replace_nth(Content, N, Nodes, Rest)),
    XML#xmlElement{content=Content1}.

replace_nth([], _, Nodes, []) ->
    Nodes;
replace_nth([_], 1, Nodes, []) ->
    Nodes;
replace_nth([Content], 1, Nodes, Rest) ->
    [update_tree(Content, Nodes, Rest)];
replace_nth([H|Content], 1, Nodes, Rest) ->
    [update_tree(H, Nodes, Rest) | Content];
replace_nth([H|Content], N, Nodes, Rest) ->
    lists:flatten([H | replace_nth(Content, N - 1, Nodes, Rest)]).

attribute(AttrName, Node) ->
    attribute(AttrName, Node, undefined).

attribute(AttrName, #xmlElement{attributes=Attrs}, Default) ->
    case lists:keyfind(AttrName, #xmlAttribute.name, Attrs) of
        false -> Default;
        #xmlAttribute{value=Value} -> Value
    end.

update_attribute(Attr, #xmlElement{attributes=Attrs}=Node, Value) -> 
    case lists:keyfind(Attr, #xmlAttribute.name, Attrs) of
        false ->
            A = #xmlAttribute{name=Attr, value=Value},
            Node#xmlElement{attributes=[A | Attrs]};
        #xmlAttribute{} = A0 -> 
            A = A0#xmlAttribute{value=Value},
            Attrs1 = lists:keyreplace(Attr, #xmlAttribute.name, Attrs, A),
            Node#xmlElement{attributes=Attrs1}
    end.

