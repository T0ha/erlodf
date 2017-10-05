-module(erlodf).

-export([
         %new/0,
         open/1, 
         save/1
        ]).

-include("odf.hrl").

-spec open(file:name() | binary()) -> {ok, pid()}.
open(Fname) ->
    {ok, PID} = erlodf_sup:open_document(Fname),

    {ok, erlodf_document_sup:get_document_pid(PID)}.

-spec save(pid() | file:filename_all())-> binary().
save(PID) when is_pid(PID) ->
    erlodf_document:save(PID);
save(Fname) ->
    save(ets:lookup_element(erlodf_documents, Fname, 2)).


