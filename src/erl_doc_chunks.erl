-module(erl_doc_chunks).
-export([file/1]).

file(ErlPath) ->
  ContentType = <<"text/markdown">>,
  Module = list_to_atom(filename:basename(ErlPath, ".erl")),
  BeamPath = code:which(Module),
  {ok, {Module, [{"Dbgi", Dbgi}]}} = beam_lib:chunks(BeamPath, ["Dbgi"]),
  {debug_info_v1, erl_abstract_code, {Abst, _}} = binary_to_term(Dbgi),
  Docs = docs(ErlPath),

  Entries = [Entry || {function, {Line, _Column}, Name, Arity, _} <- Abst,
                      (Entry = entry(Name, Arity, maps:find(Line, Docs))) =/= undefined],

  {attribute, {Line, _Column}, module, Module} = lists:keyfind(module, 3, Abst),

  {DocAnno, Doc} =
    case maps:find(Line, Docs) of
      {ok, {X, Y}} -> {X, #{<<"en">> => Y}};
      error -> {1, hidden}
    end,

  Metadata = #{},
  {docs_v1, DocAnno, erlang, ContentType, Doc, Metadata, Entries}.

docs(ErlPath) ->
  List = [doc(Line, Lines) || {Line, _, 0, ["% @doc" | Lines]} <- erl_comment_scan:file(ErlPath)],
  maps:from_list(List).

doc(Line, Lines) ->
  Lines1 = lists:map(fun
                       ("%") -> $\n;
                       ("% " ++ Rest) -> [Rest, $\n]
                     end, Lines),
  {Line + length(Lines) + 1, {Line, iolist_to_binary(Lines1)}}.

entry(Name, Arity, {ok, {DocAnno, Doc}}) ->
  Signature = [iolist_to_binary(io_lib:format("~s/~p", [Name, Arity]))],
  Metadata = #{},
  {{function, Name, Arity}, DocAnno, Signature, #{<<"en">> => Doc}, Metadata};
entry(_, _, error) ->
  undefined.
