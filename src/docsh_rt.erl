-module(docsh_rt).
-compile([debug_info,
          {inline, [h/1, h/3,
                    guard_no_docs/2,
                    guard_not_supported/2]}]).

-export([h0/1,
         h2/1]).

-import(docsh_lib, [print/2]).

-define(il2b(IOList), iolist_to_binary(IOList)).

h0(Mod) -> guard_no_docs(Mod, fun h/1).

h2(Mod) -> guard_no_docs(Mod, fun h/3).

h(Docs) ->
    {_, ModDoc} = proplists:get_value(moduledoc, Docs),
    ModDoc.

h(Docs, Fun, Arity) ->
    {docs, FunDocs} = lists:keyfind(docs, 1, Docs),
    FA = {Fun, Arity},
    %% TODO: fragile
    {FA,_,_,_,Doc} = lists:keyfind(FA, 1, FunDocs),
    Doc.

guard_no_docs(Mod, Fun) ->
    T = try
            guard_not_supported(Fun, Mod:'__docs'())
        catch
            error:undef ->
                <<"Module documentation not found">>;
            E:R ->
                ?il2b([<<"Internal error: ">>,
                       io_lib:format("~p:~p", [E, R])])
        end,
    io:format("~s~n", [T]).

guard_not_supported(Fun, {elixir_docs_v1, Docs}) ->
    Fun(Docs);
guard_not_supported(_, _) -> 
    <<"Documentation format not supported">>.
