-module(docsh_embeddable).
-compile([debug_info,
          {inline, [get_elixir_docs_v1/1,
                    do_with_docs/2,
                    do_with_supported/2,
                    types/1]}]).

-export([h/1,
         h/3]).

-type fname() :: atom().

-import(docsh_lib, [print/2]).

-define(a2b(A), atom_to_binary(A, utf8)).
-define(i2b(I), integer_to_binary(I)).
-define(il2b(IOList), iolist_to_binary(IOList)).

-spec h(module()) -> ok.
h(Mod) ->
    F = fun (Docs) ->
                {_, ModDoc} = proplists:get_value(moduledoc, Docs),
                %% TODO: work on the printout format in cases
                %%       of unavailable docs
                io_lib:format("## Description~n~n~s~n"
                              "## Types~n~s~n",
                              [ModDoc, types(Docs)])
        end,
    do_with_docs(Mod, F).

-spec h(module(), fname(), arity()) -> ok.
h(Mod, Fun, Arity) ->
    F = fun (Docs) ->
                case {lists:keyfind(docs, 1, Docs),
                      lists:keyfind(specs, 1, Docs)} of
                    {false, false} -> error({no_docs, <<"neither docs nor specs found">>});
                    {false, {_, Specs}} -> format({Fun, Arity}, no_docs, Specs);
                    {{_, FDocs}, false} -> format({Fun, Arity}, FDocs, no_specs);
                    {{_, FDocs}, {_, Specs}} -> format({Fun, Arity}, FDocs, Specs)
                end
        end,
    do_with_docs(Mod, F).

do_with_docs(Mod, Fun) ->
    T = try
            do_with_supported(Fun, get_elixir_docs_v1(Mod))
        catch
            error:{no_docs, R} ->
                <<"Module documentation not found:", R/bytes>>;
            _:R ->
                ?il2b([<<"docsh error: ">>,
                       io_lib:format("~p\n~p\n", [R, erlang:get_stacktrace()])])
        end,
    io:format("~s~n", [T]).

do_with_supported(Fun, {elixir_docs_v1, Docs}) ->
    Fun(Docs);
do_with_supported(_, _) ->
    <<"Documentation format not supported">>.

format(FunArity, no_docs, Specs) ->
    {FunArity, Spec} = lists:keyfind(FunArity, 1, Specs),
    io_lib:format("~s", [Spec]);
format(FunArity, Docs, no_specs) ->
    {FunArity, _, _, _, Doc} = lists:keyfind(FunArity, 1, Docs),
    io_lib:format("~s", [Doc]);
format(FunArity, Docs, Specs) ->
    {FunArity, _, _, _, Doc} = lists:keyfind(FunArity, 1, Docs),
    {FunArity, Spec} = lists:keyfind(FunArity, 1, Specs),
    io_lib:format("~s~n~s", [Spec, Doc]).

types(Docs) ->
    Types = proplists:get_value(types, Docs, []),
    [ ["\n", Desc] || {{_Name, _Arity}, Desc} <- Types ].

get_elixir_docs_v1(Mod) ->
    BEAMFile = code:which(Mod),
    case beam_lib:chunks(BEAMFile, ["ExDc"]) of
        {ok, {Mod, [{"ExDc", BExDc}]}} ->
            erlang:binary_to_term(BExDc);
        {error, _, {missing_chunk, _, _}} ->
            error({no_docs, <<"no ExDc chunk">>})
    end.
