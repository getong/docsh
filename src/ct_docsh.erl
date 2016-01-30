-module(ct_docsh).

-export([core_transform/2]).

-import(docsh_lib, [print/2]).

-spec core_transform(cerl:c_module(), _) -> cerl:c_module().
core_transform(Mod, _Opts) ->
    %print("core ast: ~p~n", [Mod]),
    print("core: ~s~n", [core_pp:format(Mod)]),
    Mod.
