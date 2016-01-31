-module(docsh_edoc_xmerl).

-export(['#root#'/4,
         '#element#'/5,
         '#text#'/1,
         '#xml-inheritance#'/0]).

-export([dd/4,
         dl/4,
         fullDescription/4,
         li/4,
         module/4]).

-record(function, {name, arity, exported, label, description}).

-include_lib("xmerl/include/xmerl.hrl").

-define(il2b(IOList), iolist_to_binary(IOList)).
-define(l2b(L), list_to_binary(L)).
-define(l2i(L), list_to_integer(L)).
-define(l2ea(L), list_to_existing_atom(L)).
-define(NL, "\n").

%% The '#text#' function is called for every text segment.
'#text#'(Text) -> ?il2b(Text).

%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.
'#root#'(Data, _Attrs, [], _E) ->
    lists:flatten(Data).

%% The '#element#' function is the default handler for XML elements.
'#element#'(function, Data, Attrs, _Parents, _E) ->
    F1 = function_details_from_attrs(Attrs, #function{}),
    F2 = function_details_from_data(lists:flatten(Data), F1),
    [{function, debug(function, {{F2#function.name, F2#function.arity},
                                 {exported,    F2#function.exported},
                                 {label,       F2#function.label},
                                 {description, F2#function.description}})}];
'#element#'(see, Data, _Attrs, _Parents, _E) ->
    {fmt, debug(see, ["See ", Data, "\n"])};
'#element#'(equiv, Data, _Attrs, _Parents, _E) ->
    Desc = case collect_loose_text(Data) of
               [{fmt, Eq}, {fmt, See}] ->
                   {description, ?il2b(["Equivalent to ", Eq, See])};
               [{fmt, Eq}] ->
                   {description, ?il2b(["Equivalent to ", Eq, "\n"])}
           end,
    debug(equiv, Desc);
'#element#'(functions, Data, _Attrs, _Parents, _E) ->
    %% Functions are already extracted.
    [{functions, Data}];
'#element#'(briefDescription, _Data, _Attrs, _Parents, _E) ->
    [];
'#element#'(description, Data, _Attrs, _Parents, _E) ->
    [{description, debug(description, ?il2b(Data))}];
'#element#'(Tag, Data, _Attrs, _Parents, _E)
        when Tag =:= h1;
             Tag =:= h2;
             Tag =:= h3 ->
    {fmt, debug(inline, header(Tag, unwrap_inline(Data)))};
'#element#'(Tag, Data, _Attrs, _Parents, _E)
        when Tag =:= a;
             Tag =:= code;
             Tag =:= em;
             Tag =:= expr;
             Tag =:= h4;
             Tag =:= h5;
             Tag =:= h6;
             Tag =:= tt ->
    debug('inline:before', Data),
    After = [ unwrap_inline(E) || E <- Data ],
    debug('inline:after', After),
    debug(Tag, After),
    {inline, After};
'#element#'(Tag, Data, _Attrs, _Parents, _E) when
        Tag =:= dt ->
    {dt, debug(Tag, Data)};
'#element#'(Tag, Data, _Attrs, _Parents, _E) when
        Tag =:= p ->
    {fmt, debug(Tag, cleanup_lines(Data))};
'#element#'(Tag, Data, _Attrs, _Parents, _E) when
        Tag =:= pre ->
    {fmt, debug(Tag, [Data, "\n"])};
'#element#'(Tag, Data, _Attrs, _Parents, _E) when
        Tag =:= ol;
        Tag =:= ul ->
    list(Tag, Data);
'#element#'(Tag, Data, _Attrs, _Parents, _E) ->
    debug(discarded, {Tag, Data}),
    [].

module(Data, Attrs, _Parents, #xmlElement{name = module, content = Es}) ->
    Types = [{type_name(E), E} || E <- get_content(typedecls, Es)],
    debug(types, types(lists:sort(Types), [])),
    Details = module_details_from_data(Data),
    {value, {functions, Functions}, Rest} = lists:keytake(functions, 1, Details),
    debug(module, [{module, module_details_from_attrs(Attrs) ++ Rest}
     | Functions]).

types([], _Opts) -> [];
types(Ts, Opts) ->
    lists:flatmap(fun ({Name, E}) ->
                          try
                              [?il2b(typedecl(Name, E, Opts))]
                          catch
                              _:_ -> typedecl(Name, E, Opts)
                          end
                  end, Ts).

typedecl(_Name, #xmlElement{content = Es}, Opts) ->
    typedef(get_content(typedef, Es), Opts) ++ fulldesc(Es).

fulldesc(Es) ->
    case get_content(fullDescription, get_content(description, Es)) of
        [] -> [?NL];
        Desc -> [{p, Desc}, ?NL]
    end.

get_content(Name, Es) ->
    case get_elem(Name, Es) of
        [#xmlElement{content = Es1}] ->
            Es1;
        [] -> []
    end.

get_elem(Name, [#xmlElement{name = Name} = E | Es]) ->
    [E | get_elem(Name, Es)];
get_elem(Name, [_ | Es]) ->
    get_elem(Name, Es);
get_elem(_, []) ->
    [].

type_name(#xmlElement{content = Es}) ->
    t_name(get_elem(erlangName, get_content(typedef, Es))).

t_name([E]) ->
    N = get_attrval(name, E),
    case get_attrval(module, E) of
        "" -> atom(N);
        M ->
            S = atom(M) ++ ":" ++ atom(N),
            case get_attrval(app, E) of
                "" -> S;
                A -> "//" ++ atom(A) ++ "/" ++ S
            end
    end.

atom(String) ->
    io_lib:write_atom(list_to_atom(String)).

typedef(Es, Opts) ->
    Name = ([t_name(get_elem(erlangName, Es)), "("]
            ++ seq(fun t_utype_elem/1, get_content(argtypes, Es), [")"])),
    (case get_elem(type, Es) of
         [] -> [{b, ["abstract datatype"]}, ": ", {tt, Name}];
         Type -> format_type(Name, Name, Type, [], Opts)
     end
     ++ local_defs(get_elem(localdef, Es), Opts)).

t_utype([E]) ->
    t_utype_elem(E).

t_utype_elem(E=#xmlElement{content = Es}) ->
    case get_attrval(name, E) of
        "" -> t_type(Es);
        Name ->
            T = t_type(Es),
            case T of
                [Name] -> T;    % avoid generating "Foo::Foo"
                T -> [Name] ++ ["::"] ++ T
            end
    end.

see(E, Es) ->
    case href(E) of
        [] -> Es;
        Ref ->
            [{a, Ref, Es}]
    end.

href(E) ->
    case get_attrval(href, E) of
        "" -> [];
        URI ->
            T = case get_attrval(target, E) of
                    "" -> [];
                    S -> [{target, S}]
                end,
            [{href, URI} | T]
    end.

etypef(L, O0) ->
    {R, O} = etypef(L, [], O0, []),
    {lists:reverse(R), O}.

etypef([C | L], St, [C | O], R) ->
    etypef(L, St, O, [[C] | R]);
etypef(" "++L, St, O, R) ->
    etypef(L, St, O, R);
etypef("", [Cs | St], O, R) ->
    etypef(Cs, St, O, R);
etypef("", [], O, R) ->
    {R, O};
etypef(L, St, " "++O, R) ->
    etypef(L, St, O, [" " | R]);
etypef(L, St, "\n"++O, R) ->
    Ss = lists:takewhile(fun(C) -> C =:= $\s end, O),
    etypef(L, St, lists:nthtail(length(Ss), O), ["\n"++Ss | R]);
etypef([{a, HRef, S0} | L], St, O0, R) ->
    {S, O} = etypef(S0, app_fix(O0)),
    etypef(L, St, O, [{a, HRef, S} | R]);
etypef("="++L, St, "::"++O, R) ->
    %% EDoc uses "=" for record field types; Erlang types use "::".
    %% Maybe there should be an option for this, possibly affecting
    %% other similar discrepancies.
    etypef(L, St, O, ["=" | R]);
etypef([Cs | L], St, O, R) ->
    etypef(Cs, [L | St], O, R).

app_fix(L) ->
    try
        {"//" ++ R1,L2} = app_fix(L, 1),
        [App, Mod] = string:tokens(R1, "/"),
        "//" ++ atom(App) ++ "/" ++ atom(Mod) ++ L2
    catch _:_ -> L
    end.

app_fix(L, I) -> % a bit slow
    {L1, L2} = lists:split(I, L),
    case erl_scan:tokens([], L1 ++ ". ", 1) of
        {done, {ok,[{atom,_,Atom}|_],_}, _} -> {atom_to_list(Atom), L2};
        _ -> app_fix(L, I+1)
    end.

format_type(Prefix, Name, Type, Last, Opts) ->
    try
        L = t_utype(Type),
        O = pp_type(Name, Type),
        {R, ".\n"} = etypef(L, O),
        [Prefix ++ [" :: "] ++ R ++ Last]
    catch _:_ ->
              %% Example: "t() = record(a)."
              format_type(Prefix, Name, Type, Last, Opts)
    end.

pp_type(Prefix, Type) ->
    Atom = list_to_atom(lists:duplicate(iolist_size(Prefix), $a)),
    Attr = {attribute,0,type,{Atom,ot_utype(Type),[]}},
    L1 = erl_pp:attribute(erl_parse:new_anno(Attr)),
    {L2,N} = case lists:dropwhile(fun(C) -> C =/= $: end, lists:flatten(L1)) of
                 ":: " ++ L3 -> {L3,9}; % compensation for extra "()" and ":"
                 "::\n" ++ L3 -> {"\n"++L3,6}
             end,
    Ss = lists:duplicate(N, $\s),
    re:replace(L2, "\n"++Ss, "\n", [{return,list},global]).

ot_utype([E]) ->
    ot_utype_elem(E).

ot_utype_elem(E=#xmlElement{content = Es}) ->
    case get_attrval(name, E) of
        "" -> ot_type(Es);
        N ->
            Name = {var,0,list_to_atom(N)},
            T = ot_type(Es),
            case T of
                Name -> T;
                T -> {ann_type,0,[Name, T]}
            end
    end.

ot_type([E=#xmlElement{name = typevar}]) ->
    ot_var(E);
ot_type([E=#xmlElement{name = atom}]) ->
    ot_atom(E);
ot_type([E=#xmlElement{name = integer}]) ->
    ot_integer(E);
ot_type([E=#xmlElement{name = range}]) ->
    ot_range(E);
ot_type([E=#xmlElement{name = binary}]) ->
    ot_binary(E);
ot_type([E=#xmlElement{name = float}]) ->
    ot_float(E);
ot_type([#xmlElement{name = nil}]) ->
    ot_nil();
ot_type([#xmlElement{name = paren, content = Es}]) ->
    ot_paren(Es);
ot_type([#xmlElement{name = list, content = Es}]) ->
    ot_list(Es);
ot_type([#xmlElement{name = nonempty_list, content = Es}]) ->
    ot_nonempty_list(Es);
ot_type([#xmlElement{name = tuple, content = Es}]) ->
    ot_tuple(Es);
ot_type([#xmlElement{name = map, content = Es}]) ->
    ot_map(Es);
ot_type([#xmlElement{name = 'fun', content = Es}]) ->
    ot_fun(Es);
ot_type([#xmlElement{name = record, content = Es}]) ->
    ot_record(Es);
ot_type([#xmlElement{name = abstype, content = Es}]) ->
    ot_abstype(Es);
ot_type([#xmlElement{name = union, content = Es}]) ->
    ot_union(Es).

ot_var(E) ->
    {var,0,list_to_atom(get_attrval(name, E))}.

ot_atom(E) ->
    {ok, [{atom,A,Name}], _} = erl_scan:string(get_attrval(value, E), 0),
    {atom,erl_anno:line(A),Name}.

ot_integer(E) ->
    {integer,0,list_to_integer(get_attrval(value, E))}.

ot_range(E) ->
    [I1, I2] = string:tokens(get_attrval(value, E), "."),
    {type,0,range,[{integer,0,list_to_integer(I1)},
                   {integer,0,list_to_integer(I2)}]}.

ot_binary(E) ->
    {Base, Unit} =
    case string:tokens(get_attrval(value, E), ",:*><") of
        [] ->
            {0, 0};
        ["_",B] ->
            {list_to_integer(B), 0};
        ["_","_",U] ->
            {0, list_to_integer(U)};
        ["_",B,_,"_",U] ->
            {list_to_integer(B), list_to_integer(U)}
    end,
    {type,0,binary,[{integer,0,Base},{integer,0,Unit}]}.

ot_float(E) ->
    {float,0,list_to_float(get_attrval(value, E))}.

ot_nil() ->
    {nil,0}.

ot_paren(Es) ->
    {paren_type,0,[ot_utype(get_elem(type, Es))]}.

ot_list(Es) ->
    {type,0,list,[ot_utype(get_elem(type, Es))]}.

ot_nonempty_list(Es) ->
    {type,0,nonempty_list,[ot_utype(get_elem(type, Es))]}.

ot_tuple(Es) ->
    {type,0,tuple,[ot_utype_elem(E) || E <- Es]}.

ot_map(Es) ->
    {type,0,map,[ot_map_field(E) || E <- get_elem(map_field,Es)]}.

ot_map_field(#xmlElement{content=[K,V]}) ->
    {type,0,map_field_assoc,ot_utype_elem(K), ot_utype_elem(V)}.

ot_fun(Es) ->
    Range = ot_utype(get_elem(type, Es)),
    Args = [ot_utype_elem(A) || A <- get_content(argtypes, Es)],
    {type,0,'fun',[{type,0,product,Args},Range]}.

ot_record(Es) ->
    {type,0,record,[ot_type(get_elem(atom, Es)) |
                    [ot_field(F) || F <- get_elem(field, Es)]]}.

ot_field(#xmlElement{content = Es}) ->
    {type,0,field_type,
     [ot_type(get_elem(atom, Es)), ot_utype(get_elem(type, Es))]}.

ot_abstype(Es) ->
    ot_name(get_elem(erlangName, Es),
            [ot_utype_elem(Elem) || Elem <- get_elem(type, Es)]).

ot_union(Es) ->
    {type,0,union,[ot_utype_elem(E) || E <- Es]}.

ot_name(Es, T) ->
    case ot_name(Es) of
        [Mod, ":", Atom] ->
            {remote_type,0,[{atom,0,list_to_atom(Mod)},
                            {atom,0,list_to_atom(Atom)},T]};
        "tuple" when T =:= [] ->
            {type,0,tuple,any};
        Atom ->
            {type,0,list_to_atom(Atom),T}
    end.

ot_name([E]) ->
    Atom = get_attrval(name, E),
    case get_attrval(module, E) of
        "" -> Atom;
        M ->
            case get_attrval(app, E) of
                "" ->
                    [M, ":", Atom];
                A ->
                    ["//"++A++"/" ++ M, ":", Atom] % EDoc only!
            end
    end.

get_attr(Name, [#xmlAttribute{name = Name} = A | As]) ->
    [A | get_attr(Name, As)];
get_attr(Name, [_ | As]) ->
    get_attr(Name, As);
get_attr(_, []) ->
    [].

get_attrval(Name, #xmlElement{attributes = As}) ->
    case get_attr(Name, As) of
    [#xmlAttribute{value = V}] ->
        V;
    [] -> ""
    end.

t_type([E=#xmlElement{name = typevar}]) ->
    t_var(E);
t_type([E=#xmlElement{name = atom}]) ->
    t_atom(E);
t_type([E=#xmlElement{name = integer}]) ->
    t_integer(E);
t_type([E=#xmlElement{name = range}]) ->
    t_range(E);
t_type([E=#xmlElement{name = binary}]) ->
    t_binary(E);
t_type([E=#xmlElement{name = float}]) ->
    t_float(E);
t_type([#xmlElement{name = nil}]) ->
    t_nil();
t_type([#xmlElement{name = paren, content = Es}]) ->
    t_paren(Es);
t_type([#xmlElement{name = list, content = Es}]) ->
    t_list(Es);
t_type([#xmlElement{name = nonempty_list, content = Es}]) ->
    t_nonempty_list(Es);
t_type([#xmlElement{name = map, content = Es}]) ->
    t_map(Es);
t_type([#xmlElement{name = tuple, content = Es}]) ->
    t_tuple(Es);
t_type([#xmlElement{name = 'fun', content = Es}]) ->
    ["fun("] ++ t_fun(Es) ++ [")"];
t_type([E = #xmlElement{name = record, content = Es}]) ->
    t_record(E, Es);
t_type([E = #xmlElement{name = abstype, content = Es}]) ->
    t_abstype(E, Es);
t_type([#xmlElement{name = union, content = Es}]) ->
    t_union(Es).

t_var(E) ->
    [get_attrval(name, E)].

t_atom(E) ->
    [get_attrval(value, E)].

t_integer(E) ->
    [get_attrval(value, E)].

t_range(E) ->
    [get_attrval(value, E)].

t_binary(E) ->
    [get_attrval(value, E)].

t_float(E) ->
    [get_attrval(value, E)].

t_nil() ->
    ["[]"].

t_paren(Es) ->
    ["("] ++ t_utype(get_elem(type, Es)) ++ [")"].

t_list(Es) ->
    ["["] ++ t_utype(get_elem(type, Es)) ++ ["]"].

t_nonempty_list(Es) ->
    ["["] ++ t_utype(get_elem(type, Es)) ++ [", ...]"].

t_tuple(Es) ->
    ["{"] ++ seq(fun t_utype_elem/1, Es, ["}"]).

t_fun(Es) ->
    ["("] ++ seq(fun t_utype_elem/1, get_content(argtypes, Es),
                 [") -> "] ++ t_utype(get_elem(type, Es))).

t_map(Es) ->
    Fs = get_elem(map_field, Es),
    ["#{"] ++ seq(fun t_map_field/1, Fs, ["}"]).

t_map_field(#xmlElement{content = [K,V]}) ->
    t_utype_elem(K) ++ [" => "] ++ t_utype_elem(V).

t_record(E, Es) ->
    Name = ["#"] ++ t_type(get_elem(atom, Es)),
    case get_elem(field, Es) of
        [] ->
            see(E, [Name, "{}"]);
        Fs ->
            see(E, Name) ++ ["{"] ++ seq(fun t_field/1, Fs, ["}"])
    end.

t_field(#xmlElement{content = Es}) ->
    t_type(get_elem(atom, Es)) ++ [" = "] ++ t_utype(get_elem(type, Es)).

t_abstype(E, Es) ->
    Name = t_name(get_elem(erlangName, Es)),
    case get_elem(type, Es) of
        [] ->
            see(E, [Name, "()"]);
        Ts ->
            see(E, [Name]) ++ ["("] ++ seq(fun t_utype_elem/1, Ts, [")"])
    end.

t_abstype(Es) ->
    ([t_name(get_elem(erlangName, Es)), "("]
     ++ seq(fun t_utype_elem/1, get_elem(type, Es), [")"])).

t_union(Es) ->
    seq(fun t_utype_elem/1, Es, " | ", []).

seq(F, Es, Tail) ->
    seq(F, Es, ", ", Tail).

seq(F, [E], _Sep, Tail) ->
    F(E) ++ Tail;
seq(F, [E | Es], Sep, Tail) ->
    F(E) ++ [Sep] ++ seq(F, Es, Sep, Tail);
seq(_F, [], _Sep, Tail) ->
    Tail.

local_defs(Es, Opts) ->
    local_defs(Es, [], Opts).

local_defs([], _, _Opts) -> [];
local_defs(Es0, Last, Opts) ->
    [E | Es] = lists:reverse(Es0),
    [?NL,
     {ul, [{class, "definitions"}],
      lists:reverse(lists:append([localdef(E1, [], Opts) || E1 <- Es]),
                    localdef(E, Last, Opts))}].

localdef(E = #xmlElement{content = Es}, Last, Opts) ->
    Name = case get_elem(typevar, Es) of
               [] ->
                   N0 = t_abstype(get_content(abstype, Es));
               [V] ->
                   N0 = t_var(V)
           end,
    [{li, format_type(Name, N0, get_elem(type, Es), Last, Opts)}].

unwrap_inline([]) -> [];
unwrap_inline([{inline, Elements}]) when is_list(Elements) -> Elements;
unwrap_inline({inline, Elements}) when is_list(Elements) -> Elements;
unwrap_inline([BString]) when is_binary(BString) -> BString;
unwrap_inline(BString) when is_binary(BString) -> BString.

debug(Tag, Content) ->
    docsh_lib:debug(Tag, "~s: ~p~n", [Tag, Content]),
    Content.

cleanup_lines(BString) when is_binary(BString) ->
    Lines = re:replace(BString, <<"\s*\n\s*">>, <<"\n">>, [global, {return, list}]),
    S = string:strip(lists:flatten(Lines), both, $\n),
    [ [T, "\n"] || T <- string:tokens(S, "\n") ];
cleanup_lines(IOList) ->
    BString = ?il2b([ unwrap_inline(E) || E <- IOList ]),
    cleanup_lines(BString).

fullDescription(Data, _Attrs, _Parents, _E) ->
    [{fmt, H} | T] = collect_loose_text(Data),
    ?il2b([H] ++ [ ["\n", E] || {fmt, E} <- T ]).

li(Data, _Attrs, _Parents, _E) ->
    item(li, fmt, Data).

dd(Data, _Attrs, _Parents, _E) ->
    item(dd, dd, Data).

item(Type, Out, Data) ->
    {Out, debug(Type, lists:flatmap(fun unwrap_fmt/1, collect_loose_text(Data)))}.

dl(Data, _Attrs, _Parents, _E) ->
    debug('dl:in', Data),
    {fmt, debug(dl, [ itemize(Type, undefined, undefined, Content)
                      || E <- Data,
                         {Type, Content} <- unwrap_dl_content(E) ])}.

unwrap_dl_content({dt, BString}) when is_binary(BString) -> [{dt, BString}];
unwrap_dl_content({dt, [BString]}) when is_binary(BString) -> [{dt, BString}];
unwrap_dl_content({dd, Lines}) when is_list(Lines) ->
    Length = length(Lines),
    [ {dd, append_dd_trailing_newline(Nth, Length, L)}
      || {Nth, L} <- enumerate(Lines) ];
unwrap_dl_content(C) ->
    debug('unwrap_dl_content:discard', C),
    [].

append_dd_trailing_newline(Length, Length, L) -> [L, "\n"];
append_dd_trailing_newline(_Nth, _Length, L) -> L.

list(Type, Data) ->
    %% Two passes to only enumerate items that get through the first pass.
    %% Whitespace is filtered out, the remainings are the list items.
    Items = enumerate([ Unwrapped
                        || E <- Data,
                           [_|_] = Unwrapped <- [unwrap_fmt(E)] ]),
    {fmt, debug(Type, [ debug(itemize, itemize(Type, N, Line, Item))
                        || {N, E} <- Items,
                           {Line, Item} <- enumerate(E) ])}.

-spec itemize(Type, Nth, Line, Content) -> iolist() when
      Type :: dl | ol | ul,
      Nth :: pos_integer(),
      Line :: pos_integer(),
      Content :: iolist().
itemize(dt, _Nth, _, Content) -> ["  ", Content, "\n\n"];
itemize(dd, _Nth, _, Content) -> ["      ", Content];
itemize(ol,  Nth, 1, Content) -> [io_lib:format("  ~b. ", [Nth]), Content];
itemize(ol, _Nth, _, Content) -> ["    ", Content];
itemize(ul, _Nth, 1, Content) -> ["  - ", Content];
itemize(ul, _Nth, _, Content) -> ["    ", Content].

enumerate(List) ->
    lists:zip(lists:seq(1, length(List)), List).

unwrap_fmt({fmt, Lines}) -> Lines;
unwrap_fmt(_) -> [].

collect_loose_text(Data) ->
    debug(loose, collect_loose_text(Data, [], [])).

collect_loose_text([], [], Fmt) -> Fmt;
collect_loose_text([], Data, Fmt) ->
    [{fmt, cleanup_lines(?il2b(lists:reverse(Data)))} | Fmt];
collect_loose_text([{inline, Element} | T], [], Fmt) ->
    collect_loose_text(T, [Element], Fmt);
collect_loose_text([{fmt, Element} | T], [], Fmt) ->
    [{fmt, Element} | collect_loose_text(T, [], Fmt)];
collect_loose_text([{inline, Element} | T], Data, Fmt) ->
    collect_loose_text(T, [Element | Data], Fmt);
collect_loose_text([{fmt, Element} | T], Data, Fmt) ->
    Clean = cleanup_lines(?il2b(lists:reverse(Data))),
    case Clean of
        [] -> [{fmt, Element}];
        _ -> [{fmt, Clean}, {fmt, Element}]
    end ++ collect_loose_text(T, [], Fmt);
collect_loose_text([LooseText | T], Data, Fmt) when is_binary(LooseText);
                                                    is_list(LooseText) ->
    collect_loose_text(T, [LooseText | Data], Fmt).

header(Level, Data) ->
    ["\n", header_prefix(Level), Data, "\n"].

header_prefix(h1) -> <<"# ">>;
header_prefix(h2) -> <<"## ">>;
header_prefix(h3) -> <<"### ">>.

%% Unused.
'#xml-inheritance#'() -> [].

module_details_from_attrs(Attrs) ->
    [ D || At <- Attrs, D <- [module_detail_from_attr(At)], D /= ignore ].

module_detail_from_attr(#xmlAttribute{name = name} = At) ->
    {name, ?l2ea(value(At))};
module_detail_from_attr(#xmlAttribute{}) ->
    ignore.

module_details_from_data(Data) ->
    lists:flatmap(fun module_detail_from_data/1, Data).

module_detail_from_data([{description, _}] = Desc) ->
    Desc;
module_detail_from_data([{functions, _}] = Functions) ->
    Functions;
module_detail_from_data(_) -> [].

function_details_from_attrs(Attrs, F) ->
    lists:foldl(fun fda/2, F, Attrs).

fda(#xmlAttribute{name = name} = At, F)     -> F#function{name = ?l2ea(value(At))};
fda(#xmlAttribute{name = arity} = At, F)    -> F#function{arity = ?l2i(value(At))};
fda(#xmlAttribute{name = exported} = At, F) -> F#function{exported = string_to_boolean(value(At))};
fda(#xmlAttribute{name = label} = At, F)    -> F#function{label = ?l2b(value(At))}.

function_details_from_data(Data, F) ->
    lists:foldl(fun fdd/2, F, Data).

fdd({description, Desc}, F) -> F#function{description = Desc};
fdd(_, F)                   -> F.

value(#xmlAttribute{value = V}) -> V.

string_to_boolean("yes") -> true;
string_to_boolean("no") -> false.
