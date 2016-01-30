-module(edoc_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("proplists_eq.hrl").

-define(eq(Expected, Actual), ?proplists_eq(Expected, Actual)).
-define(TESTED, docsh_edoc).

all() ->
    [edoc_to_internal,
     edoc_format,
     edoc_format_code_in_loose_text,
     edoc_format_p,
     edoc_format_pre,
     edoc_format_text1,
     edoc_format_text2,
     edoc_format_ul].

edoc_to_internal(_) ->
    File = source_file(edoc_example),
    ct:pal("~p", [File]),
    ?eq([{module, [{name, edoc_example},
                   {description, <<"Top-level module doc.">>}]},
         {function, [{name, f},
                     {arity, 0},
                     {exported, true},
                     {label, <<"f-0">>},
                     {description, <<"Doc for f/0.">>}]}],
        ?TESTED:to_internal(File)).

edoc_format(_) ->
    File = source_file(edoc_example2),
    D = function_description({g,0}, ?TESTED:to_internal(File)),
    ct:pal("~s", [D]),
    ?eq([<<"g() returns a more complex value,\n"
           "while its documentation uses more complex markup.\n"
           "\n"
           "  Why?\n"
           "  To test the documentation extraction and formatting process.\n"
           "\n"
           "  Any other reason?\n"
           "  Not really.\n"
           "\n"
           "  1. Some\n"
           "  2. Random\n"
           "  3. Items\n"
           "\n"
           "  - One\n"
           "  - Two\n"
           "  - Three:\n"
           "      - a\n"
           "      - b\n"
           "      - c\n">>],
        [D]).

edoc_format_code_in_loose_text(C) ->
    edoc_format(C, code_in_loose_text, <<"Fetch the internal state of an OTP process.\n"
                                         "Calls sys:get_state/2 directly in R16B01+, and fetches\n"
                                         "it dynamically on older versions of OTP.">>).

edoc_format_p(C) ->
    edoc_format(C, p, <<"Just\n"
                        "a paragraph.">>).

edoc_format_pre(C) ->
    edoc_format(C, pre, <<"    pre\n"
                          "      formatted\n"
                          "        text">>).

edoc_format_text1(C) ->
    edoc_format(C, text1, <<"Some\n"
                            "loose text,\n"
                            "not a paragraph.">>).

edoc_format_text2(C) ->
    edoc_format(C, text2, <<"Some\n"
                            "loose text,\n"
                            "not a paragraph.\n"
                            "\n"
                            "A paragraph.">>).

edoc_format_ul(C) ->
    edoc_format(C, ul, <<"  - Module is any atom representing a module\n"
                         "  - Function is any atom representing a function, or the wildcard\n"
                         "    '_'\n"
                         "  - Args is either the arity of a function (0..255), a wildcard\n"
                         "    pattern ('_'), a\n"
                         "    match specification,\n"
                         "    or a function from a shell session that can be transformed into\n"
                         "    a match specification">>).

edoc_format(_, Element, Expected) ->
    File = source_file(edoc_example2),
    D = function_description({Element, 0}, ?TESTED:to_internal(File)),
    ct:pal("~s", [D]),
    ?eq([Expected], [D]).

function_description(F, Docs) ->
    Functions = proplists:get_all_values(function, Docs),
    {F,_,_,{description, D}} = lists:keyfind(F, 1, Functions),
    D.

source_file(Mod) ->
    proplists:get_value(source, Mod:module_info(compile)).
