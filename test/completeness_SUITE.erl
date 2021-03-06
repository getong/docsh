-module(completeness_SUITE).
-compile([export_all, nowarn_export_all]).

-import(docsh_helpers, [sh/2]).

%init_per_suite(_) -> {skip, "work in progress"}.

all() ->
    [
     sanity_check,
     %edoc_in_otp,
     %intermediate_representations,
     docsh_works_for_each_file_with_edoc
    ].

sanity_check(_) -> ok.

edoc_in_otp(_) ->
    %put(sh_log, true),
    Stats = [ app_stats(app_sources(App), [has_edoc], [no_details]) || App <- apps() ],
    %Stats = [ app_stats(app_sources(App), [has_edoc, has_comments], [no_details]) || App <- apps() ],
    ct:pal("edoc in OTP: ~p", [Stats]),
    {skip, "we run this just for the stats printout"}.

docsh_works_for_each_file_with_edoc(_) ->
    Stats = [ app_stats(app_sources(App), [has_edoc], []) || App <- apps() ],
    ModsSources = [ ModSource
                    || {_App, AppStats} <- Stats,
                       ModSource <- case lists:keyfind([has_edoc], 1, AppStats) of
                                         false -> [];
                                         {_, _, Sources} -> Sources
                                     end ],
    %ct:pal("sources: ~p", [ModsSources]),
    ModsEDocResults =
        [ {Mod, Result}
          || {Mod, _Features, _Source} <- ModsSources,
             Result <- [try docsh_edoc:to_internal(element(2, docsh_beam:from_loaded_module(Mod))) of
                            {ok, _Internal} -> ok;
                            Error -> Error
                        catch
                            _:R  -> {error, R}
                        end] ],
    %ct:pal("results: ~p", [ModsEDocResults]),
    %ct:pal("results length check: ~p ~p ~p",
    %       [length(ModsSources), length(ModsEDocResults),
    %        length(ModsSources) == length(ModsEDocResults)]),
    EDocFailed = [ {M, R} || {M, R} <- ModsEDocResults, R /= ok ],
    ct:pal("failed: ~p", [EDocFailed]),
    case EDocFailed of
        [_|_] ->
            ct:pal("docsh_edoc fails for ~p out of ~p modules",
                   [length(EDocFailed), length(ModsEDocResults)]),
            {skip, "run just for the analysis printout"};
            %% TODO: uncomment this one day
            %ct:fail("docsh_edoc fails for ~p out of ~p modules",
            %        [length(EDocFailed), length(ModsEDocResults)]);
        _ ->
            ok
    end.

intermediate_representations(Config) ->
    PrivDir = test_server:lookup_config(priv_dir, Config),
    Stats = [ app_stats(app_sources(App), [has_edoc], []) || App <- apps() ],
    ModsSources = [ ModSource
                    || {_App, AppStats} <- Stats,
                       ModSource <- case lists:keyfind([has_edoc], 1, AppStats) of
                                        false -> [];
                                        {_, _, Sources} -> Sources
                                    end ],
    [ ok
      || {Mod, _Features, _Source} <- ModsSources,
         _ <- [try docsh_edoc:to_internal(element(2, docsh_beam:from_loaded_module(Mod)),
                                          [{debug, {PrivDir, [source, edoc, html, internal, otpsgml]}}]) of
                   {ok, _Internal} -> ok;
                   Error -> Error
               catch
                   _:R  -> {error, R}
               end] ],
    {skip, "we run this just for the on disk results: " ++ PrivDir}.

apps() ->
    [
     %asn1,
     common_test,
     compiler,
     %cosEvent,
     %cosEventDomain,
     %cosFileTransfer,
     %cosNotification,
     %cosProperty,
     %cosTime,
     %cosTransactions,
     crypto,
     debugger,
     dialyzer,
     diameter,
     edoc,
     eldap,
     erl_docgen,
     erl_interface,
     erts,
     et,
     eunit,
     %hipe,
     ic,
     inets,
     %jinterface,
     kernel,
     %megaco,
     mnesia,
     observer,
     odbc,
     %orber,
     os_mon,
     otp_mibs,
     parsetools,
     public_key,
     reltool,
     runtime_tools,
     sasl,
     snmp,
     ssh,
     ssl,
     stdlib,
     syntax_tools,
     tools,
     wx,
     xmerl
    ].

app_sources(App) ->
    load_application(App),
    {ok, Modules} = application:get_key(App, modules),
    ModulesSources = app_modules_sources(App, Modules),
    {App, ModulesSources}.

app_modules_sources(App, Modules) ->
    [ ModSource
      || M <- Modules,
         {_, _} = ModSource <-
             [case docsh_beam:from_loaded_module(M) of
                  {ok, B} ->
                      {M, docsh_beam:source_file(B)};
                  R ->
                      ct:pal("can't get source for ~p ~p: ~p", [App, M, R]),
                      skip
              end] ].

app_stats({App, ModulesSources}, Predicates, Opts) ->
    WithEdocFlag = [ WithFlag
                     || {M, Source} <- ModulesSources,
                        {_, _, _} = WithFlag <-
                            [try
                                 Features = [ ?MODULE:P(Source) || P <- Predicates ],
                                 {M, Features, Source}
                             catch
                                 _:_ -> ct:pal("can't tell if has edoc: ~p ~p ~p", [App, M, Source]),
                                        skip
                             end] ],
    Stats = docsh_lib:group_by(fun ({_,Features,_}) -> Features end,
                               WithEdocFlag),
    case proplists:is_defined(no_details, Opts) of
        false ->
            {App, [ {Feature, length(Items), Items} || {Feature, Items} <- dict:to_list(Stats) ]};
        true ->
            {App, [ {Features, length(Items)} || {Features, Items} <- dict:to_list(Stats) ]}
    end.

load_application(App) ->
    case application:load(App) of
        ok -> ok;
        {error, {already_loaded, App}} -> ok;
        {error, R} -> error({cannot_load, App, R})
    end.

has_edoc(SourceFile) ->
    %% It's **so** unlikely that a file is EDoc-documented, but does not use the @doc tag.
    case sh(["grep -F @doc ", SourceFile, " > /dev/null"], [dont_fail]) of
        {done, 0, _} -> has_edoc;
        {done, _, _} -> no_edoc
    end.

has_comments(SourceFile) ->
    case sh(["grep -F '%%' ", SourceFile, " > /dev/null"], [dont_fail]) of
        {done, 0, _} -> has_comments;
        {done, _, _} -> no_comments
    end.
