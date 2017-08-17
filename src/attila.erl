-module(attila).

%%% Exports
-export([main/1]).

%%% Defaults

-define(DEFAULT_INTERVAl, 1).
-define(DEFAULT_CONCURRENCY, 1).

%%% API

main(Args) ->
    Entries = case Args of
                  [ConfigPath] ->
                      read_config(ConfigPath);
                  _ ->
                      fatal_format("Usage: attila <config_path>~n", [])
              end,
    InitialOptions = validate_entries(Entries),
    Options = enrich_with_defaults(InitialOptions),
    io:format("Valid options: ~p~n", [Options]),
    erlang:halt(0).

%%% Initial option validation

validate_entries(Options) ->
    lists:foldl(fun validate_entry/2, maps:new(), Options).

validate_entry(Entry, Options) ->
    case Entry of
        {host, Host} ->
            validate_host(Host, Options);
        {interval, Interval} ->
            validate_interval(Interval, Options);
        {concurrency, Concurrency} ->
            validate_concurrency(Concurrency, Options);
        Other ->
            fatal_format("Unknown entry: ~p~n", [Other])
    end.

validate_host(Host, Options) ->
    validate_initial_option(host, Host, "string", Options).

validate_interval(Interval, Options) ->
    validate_initial_option(interval, Interval, "positive integer", Options).

validate_concurrency(Concurrency, Options) ->
    validate_initial_option(concurrency, Concurrency, "positive integer", Options).

validate_initial_option(Key, Value, RequiredType, Options) ->
    case maps:find(Key, Options) of
        {ok, _PreviousValue} ->
            fatal_format("Duplicate entry for option \"~p\": ~p~n", [Key, Value]);
        error ->
            check_value(Key, Value, RequiredType, Options)
    end.

%%% Option enrichment with defaults

enrich_with_defaults(Options) ->
    Enrichers = [fun enrich_with_interval/1,
                 fun enrich_with_concurrency/1],
    lists:foldl(fun enrich_with_default/2, Options, Enrichers).

enrich_with_default(Enricher, Options) ->
    Enricher(Options).

enrich_with_interval(Options) ->
    enrich_with_entry(interval, ?DEFAULT_INTERVAl, Options).

enrich_with_concurrency(Options) ->
    enrich_with_entry(concurrency, ?DEFAULT_CONCURRENCY, Options).

enrich_with_entry(Key, DefaultValue, Options) ->
    case maps:find(Key, Options) of
        {ok, _Value} ->
            Options;
        error ->
            maps:put(Key, DefaultValue, Options)
    end.

%%% Miscellaneous utilities

read_config(Path) ->
    case file:consult(Path) of
        {ok, Terms} ->
            Terms;
        {error, {Line, _Module, _Term}} ->
            fatal_format("Invalid term in \"~s\" at line ~p.~n", [Path, Line]);
        {error, Reason} ->
            fatal_format("Failed to read configuration file at \"~s\": ~s.~n",
                         [Path, file:format_error(Reason)])
    end.

check_value(Key, Value, RequiredType, Options) ->
    Pred = case RequiredType of
               "string" ->
                   fun erlang:is_list/1;
               "positive integer" ->
                   fun(X) ->
                           erlang:is_integer(X) andalso X > 0
                   end
           end,
    case Pred(Value) of
        true ->
            maps:put(Key, Value, Options);
        false ->
            fatal_format("Invalid type for option \"~p\" (must be ~s): ~p~n",
                         [Key, RequiredType, Value])
    end.

fatal_format(Format, Args) ->
    io:format(Format, Args),
    erlang:halt(1).
