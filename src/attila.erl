-module(attila).

%%% Exports
-export([main/1]).

%%% Defaults

-define(DEFAULT_INTERVAl, 1).
-define(DEFAULT_CONCURRENCY, 1).

%%% API

main(Args) ->
    case Args of
        [ConfigPath] ->
            case config:read(ConfigPath) of
                {ok, Config} ->
                    io:format("Valid config: ~p~n", [Config]),
                    erlang:halt(0);
                {error, Reason} ->
                    fatal_format("Failure: ~p~n", [Reason])
            end;
        _ ->
            fatal_format("Usage: attila <config_path>~n", [])
    end.

%%% Utilities

fatal_format(Format, Args) ->
    io:format(Format, Args),
    erlang:halt(1).
