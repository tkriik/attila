-module(config_test).

-include_lib("eunit/include/eunit.hrl").

-define(CONFIG_DIR, <<"test/data/config">>).

%%% Tests

valid_minimal_config_test() ->
    Expected = {ok, config:config([])},
    Actual = read_config("valid_minimal.conf"),
    ?assertEqual(Expected, Actual).

nonexistent_config_test() ->
    Expected = {error, {io_error, enoent}},
    Actual = read_config("nonexistent.conf"),
    ?assertEqual(Expected, Actual).

invalid_format_config_test() ->
    Expected = {error, {invalid_term, 3}},
    Actual = read_config("invalid_format.conf"),
    ?assertEqual(Expected, Actual).

%%% Utilities

read_config(ConfigName) ->
    ConfigPath = filename:join(?CONFIG_DIR, ConfigName),
    config:read(ConfigPath).
