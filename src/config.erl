%%% Module for reading, validating and translating attila configuration
%%% files to usable data structures.

-module(config).

%%% Exports

-export([read/1,
         config/1,
         txn_spec/4]).

%%% Records

-record(config, { txn_specs :: [txn_spec()] }).

-record(txn_spec, { method      :: method(),
                    path        :: binary(),
                    interval    :: pos_integer(),
                    concurrency :: pos_integer() }).

%%% Specs

-type config() :: #config{}.
-type txn_spec() :: #txn_spec{}.
-type method() :: get.

-spec read(file:name_all()) -> {ok, config()} |
                               {error, {io_error, term()}} |
                               {error, {invalid_term, non_neg_integer()}}.

%%% API

read(Path) ->
    case file:consult(Path) of
        {ok, Terms} ->
            try_make_config(Terms);
        {error, {Line, _Module, _Term}} ->
            {error, {invalid_term, Line}};
        {error, Reason} ->
            {error, {io_error, Reason}}
    end.

config(TxnSpecs) ->
    #config{ txn_specs = TxnSpecs }.

txn_spec(Method, Path, Interval, Concurrency) ->
    #txn_spec{ method       = Method,
               path         = Path,
               interval     = Interval,
               concurrency  = Concurrency }.

%%% Utilities

try_make_config(Terms) ->
    TxnEntries = get_txn_entries(Terms),
    TxnSpecs = lists:map(fun try_make_txn_spec/1, TxnEntries),

