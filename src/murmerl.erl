-module(murmerl).

-export([
         murmur3_32/2,
         murmur3_128/3,
         murmur3_128/2
        ]).

-on_load(init/0).

-define(APPNAME, murmerl).
-define(LIBNAME, murmerl_drv).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

%% ===================================================================
%% API
%% ===================================================================

% @doc murmur3_32/2 is a wrapper for the murmur3 C library by Peter Scott,
% it takes a passed iolist and a seed value and runs the murmur3 hash on the
% iolist the returned binary is the 32Bit hash value. The function is
% implemented as a NIF as murmur3_x86_32.
-spec murmur3_32(string(), integer()) -> binary().
murmur3_32(Value, Seed) ->
    murmur3_x86_32(Value, Seed).

% @doc murmur3_128/2 is a wrapper for the murmur3 C library by Peter Scott, it
% takes a passed iolist and a seed value and runs the murmur3 hash on the
% iolist the returned binary is the 128Bit hash value. This function defaults
% to the x64 optimised implementation of murmur3 with 128 Bit return.  The
% function is implemented as a NIF as murmur3_x64_128.
-spec murmur3_128(string(), integer()) -> binary().
murmur3_128(Value, Seed) ->
    murmur3_x64_128(Value, Seed).

% @doc murmur3_128/3 is a wrapper for the murmur3 C library by Peter Scott, it
% takes a passed iolist and a seed value and runs the murmur3 hash on the
% iolist the returned binary is the 128Bit hash value. This function allows to
% select either the x86 optimized version by passing x86 or the x64 optimized
% version by passing x64 in either case it will return the 128 Bit hash.  The
% function is implemented as a NIF as murmur3_x64_128 / murmur3_x86_128.
-spec murmur3_128(string(), integer(), x64|x86) -> binary().
murmur3_128(Value, Seed, x86) ->
    murmur3_x86_128(Value, Seed);
murmur3_128(Value, Seed, x64) ->
    murmur3_x64_128(Value, Seed).

%% ===================================================================
%% NIF Fallbacks
%% ===================================================================
murmur3_x86_32(_, _) ->
    erlang:nif_error({error, not_loaded}).

murmur3_x86_128(_, _) ->
    erlang:nif_error({error, not_loaded}).

murmur3_x64_128(_, _) ->
    erlang:nif_error({error, not_loaded}).

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

murmur3_32_test() ->
    Value = murmur3_32("foo", 23),
    Other = murmur3_x86_32("foo", 23),
    ?assert(Value == Other).

murmur3_128_default_test() ->
    Value = murmur3_128("foo", 23),
    Other = murmur3_x64_128("foo", 23),
    ?assert(Value == Other).

murmur3_128_x86_version_test() ->
    Value = murmur3_128("foo", 23, x86),
    Other = murmur3_x86_128("foo", 23),
    ?assert(Value == Other).

murmur3_128_x64_version_test() ->
    Value = murmur3_128("foo", 23, x64),
    Other = murmur3_x64_128("foo", 23),
    ?assert(Value == Other).

murmur3_x86_32_test() ->
    Value = murmur3_x86_32("foo", 23),
    Other = murmur3_x86_32("bar", 23),
    ?assert(bit_size(Value) == 32),
    ?assert(is_binary(Value)),
    ?assert(is_binary(Other)),
    ?assertNot(Value == Other).

murmur3_x86_128_test() ->
    Value = murmur3_x86_128("foo", 23),
    Other = murmur3_x86_128("foo", 42),
    ?assert(bit_size(Value) == 128),
    ?assert(is_binary(Value)),
    ?assert(is_binary(Other)),
    ?assertNot(Value == Other).

murmur3_x64_128_test() ->
    Value = murmur3_x64_128("foo", 23),
    Other = murmur3_x64_128("bar", 23),
    io:format("Value: ~p", [Value]),
    ?assert(bit_size(Value) == 128),
    ?assert(is_binary(Value)),
    ?assert(is_binary(Other)),
    ?assertNot(Value == Other).

-endif.
