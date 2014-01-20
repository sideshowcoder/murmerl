-module(murmerl).

-export([
         murmur3_x86_32/2,
         murmur3_x86_128/2,
         murmur3_x64_128/2
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


murmur3_x86_32(_, _) -> exit(nif_library_no_loaded).
murmur3_x86_128(_, _) -> exit(nif_library_no_loaded).
murmur3_x64_128(_, _) -> exit(nif_library_no_loaded).

-ifdef(TEST).

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
