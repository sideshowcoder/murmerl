-module(murmerl).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(TEST).

works_test() ->
    ?assert(true).

-endif.
