-module(upload_query).

-export([start/0]).

-spec start() -> 'ok'.
start() ->
    {ok, _} = application:ensure_all_started(upload_query),
    ok.
