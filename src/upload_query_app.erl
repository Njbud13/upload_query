-module(upload_query_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([         
            {'_', [
                {"/upload", upload_handler, []}
            ]}
    ]),     
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch},
        middlewares => [cowboy_router, cowboy_handler]
    }),
    upload_query_sup:start_link().

stop(_State) ->
    ok.
