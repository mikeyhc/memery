-module(status_handler).

-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"application/json">>},
                           jsone:encode(#{<<"status">> => <<"ok">>}),
                           Req0),
    {ok, Req, State}.
