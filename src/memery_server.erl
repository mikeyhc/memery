-module(memery_server).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).

init([]) ->
    {ok, []}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
