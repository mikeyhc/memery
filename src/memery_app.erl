%%%-------------------------------------------------------------------
%% @doc memery public API
%% @end
%%%-------------------------------------------------------------------

-module(memery_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mnesia:wait_for_tables([meme], 5000),
    memery_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
