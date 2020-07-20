%%%-------------------------------------------------------------------
%% @doc memery public API
%% @end
%%%-------------------------------------------------------------------

-module(memery_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Routes = [{'_', [{"/", status_handler, []}]}],
    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_clear(memery_listener,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch}}),
    mnesia:wait_for_tables([meme], 5000),
    memery_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
