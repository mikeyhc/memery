-module(fetch_meme_handler).
-behaviour(cowboy_rest).

-export([init/2, resource_exists/2, content_types_provided/2, to_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

resource_exists(Req, State) ->
    ID = cowboy_req:binding(meme_id, Req),
    case memery_db:fetch_meme(ID) of
        [] -> {false, Req, State};
        Memes -> {true, Req, State#{memes => Memes}}
    end.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

to_json(Req, State=#{memes := Memes0}) ->
    Memes = lists:map(fun path_to_uri/1, Memes0),
    {jsone:encode(Memes), Req, State}.

path_to_uri(M=#{<<"path">> := BinPath}) ->
    % TODO use a bin split
    Path = binary:bin_to_list(BinPath),
    [Filename|_] = lists:reverse(string:split(Path, "/", all)),
    M#{<<"path">> => binary:list_to_bin("/meme/" ++ Filename)}.
