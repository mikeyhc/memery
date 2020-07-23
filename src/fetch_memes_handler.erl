-module(fetch_memes_handler).
-behaviour(cowboy_rest).

-export([init/2, content_types_provided/2, to_json/2]).

-define(DEFAULT_LIMIT, 20).

init(Req, State) ->
    {cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

to_json(Req, State) ->
    ParsedQs = cowboy_req:parse_qs(Req),
    Limit = case lists:keyfind(<<"limit">>, 1, ParsedQs) of
                {_, V} -> binary_to_integer(V);
                false -> ?DEFAULT_LIMIT
            end,
    Memes0 = memery_db:random_memes(Limit),
    Memes = lists:map(fun path_to_uri/1, Memes0),
    {jsone:encode(Memes), Req, State}.

% TODO refactor this to a common module shared with fetch_meme_handler
path_to_uri(M=#{<<"path">> := BinPath}) ->
    % TODO use a bin split
    Path = binary:bin_to_list(BinPath),
    [Filename|_] = lists:reverse(string:split(Path, "/", all)),
    M#{<<"path">> => binary:list_to_bin("/meme/" ++ Filename)}.
