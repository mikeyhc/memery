-module(store_meme_handler).
-behaviour(cowboy_rest).

-export([init/2, allowed_methods/2, content_types_accepted/2, from_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

from_json(Req0, State=#{tmp_img_dir := Dir}) ->
    {ok, Data, Req1} = cowboy_req:read_body(Req0),
    #{<<"name">> := Name,
      <<"description">> := Description,
      <<"tags">> := Tags,
      <<"filename">> := SourceFile,
      <<"data">> := EncodedImage} = jsone:decode(Data),
    [Suffix|_] = lists:reverse(
                   string:split(binary:bin_to_list(SourceFile), ".", all)),
    Image = base64:decode(EncodedImage),
    <<Hash:256>> = crypto:hash(sha256, Image),
    StrHash = io_lib:format("~64.16.0b", [Hash]),
    BinHash = binary:list_to_bin(StrHash),
    case memery_db:fetch_meme(BinHash) of
        [] ->
            StrPath = lists:flatten(io_lib:format("~s/~s.~s",
                                                  [Dir, StrHash, Suffix])),
            Path = binary:list_to_bin(StrPath),
            ok = file:write_file(Path, Image),
            UUID = memery_db:store_meme(Name, Description, Tags, Path, BinHash),
            Reply = #{<<"status">> => <<"ok">>, <<"id">> => UUID},
            Headers = #{<<"content-type">> => <<"application/json">>},
            Req2 = cowboy_req:set_resp_body(jsone:encode(Reply), Req1),
            Req = cowboy_req:set_resp_headers(Headers, Req2),
            {true, Req, State};
        _ -> {{true, "/meme/" ++ StrHash}, Req1, State}
    end.
