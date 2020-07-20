-module(memery_db).
-export([install/1, store_meme/5, fetch_meme/1, meme_by_name/1]).

-include_lib("stdlib/include/ms_transform.hrl").

-record(meme, {uuid :: uuid:uuid(),
               name :: string(),
               description :: string() | undefined,
               tags :: [string()],
               path :: string()
              }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec install([node()]) -> ok.
install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(meme,
                        [{attributes, record_info(fields, meme)},
                         {index, [#meme.name]},
                         {disc_copies, Nodes}]),
    rpc:multicall(Nodes, application, stop, [mnesia]),
    ok.

-spec store_meme(string(), string(), [string()], string(), string()) ->
    string().
store_meme(Name, Description, Tags, Source, Outpath) ->
    [Type|_] = lists:reverse(string:split(Source, ".", all)),
    UUID = uuid:get_v4(),
    SID = uuid:uuid_to_string(UUID),
    Output = Outpath ++ "/" ++ SID ++ "." ++ Type,
    {ok, _} = file:copy(Source, Output),
    Meme = #meme{uuid=UUID,
                 name=Name,
                 description=Description,
                 tags=Tags,
                 path=Output},
    mnesia:activity(transaction, fun() -> mnesia:write(Meme) end),
    SID.


-spec fetch_meme(string()) -> [maps:map(string(), string() | [string()])].
fetch_meme(ID) ->
    UUID = uuid:string_to_uuid(ID),
    Memes = mnesia:activity(async_dirty, fun() -> mnesia:read(meme, UUID) end),
    lists:map(fun record_to_map/1, Memes).

-spec meme_by_name(string()) -> [maps:map(string(), string() | [string()])].
meme_by_name(Name) ->
    Match = ets:fun2ms(fun(M=#meme{name = N}) when N =:= Name -> M end),
    Select = fun() -> mnesia:select(meme, Match) end,
    Memes = mnesia:activity(async_dirty, Select),
    lists:map(fun record_to_map/1, Memes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

record_to_map(#meme{uuid=UUID, name=Name, description=Description, tags=Tags,
                    path=Path}) ->
    #{uuid => UUID,
      name => Name,
      description => Description,
      tag => Tags,
      path => Path
     }.
