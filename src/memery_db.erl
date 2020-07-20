-module(memery_db).
-export([install/1, store_meme/5]).

-record(meme, {uuid :: uuid:uuid(),
               name :: string(),
               description :: string() | undefined,
               tags :: [string()],
               path :: string()
              }).

install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(meme,
                        [{attributes, record_info(fields, meme)},
                         {index, [#meme.name]},
                         {disc_copies, Nodes}]),
    rpc:multicall(Nodes, application, stop, [mnesia]).

store_meme(Name, Description, Tags, Source, Outpath) ->
    [Type|_] = lists:reverse(string:split(Source, ".", all)),
    UUID = uuid:get_v4(),
    Output = Outpath ++ "/" ++ UUID ++ "." ++ Type,
    {ok, _} = file:copy(Source, Output),
    Meme = #meme{uuid=UUID,
                 name=Name,
                 description=Description,
                 tags=Tags,
                 path=Output},
    mnesia:activity(transaction, fun() -> mnesia:write(Meme) end).
