-module(memery_db).
-export([install/1]).

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
