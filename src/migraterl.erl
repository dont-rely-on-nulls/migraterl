-module(migraterl).

-export([init/0, migrate/2]).
-export([default_connection/0, init/2]).

-include("internal_types.hrl").

-define(PGHOST, os:getenv("PGHOST", "127.0.0.1")).
-define(PGPORT, list_to_integer(os:getenv("PGPORT", "5432"))).
-define(PGUSER, os:getenv("PGUSER", "admin")).
-define(PGPASSWORD, os:getenv("PGPASSWORD", "postgres")).
-define(PGDATABASE, os:getenv("PGDATABASE", "migraterl")).

-spec default_connection() -> {ok, epgsql:connection()} | {error, epgsql:connect_error()}.
default_connection() ->
    Connection =
        #{
            host => ?PGHOST,
            port => ?PGPORT,
            username => ?PGUSER,
            password => ?PGPASSWORD,
            database => ?PGDATABASE,
            timeout => 4000
        },
    epgsql:connect(Connection).

-spec format_bin_content(Bin :: binary()) -> [binary()].
format_bin_content(Bin) ->
    RemoveLineBreaks = binary:split(Bin, [<<"\n">>], [global]),
    Content = lists:map(fun(X) -> unicode:characters_to_list(X) end, RemoveLineBreaks),
    lists:concat(Content).

-spec init(Conn :: epgsql:connection(), Filename :: filename()) -> ok | {error, any()}.
init(Conn, Filename) ->
    {ok, Bin} = file:read_file(Filename),
    Content = format_bin_content(Bin),
    case epgsql:squery(Conn, Content) of
        {ok, _, _} -> ok;
        Otherwise -> {error, Otherwise}
    end.

%-spec init(Conn :: epgsql:connection()) -> ok.
init() ->
    {ok, Files} = file_utils:read_system_migrations(?MODULE),
    io:format("~p~n", [Files]),
    {ok, Conn} = default_connection(),
    % TODO: Rewrite this to be in a single transaction
    _X = lists:map(fun(F) -> init(Conn, F) end, Files),
    ok.

migrate(_Path, _Query) ->
    ok.
