-module(heja_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([test_facade_api/1]).

all() -> [test_facade_api].

init_per_suite(Config) ->
    {ok, _} = heja:start(),
    Config.

end_per_suite(_Config) ->
    heja:stop().

%% Tests ----------------------------------------------------------------------

test_facade_api(_Config) ->
    #{} = heja:status(),

    {ok, Ref} = heja:new(facade_test_api),
    ok = setup_api(Ref),
    ok = heja:serve(Ref, 8080),

    #{{facade_test_api,1} := #{dispatch := [{delete,<<"/api/v1/users/:id">>},
                                            {get,<<"/api/v1/users/:id">>},
                                            {post,<<"/api/v1/users">>},
                                            {put,<<"/api/v1/users/:id">>}],
                               status := {running,8080}}} = heja:status(),

    %% Create user Alice
    Body = lejson:encode(#{user => #{name => "Alice"}}),
    {ok, Resp1} = request(post, "http://localhost:8080/api/v1/users", Body),
    true = assert_application_json(headers(Resp1)),
    200 = status(Resp1),

    %% Reuse Id from POST call in subsequent calls
    #{<<"id">> := Id} = lejson:decode(body(Resp1)),
    IdStr = binary_to_list(Id),

    %% Ensure Alice exists
    {ok, Resp2} = request(get, "http://localhost:8080/api/v1/users/" ++ IdStr),
    #{<<"user">> := #{<<"name">> := "Alice"}} = lejson:decode(body(Resp2)),
    true = assert_application_json(headers(Resp2)),
    200 = status(Resp2),

    %% Replace Alice with Bob
    Body2 = lejson:encode(#{user => #{name => "Bob"}}),
    {ok, Resp3} = request(put, "http://localhost:8080/api/v1/users/" ++ IdStr, Body2),
    true = assert_application_json(headers(Resp3)),
    200 = status(Resp3),

    %% Ensure Bob exists
    {ok, Resp4} = request(get, "http://localhost:8080/api/v1/users/" ++ IdStr),
    #{<<"user">> := #{<<"name">> := "Bob"}} = lejson:decode(body(Resp4)),
    true = assert_application_json(headers(Resp4)),
    200 = status(Resp4),

    %% Delete Bob
    {ok, Resp5} = request(delete, "http://localhost:8080/api/v1/users/" ++ IdStr),
    #{<<"deleted">> := Id} = lejson:decode(body(Resp5)),
    true = assert_application_json(headers(Resp5)),
    200 = status(Resp5),

    %% Ensure Bob is deleted
    {ok, Resp6} = request(get, "http://localhost:8080/api/v1/users/" ++ IdStr),
    #{<<"error">> := <<"not_found">>} = lejson:decode(body(Resp6)),
    true = assert_application_json(headers(Resp6)),
    400 = status(Resp6),

    ok = heja:deserve(Ref),
    ok = teardown_api(Ref),
    ok = heja:del(Ref),

    #{} = heja:status(),

    true.

%% Fixtures -------------------------------------------------------------------

setup_api(Ref) ->
    users = ets:new(users, [public, named_table]),

    ok = heja:post(Ref, "/api/v1/users",
                   fun(Body, _Context) ->
                       Id = id(),
                       true = ets:insert(users, {Id, Body}),
                       {ok, #{id => Id}}
                   end),

    ok = heja:get(Ref, "/api/v1/users/:id",
                  fun(_Body, #{id:=Id}) ->
                      case ets:lookup(users, Id) of
                          [] ->
                              {error, not_found};
                          [{_, User}] ->
                              {ok, User}
                      end
                  end),

    ok = heja:put(Ref, "/api/v1/users/:id",
                  fun(Body, #{id:=Id}) ->
                      true = ets:insert(users, {Id, Body}),
                      {ok, #{id=> Id}}
                  end),

    ok = heja:delete(Ref, "/api/v1/users/:id",
                     fun(_Body, #{id:=Id}) ->
                         true = ets:delete(users, Id),
                         {ok, #{deleted => Id}}
                     end).

teardown_api(Ref) ->
    ok = heja:del_handler(Ref, post, "/api/v1/users"),
    ok = heja:del_handler(Ref, get, "/api/v1/users/:id"),
    ok = heja:del_handler(Ref, put, "/api/v1/users/:id"),
    ok = heja:del_handler(Ref, delete, "/api/v1/users/:id").

id() ->
    <<"c08ba9b6-4cb3-4750-a141-64f5d81b904c">>.

%% Internal -------------------------------------------------------------------

request(Method, Url) ->
    httpc:request(Method, {Url, []}, [], []).

request(Method, Url, Body) ->
    ContentType = "application/json",
    httpc:request(Method, {Url, [], ContentType, Body}, [], []).

status({{_, Status, _}, _, _}) -> Status.
body({_, _, Body})             -> Body.
headers({_, Headers, _})       -> lists:sort(Headers).

assert_application_json(Headers) ->
    lists:member({"content-type","application/json"}, Headers).
