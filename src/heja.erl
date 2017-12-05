%%  Description: heja facade
%%

-module(heja).

%% Management API
-export([start/0, stop/0]).

%% API
-export([new/1, new/2, new/3, del/1,
         get/3, post/3, put/3, delete/3,
         del_handler/3,
         serve/2, deserve/1,
         status/0]).

%% Management API -------------------------------------------------------------

-spec start() -> {ok, [atom()]} | {error, atom()}.
start() ->
    application:ensure_all_started(?MODULE).

-spec stop() -> ok | {error, atom()}.
stop() ->
    application:stop(?MODULE).

%% API ------------------------------------------------------------------------

-spec new(atom()) ->
    {ok, reference()} | {error, atom()}.
new(Name) ->
    new(Name, 1, #{}).

-spec new(atom(), non_neg_integer()) ->
    {ok, reference()} | {error, atom()}.
new(Name, Version) ->
    new(Name, Version, #{}).

-spec new(atom(), non_neg_integer(), map()) ->
    {ok, reference()} | {error, atom()}.
new(Name, Version, Opts) ->
    heja_api:new(Name, Version, Opts).

-spec del(reference()) -> ok.
del(Ref) ->
    heja_api:del(Ref).

-spec get(reference(), binary(), function()) -> ok | {error, atom()}.
get(Ref, Path, Fun) ->
    heja_api:add_handler(Ref, get, to_bin(Path), Fun).

-spec post(reference(), binary(), function()) -> ok | {error, atom()}.
post(Ref, Path, Fun) ->
    heja_api:add_handler(Ref, post, to_bin(Path), Fun).

-spec put(reference(), binary(), function()) -> ok | {error, atom()}.
put(Ref, Path, Fun) ->
    heja_api:add_handler(Ref, put, to_bin(Path), Fun).

-spec delete(reference(), binary(), function()) -> ok | {error, atom()}.
delete(Ref, Path, Fun) ->
    heja_api:add_handler(Ref, delete, to_bin(Path), Fun).

-spec del_handler(reference(), get|post|put|delete, binary()) -> ok.
del_handler(Ref, Method, Path) ->
    heja_api:del_handler(Ref, Method, to_bin(Path)).

-spec serve(reference(), non_neg_integer()) -> ok | {error, not_found}.
serve(Ref, Port) ->
    heja_api:serve(Ref, Port).

-spec deserve(reference()) -> ok.
deserve(Ref) ->
    heja_api:deserve(Ref).

-spec status() -> map().
status() ->
    heja_api:status().

%% Internal -------------------------------------------------------------------

to_bin(IoData) ->
    iolist_to_binary(IoData).
