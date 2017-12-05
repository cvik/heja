%%  Description: heja_sup
%%

-module(heja_sup).

-behaviour(supervisor).

%% Management Api
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% Management Api -------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_arg).

%% supervisor callbacks -------------------------------------------------------

init(no_arg) ->
    MetaServer = child(heja_api, worker, []),
    Strategy = {one_for_one, 100, 1},
    {ok, {Strategy, [MetaServer]}}.

%% Internal -------------------------------------------------------------------

child(Name, Type, Args) ->
    {Name, {Name, start_link, Args}, permanent, 3000, Type, [Name]}.
