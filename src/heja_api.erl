%% Description: heja_api
%%

-module(heja_api).

-behaviour(gen_server).

%% Management API
-export([start_link/0]).

%% API
-export([new/3, del/1,
         add_handler/4, del_handler/3, get_handler/3, get_ref/2,
         serve/2, deserve/1, status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%% Management API -------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% API ------------------------------------------------------------------------

new(Name, Version, Opts) ->
    gen_server:call(?MODULE, {new, Name, Version, Opts}).

del(Ref) ->
    gen_server:call(?MODULE, {del, Ref}).

status() ->
    gen_server:call(?MODULE, status).

add_handler(Ref, Method, Path, Fun) ->
    gen_server:call(?MODULE, {add_handler, Ref, Method, Path, Fun}).

del_handler(Ref, Method, Path) ->
    gen_server:call(?MODULE, {del_handler, Ref, Method, Path}).

get_handler(Ref, Method, Path) ->
    gen_server:call(?MODULE, {get_handler, Ref, Method, Path}).

get_ref(Name, Version) ->
    gen_server:call(?MODULE, {get_ref, Name, Version}).

serve(Ref, Port) ->
    gen_server:call(?MODULE, {serve, Ref, Port}).

deserve(Ref) ->
    gen_server:call(?MODULE, {deserve, Ref}).

%% gen_server callbacks -------------------------------------------------------

init([]) ->
    process_flag(trap_exit, true),
    {ok, #{}}.

handle_call({new, Name, Version, Opts}, _, State) ->
    case api_exist(Name, Version, State) of
        true ->
            {reply, {error, api_exist}, State};
        false ->
            Ref = make_ref(),
            Api = #{name=>Name,
                    version=>Version,
                    options=>Opts,
                    dispatch_list=>[],
                    router=>heja_router:new()},
            {reply, {ok, Ref}, State#{Ref=>Api}}
    end;
handle_call({del, Ref}, _, State) ->
    case maps:find(Ref, State) of
        {ok, #{server_pid:=Pid}} ->
            catch elli:stop(Pid);
        _ ->
            ok
    end,
    {reply, ok, maps:remove(Ref, State)};
handle_call({add_handler, Ref, Method, Path, Fun}, _, State) ->
    case maps:find(Ref, State) of
        {ok, #{dispatch_list:=DL} = Api} ->
            NewDL = lists:ukeysort(1, [{{Method, Path}, Fun}|DL]),
            NewRouter = heja_router:from_list(NewDL),
            {reply, ok, State#{Ref:=Api#{router:=NewRouter,
                                         dispatch_list:=NewDL}}};
        error ->
            {reply, {error, not_found}, State}
    end;
handle_call({del_handler, Ref, Method, Path}, _, State) ->
    case maps:find(Ref, State) of
        {ok, #{dispatch_list:=DL} = Api} ->
            NewDL = lists:ukeysort(1, lists:keydelete({Method, Path}, 1, DL)),
            NewRouter = heja_router:from_list(NewDL),
            {reply, ok, State#{Ref:=Api#{router:=NewRouter,
                                         dispatch_list:=NewDL}}};
        error ->
            {reply, {error, not_found}, State}
    end;
handle_call({get_handler, Ref, Method, Path}, _, State) ->
    case maps:find(Ref, State) of
        {ok, #{router:=Router}} ->
            case heja_router:get(Path, Method, Router) of
                {ok, Fun, Bindings} ->
                    {reply, {ok, Fun, Bindings}, State};
                {error, Error} ->
                    {reply, {error, Error}, State}
            end;
        error ->
            {reply, {error, not_found}, State}
    end;
handle_call({get_ref, Name, Version}, _, State) ->
    {reply, get_ref(Name, Version, State), State};
handle_call({serve, Ref, Port}, _, State) ->
    case maps:find(Ref, State) of
        {ok, #{server_port:=Port}} ->
            {reply, {error, already_serving_on_port}, State};
        {ok, Api} ->
            Options = [{callback, heja_dispatch},
                       {callback_args, [Ref]},
                       {port, Port}],
            {ok, Pid} = elli:start_link(Options),
            {reply, ok, State#{Ref:=Api#{server_pid=>Pid, server_port=>Port}}};
        error ->
            {reply, {error, not_found}, State}
    end;
handle_call({deserve, Ref}, _, State) ->
    case maps:find(Ref, State) of
        {ok, #{server_pid:=Pid} = Api} ->
            elli:stop(Pid),
            NewApi = maps:without([server_pid, server_port], Api),
            {reply, ok, State#{Ref:=NewApi}};
        {ok, _} ->
            {reply, ok, State};
        error ->
            {reply, ok, State}
    end;
handle_call(status, _, State) ->
    F = fun(_, Api, M) ->
            #{name:=N, version:=V, dispatch_list:=DL} = Api,
            Port = maps:get(server_port, Api, undefined),
            Status = case maps:get(server_pid, Api, undefined) of
                         undefined ->
                             not_running;
                         Pid ->
                            case is_process_alive(Pid) of
                                true ->
                                    {running, Port};
                                false ->
                                    not_running
                            end
                  end,
            NewDL = lists:map(fun({{Me, Pt}, _}) -> {Me, Pt} end, DL),
            M#{{N, V}=>#{dispatch=>NewDL, status=>Status}}
        end,
    {reply, maps:fold(F, #{}, State), State};
handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) ->
    io:format("EXIT (~p): ~p~n", [Pid, Reason]),
    {noreply, State}.

%% Internal -------------------------------------------------------------------

api_exist(Name, Version, State) ->
    element(1, get_ref(Name, Version, State)) == ok.

get_ref(Name, Version, State) ->
    F = fun(_, #{name:=N, version:=V}) ->
                N == Name andalso V == Version;
           (_, _) ->
                false
        end,
    case maps:keys(maps:filter(F, State)) of
        [] ->
            {error, not_found};
        [Ref] ->
            {ok, Ref}
    end.
