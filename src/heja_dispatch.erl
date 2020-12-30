%% Description: heja_dispatch
%%

-module(heja_dispatch).

-behaviour(elli_handler).

%% elli_handler callbacks
-export([handle/2, handle_event/3]).

%% elli_handler callbacks -----------------------------------------------------

handle(Req, [Ref]) ->
    case valid_content_type(Req) of
        false ->
            {400, [json_header()],
                  lejson:encode(#{error=>content_type_not_acceptable})};
        true ->
            dispatch_to_handler(Req, Ref)
    end.

dispatch_to_handler(Req, Ref) ->
    Method = elli_request:method(Req),
    Path = elli_request:path(Req),
    Args = elli_request:get_args(Req),
    case heja_api:get_handler(Ref, method_to_lower(Method), Path) of
        {ok, Fun, Bindings} ->
            Body = decode_body(Req),
            Context = atomize_keys(maps:merge(maps:from_list(Args), Bindings)),
            case catch Fun(Body, Context) of
                {ok, Resp} when is_map(Resp) ->
                    {200, [json_header()], lejson:encode(Resp)};
                {ok, {text, Resp}} when is_binary(Resp) ->
                    {200, [text_header()], Resp};
                {error, Error} ->
                    {400, [json_header()], lejson:encode(#{error=>Error})};
                {'EXIT', {Error, _}} ->
                    {500, [json_header()],
                          lejson:encode(#{error=>handler_failure,
                                          details=>Error})};
                _ ->
                    {500, [json_header()],
                          lejson:encode(#{error=>handler_failure,
                                          details=>unsupported_return})}
            end;
        {error, method_not_supported} ->
            {400, [json_header()], lejson:encode(#{error=>method_not_allowed})};
        {error, not_found} ->
            {400, [json_header()], lejson:encode(#{error=>not_found})}
    end.

handle_event(_Event, _Data, _Args) ->
    ok.

method_to_lower('GET') -> get;
method_to_lower('POST') -> post;
method_to_lower('PUT') -> put;
method_to_lower('DELETE') -> delete;
method_to_lower(Other) -> Other.

decode_body(Req) ->
    case elli_request:body(Req) of
        <<>> ->
            #{};
        Bin ->
            lejson:decode(Bin)
        end.

json_header() ->
    {<<"Content-Type">>, <<"application/json">>}.

text_header() ->
    {<<"Content-Type">>, <<"text/plain">>}.

valid_content_type(Req) ->
    Type = proplists:get_value(<<"Content-Type">>, elli_request:headers(Req)),
    case elli_request:method(Req) of
        'POST' ->
            %% FIXME: Should check if Type is prefix
            is_json_type(Type);
        'PUT' ->
            is_json_type(Type);
        _ ->
            true
    end.

is_json_type(<<"application/json",_/binary>>) -> true;
is_json_type(_) -> false.

atomize_keys(Map) when is_map(Map) ->
    maps:fold(fun(K, V, M) ->M#{to_atom(K)=>atomize_keys(V)} end, #{}, Map);
atomize_keys(Values) when is_list(Values) ->
    [atomize_keys(V) ||  V <- Values];
atomize_keys(Value) ->
    Value.

to_atom(T) when is_binary(T) -> binary_to_atom(T, utf8);
to_atom(T) when is_list(T) -> list_to_atom(T);
to_atom(T) when is_atom(T) -> T.
