%% Description: heja_router
%%

-module(heja_router).

%% API
-export([new/0, from_list/1, add/4, get/3]).

%% Types
-type router() :: map().
-type path() :: binary()|[binary()].
-type method() :: atom().
-type handler() :: function().
-type dispatch_list() :: [{path(), method(), handler()}].
-type router_error() :: no_handler_found | method_not_supported.

%% API ------------------------------------------------------------------------

-spec new() -> router().
new() -> #{}.

-spec from_list(dispatch_list()) -> router().
from_list(Ls) ->
    from_list(Ls, new()).

-spec add(path(), method(), function(), router()) -> router().
add(Path, Method, Fun, Tree) ->
    try
        Parts = binary:split(Path, <<$/>>, [global, trim_all]),
        add_handler(Parts, Method, Fun, Tree)
    catch
        throw:Error -> Error
    end.

-spec get(path(), method(), router()) ->
    {ok, Handler::function(), Bindings::map()} | {error, Error::router_error()}.
get(Path, Method, Tree) when is_binary(Path) ->
    Parts = binary:split(Path, <<$/>>, [global, trim_all]),
    get_handler(Parts, Method, Tree, #{});
get(Path, Method, Tree) when is_list(Path) ->
    get_handler(Path, Method, Tree, #{}).

%% Internal -------------------------------------------------------------------

from_list([{{Method, Path}, Fun}|Endpoints], Tree) ->
    from_list(Endpoints, add(Path, Method, Fun, Tree));
from_list([], Tree) -> Tree.

add_handler([<<$:, Binding/binary>>|Path], Method, Fun, Tree) ->
    case maps:find(binding, Tree) of
        {ok, {Binding, SubTree}} when is_map(SubTree) ->
            Tree#{binding:={Binding, add_handler(Path, Method, Fun, SubTree)}};
        {ok, {Name, _}} ->
            throw({error, {mismatching_binding, Name, Binding}});
        error ->
            Tree#{binding=>{Binding, add_handler(Path, Method, Fun, #{})}}
    end;
add_handler([Part|Path], Method, Fun, Tree) ->
    case maps:find(Part, Tree) of
        {ok, {#{handler:=FunMap}, SubTree}} when is_map(SubTree) ->
            Tree#{Part:={#{handler=>FunMap#{Method=>Fun}},
                         add_handler(Path, Method, Fun, SubTree)}};
        {ok, SubTree} when is_map(SubTree) ->
            Tree#{Part:=add_handler(Path, Method, Fun, SubTree)};
        error ->
            Tree#{Part=>add_handler(Path, Method, Fun, #{})}
    end;
add_handler([], Method, Fun, #{handler:=Existing} = Tree) ->
    Tree#{handler=>maps:merge(Existing, #{Method=>Fun})};
add_handler([], Method, Fun, Tree) ->
    Tree#{handler=>#{Method=>Fun}}.

get_handler([], Method, #{handler:=FunMap}, Bindings) ->
    case maps:find(Method, FunMap) of
        {ok, Fun} ->
            {ok, Fun, Bindings};
        error ->
            {error, method_not_supported}
    end;
get_handler([], _, _, _) ->
    {error, not_found};
get_handler([Part|Path], Method, Tree, Bindings) ->
    case maps:find(Part, Tree) of
        {ok, SubTree} when is_map(SubTree) ->
            get_handler(Path, Method, SubTree, Bindings);
        error ->
            case maps:find(binding, Tree) of
                {ok, {Name, SubTree}} when is_map(SubTree) ->
                    get_handler(Path, Method, SubTree, Bindings#{Name=>Part});
                error ->
                    {error, not_found}
            end
    end.
