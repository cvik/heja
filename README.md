## Heja - HTTP Erlang JSON APIs
**Warning** Heja is brand new and is not yet stable enough for any serious use.

Heja is a library for easy creation of HTTP/JSON-APIs, with as little boilerplate
as possible. The user have no exposure to either HTTP or JSON, but are instead
required to implement their handlers according to a strict spec. The library can
manage multiple APIs at the same time and can dynamically add and remove dispatch
rules and handlers.


Heja will only ever accept bodies with `Content-Type: application/json` and will
always return JSON, even for errors or calls that are expected to return no value
(like DELETE requests). Heja will further never return any other codes than 200
for success, 400 for client errors and 500 for server errors. The user is expected
to encode what kind of error has happened in the response body.

The handlers are erlang `fun`s and must follow this spec:
```erlang
-type json()       :: json_map()|json_array().
-type json_map()   :: #{key():=val()}.
-type json_array() :: [val()].
-type key()        :: atom()|string()|binary().
-type val()        :: true|false|null|atom()|binary()|integer()|float()|json().

-spec fn(Body::json(), Context::#{atom():=binary()}) ->
		{ok, json_map()} | {ok, {text, binary()}} | {error, atom()}.
```

The above JSON spec is not exhaustive. For more details, see
[lejson](https://github.com/cvik/lejson).

**Note** that heja requires the returned value to be a `json_map()`. Arrays
are not allowed as a top level return value.

All query values and path-variables are collected into the `Context::map()`
argument. The `Body::map()` is the decoded JSON value from `PUT` and `POST`
requests. It will always be the empty map for `GET` and `DELETE` requests.

### Abstractions
 - API - a collection of dispatch rules and handlers that run on a specific port
 - Handler - An erlang fun, bound to a path and method

### Example usage

```erlang
1> heja:start().
{ok, [heja]}

2> {ok, Ref} = heja:new(my_api).
{ok, #Ref<0.2100316181.119799809.110578>}

3> heja:get(Ref, "/api/v1/users/:id", fun(_Body, #{id:=Id}) -> {ok, #{user=>#{id=>Id}}} end).
ok

4> heja:serve(8080).
ok
```

Then try:
```bash
$ curl localhost:8080/api/v1/users/25 && echo
{"user": {"id":"25"}}
$
```

### Todo
 - [x] Need to be able to remove endpoints
 - [x] Add query-map to context (handle conflicts)
 - [x] should be able to stop API (via ref)
 - [x] Should be able to get api dispatch list
 - [x] Need to only accept and always return `Content-Type: application/json`
 - [x] Need to return proper errors for:
   - [x] 400 (bad request)
   - [x] 404 (not found),
   - [x] 405 (method not allowed)
   - [x] 406 (not acceptable)
 - [x] Should be able to get api status
 - [x] Should handle multiple tries to serve on same port
 - [x] Better handle of handler-funs return types
 - [x] Support returning raw text: `{ok {text, binary()}}`
 - [x] Serve should not start a new server if one is already running on that port
 - [x] New/x should not add an api, if one with the same name and version exists
 - [x] Should handle iodata paths
 - Documentation:
   - [ ] Function descriptions in the facade
   - [ ] More README.md examples
   - [ ] Better description of intent and purpose of the library
 - Error handling:
   - [ ] Verfify which error check should be handled first, 405 or 406.
   - [ ] Catch json decode errors -> 400
   - [ ] Catch json encode errors -> 500 (it's a `handler_failure` of sorts)
   - [ ] Catch server crashes and restart (linked to the api-manager)
 - Features:
   - [ ] Context keys should always be atoms
   - [ ] Bindings should handle constraints (int|func|non\_empty)
   - [ ] Option to include stacktrace in crash response (500s)
 - Testing:
   - [ ] Test the router
   - [ ] Test the dispatcher
   - [ ] test the api-manager (maybe skip the serve/deserve code paths)

### License

Apache license version 2.0. See the LICENSE file for details.
