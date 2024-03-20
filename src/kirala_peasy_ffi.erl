-module(kirala_peasy_ffi).
-compile(export_all).

start() ->
    inets:start(),
    ssl:start().

to_char_list(Str) when is_list(Str) ->
    Str;
to_char_list(Str) when is_binary(Str) ->
    unicode:characters_to_list(Str).

readline(Prompt) ->
    io:get_line(Prompt).

show(X) when is_integer(X) ->
    integer_to_binary(X);
show(X) when is_float(X) ->
    float_to_binary(X);
show(X) when is_atom(X) ->
    atom_to_binary(X);
show(X) when is_list(X) ->
    gleam@string:join( lists:map(fun show/1, X), <<" ">> );
show(X) when is_tuple(X) ->
    gleam@string:join( lists:map(fun show/1, tuple_to_list(X)), <<" ">> );
show(X) when is_binary(X) ->
    X.


println(Any) ->
    io:format("~ts~n", [show(Any)]).

print(Any) ->
    io:format("~ts", [show(Any)]).

read_file(Filepath) ->
    file:read_file(Filepath).

read_text_file(Filepath) ->
    read_file(Filepath).

http_get(Url,Headers) ->
    {ok, {ResStatus, ResHeaders, ResBody}} = httpc:request(get,{Url, Headers}),
    {ResStatus, ResHeaders, ResBody}.

now() ->
    timestamp( erlang:now() ).

timestamp({Mega, Secs, Micro}) ->
    (Mega*1000*1000*1000*1000 + Secs * 1000 * 1000 + Micro) / 1000000.0.

get_env(Key,DefaultValue) ->
    os:getenv(Key,to_char_list(DefaultValue)).

get_env_int(Key,DefaultValue) ->
    os:getenv(Key,integer_to_list(DefaultValue)).