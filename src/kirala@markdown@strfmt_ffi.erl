-module(kirala@markdown@strfmt_ffi).
-compile(export_all).

text(Strs) ->
    gleam@string:join(Strs, <<"">>).

str(X) when is_binary(X) -> X;
str(X) when is_list(X) ->
    Content = gleam@string:join( lists:map(fun str/1, X), <<", ">>),
    <<"["/utf8,Content/utf8,"]"/utf8>>;
str(X) when is_tuple(X) ->
    Content = gleam@string:join( lists:map(fun str/1, tuple_to_list(X)),<<", ">>),
    <<"#("/utf8,Content/utf8,")"/utf8>>;
str(X) when is_integer(X) -> integer_to_binary(X);
str(X) when is_float(X) -> float_to_binary(X);
str(X) when is_atom(X) -> atom_to_binary(X, utf8).
