-module(wrapefs).

-export([run/0, run/1]).

run() ->
  Dir ="~/.emacs.d/mmccs",
  FileList = filelib:wildcard("*.el", Dir),
  R = run(FileList),
  write("features_load.el", R).

run(L) ->
  wrap(L, [load, require], []).

wrap([], _Terms, R) ->
  R;
wrap([H|T], Terms = [load, require], R) ->
  {F, _} = string:take(H, "." , true, leading),
  S = lists:concat(["(", atom_to_list(load), " \"", H, "\" (", atom_to_list(require), " '", F, "))"]),
  wrap(T, Terms, [S|R]).

write(File, List) ->
  file:write_file(File, to_binary(List)).

to_binary(L) ->
  to_binary(L, <<>>).

to_binary([], BinaryL) ->
  [<<"[">>, BinaryL, <<"]">>];
to_binary([H|[]], BinaryL) ->
  to_binary([], [list_to_binary(H)|BinaryL]);
to_binary([H|T], BinaryL) ->
  to_binary(T, [<<",">>, list_to_binary(H)|BinaryL]).
