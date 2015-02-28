-module(cw).
-export([writeConfig/2]).
-export([read/1]).

directory() ->
    DirPath = "/tmp/jarvis",
    file:make_dir(DirPath),
    DirPath.

writeConfig(Filename, Data) ->
    FilePath = directory() ++ "/" ++ Filename,
    file:write_file(FilePath, io_lib:format("{~p, ~p}.~n", [Filename, Data])).

read(Filename) ->
    FilePath = directory() ++ "/" ++ Filename,
    case file:consult(FilePath) of
        {ok, [{Filename, Data}]} -> io:format("~p~n", [Data]);
        {error, Msg} -> io:format("~p~n", [Msg])
    end.
