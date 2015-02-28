-module(jarvis).
% bot functions
-export([start/0, init/0, loop/4, stop/0, reload/0]).
% IRC functions
-export([join/1, leave/1]).
-export([op/2, deop/2]).
-export([who/1, names/1]).
-export([msg/2]).

-define(TIMEOUT, 5000). % 5 seconds


% PUBLIC (EXPORTED) FUNCTIONS

start() ->
    register(?MODULE, spawn(?MODULE, init, [])).

init() ->
    Host = read(host),
    BotNick = read(nick),
    Sock = connect(Host, BotNick),
    Users = trustedUsers(),
    Checksum = os:cmd("md5sum jarvis.erl"),
    loop(Sock, BotNick, Users, Checksum).

stop() ->
    ?MODULE ! quit.

reload() ->
    ?MODULE ! reload.

join(Channel) ->
    ?MODULE ! {join, sanitizeChannel(Channel)}.

leave(Channel) ->
    ?MODULE ! {part, sanitizeChannel(Channel)}.

op(Channel, User) ->
    ?MODULE ! {op, sanitizeChannel(Channel), stringify(User)}.

deop(Channel, User) ->
    ?MODULE ! {deop, sanitizeChannel(Channel), stringify(User)}.

who(User) ->
    ?MODULE ! {who, stringify(User)}.

names(Channel) ->
    ?MODULE ! {names, sanitizeChannel(Channel)}.

msg(Target, Msg) ->
    ?MODULE ! {privmsg, Target, Msg}.


% HELPER FUNCTIONS

directory() ->
    {ok, [[Home]]} = init:get_argument(home),
    DirPath = Home ++ "/" ++ ".jarvis.d",
    file:make_dir(DirPath),
    DirPath.

read(Element) ->
    FilePath = directory() ++ "/" ++ Element,
    case file:consult(FilePath) of
        {ok, [{Element, Data}]} -> Data;
        {error, Msg} -> io:format("~p~n~n", [Msg])
    end.

writeConfig(Element, Data) ->
    FilePath = directory() ++ "/" ++ Element,
    file:write_file(FilePath, io_lib:format("{~p, ~p}.~n", [Element, Data])).

stringify(Data) ->
    case is_atom(Data) of
        true ->
            SData = atom_to_list(Data);
        _ -> SData = Data
    end,
    SData.

sanitizeChannel(Channel) ->
    SChannel = stringify(Channel),
    re:replace(SChannel, "(^#)", "", [global, {return, list}]).

formatData(Data) ->
    % strip away \r\n
    StrippedData = re:replace(Data, "(\\s+$)", "", [global, {return, list}]),
    % split on <space> and :
    string:tokens(StrippedData, ": ").

joinChannels(_, []) -> ok;
joinChannels(Sock, [Channel|Channels]) ->
    irc_join(Sock, Channel),
    joinChannels(Sock, Channels).

watchedChannels() ->
    read(channels).

trustedUsers() ->
    read(users).

connect(Host, BotNick) -> connect(Host, 6667, BotNick).
connect(Host, Port, BotNick) ->
    io:format("~n*** ~p CONNECTING ***~n", [BotNick]),
    {ok, Sock} = gen_tcp:connect(Host, Port, [{packet, line},{reuseaddr, true}]),
    irc_connect(Sock, Host, BotNick),
    Sock.

disconnect(Sock, BotNick) ->
    irc_disconnect(Sock, BotNick),
    gen_tcp:close(Sock).

shutdown(Sock, BotNick) ->
    io:format("~n*** ~p DISCONNECTING ***~n", [BotNick]),
    disconnect(Sock, BotNick),
    exit(stopped).

extractNick(Data) ->
    stringify(lists:nth(1, string:tokens(Data, "!"))).


% LOOP

loop(Sock, BotNick, Users, Checksum) ->
    receive
        {tcp, Sock, Data} ->
            io:format("~s", [Data]), % DEBUG
            TokenizedData = formatData(Data),
            evaluate(Sock, BotNick, Users, TokenizedData),
            loop(Sock, BotNick, Users, Checksum);

        quit ->
            shutdown(Sock, BotNick);

        reload ->
            NewChecksum = os:cmd("md5sum jarvis.erl"),
            case Checksum =:= NewChecksum  of
                true ->
                    loop(Sock, BotNick, Users, Checksum);
                _Else ->
                    io:format("~n*** JARVIS RELOADING ***~n"),
                    case compile:file(?MODULE) of
                        {ok, ?MODULE} ->
                            code:purge(?MODULE),
                            code:load_file(?MODULE),
                            ?MODULE:loop(Sock, BotNick, Users, NewChecksum);
                        _ ->
                            io:format("~n*** ERROR COMPILATION FAILED ***~n"),
                            loop(Sock, BotNick, Users, NewChecksum)
                    end
            end;

        {join, Channel} ->
            irc_join(Sock, Channel),
            loop(Sock, BotNick, Users, Checksum);

        {part, Channel} ->
            irc_part(Sock, Channel),
            loop(Sock, BotNick, Users, Checksum);

        {op, Channel, Nick} ->
            irc_op(Sock, sanitizeChannel(Channel), Nick),
            loop(Sock, BotNick, Users, Checksum);

        {deop, Channel, Nick} ->
            irc_deop(Sock, Channel, Nick),
            loop(Sock, BotNick, Users, Checksum);

        {who, Nick} ->
            irc_who(Sock, Nick),
            loop(Sock, BotNick, Users, Checksum);

        {names, Channel} ->
            irc_names(Sock, Channel),
            loop(Sock, BotNick, Users, Checksum);

        {privmsg, Target, Msg} ->
            irc_privmsg(Sock, Target, Msg),
            loop(Sock, BotNick, Users, Checksum);

        {config, write, users, NewUsers} ->
            writeConfig(users, NewUsers),
            loop(Sock, BotNick, NewUsers, Checksum);

        {config, list, To, Element} ->
            Data = read(Element),
            % This will surely fuck up if we pass it a string...
            lists:foreach(fun(E) -> irc_privmsg(Sock, To, E) end, Data),
            %irc_privmsg(Sock, To, Data),
            loop(Sock, BotNick, Users, Checksum)
    end.



% IRC COMMANDS

irc_connect(Sock, Host, BotNick) ->
    gen_tcp:send(Sock, "NICK " ++ BotNick ++ "\r\n"),
    gen_tcp:send(Sock, "USER " ++ BotNick ++ " localhost " ++ Host ++ " :padowis assistant\r\n").

irc_pong(Sock, Data) ->
    gen_tcp:send(Sock, "PONG " ++ Data ++ "\r\n").

irc_disconnect(Sock, BotNick) ->
    gen_tcp:send(Sock, "QUIT :" ++ BotNick ++ " shutdown by user\r\n").

irc_join(Sock, Channel) ->
    gen_tcp:send(Sock, "JOIN #" ++ Channel ++ "\r\n").

irc_part(Sock, Channel) ->
    gen_tcp:send(Sock, "PART #" ++ Channel ++ "\r\n").

irc_privmsg(Sock, To, Message) ->
    gen_tcp:send(Sock, "PRIVMSG " ++ To ++ " :" ++ Message ++ "\r\n").

irc_op(Sock, Channel, Nick) ->
    gen_tcp:send(Sock, "MODE #" ++ Channel ++ " +o " ++ Nick ++ "\r\n").

irc_deop(Sock, Channel, Nick) ->
    gen_tcp:send(Sock, "MODE #" ++ Channel ++ " -o " ++ Nick ++ "\r\n").

irc_who(Sock, Nick) ->
    gen_tcp:send(Sock, "WHO " ++ Nick ++ " o\r\n").

irc_names(Sock, Channel) ->
    gen_tcp:send(Sock, "NAMES #" ++ Channel ++ "\r\n").


% EVALUATORS

evaluate(Sock, _, _, ["PING"|Data]) ->
    irc_pong(Sock, Data);

evaluate(Sock, _, _, [_,"376"|_]) ->
    joinChannels(Sock, watchedChannels());

evaluate(Sock, BotNick, Users, [User, "PRIVMSG", Channel, BotNick]) ->
    Nick = extractNick(User),
    case lists:member(Nick, Users) of
        true -> irc_privmsg(Sock, Channel, "Yes sir?\r\n");
        _ -> irc_privmsg(Sock, Channel, Nick ++ ": can I help you?\r\n")
    end;

%% Auto-OP based on nicks... should probably be disabled
evaluate(Sock, _, Users, [User, "JOIN", Channel]) ->
    Nick = extractNick(User),
    case lists:member(Nick, Users) of
        true -> irc_op(Sock, sanitizeChannel(Channel), Nick);
        _ -> ok
    end;

evaluate(Sock, BotNick, _, [User, "MODE", Channel, Mode, BotNick]) ->
    Nick = extractNick(User),
    case Mode of
        "+o" ->
            irc_privmsg(Sock, Channel, "Thank you " ++ Nick ++ "!");
        _ -> ok
    end;

evaluate(Sock, BotNick, Users, [User, "PRIVMSG", Channel, BotNick, "!"|Rest]) ->
    Nick = extractNick(User),
    SChannel = sanitizeChannel(Channel),
    FailMsg = "I'm sorry, " ++ Nick ++ ". I'm afraid I can't do that.",
    irc_names(Sock, SChannel),
    receive
        {tcp, Sock, Data} ->
            io:format("~s", [Data]), % DEBUG
            [_, "353", _, _, Channel|Names] = formatData(Data),

            %% only opped people may execute commands
            case lists:member("@" ++ Nick, Names) of
                true ->
                    parseCommand(Sock, BotNick, Users, Channel, Nick, Rest);
                _ ->
                    irc_privmsg(Sock, Channel, FailMsg)
            end
    after
        ?TIMEOUT ->
            irc_privmsg(Sock, Channel, FailMsg)
    end;

evaluate(Sock, BotNick, _, [User, "PRIVMSG", Channel, BotNick, "?"|_Rest]) ->
    Nick = extractNick(User),
    irc_privmsg(Sock, Channel, "I'm sorry " ++ Nick ++ ", I don't understand your question.");

evaluate(_,_,_,_) -> ok.

parseCommand(Sock, BotNick, Users, Channel, Nick, CommandParams) ->
    SChannel = sanitizeChannel(Channel),
    case CommandParams of
        ["op", Target] ->
            irc_privmsg(Sock, Channel, "Certainly " ++ Nick),
            irc_op(Sock, SChannel, Target);

        ["deop", BotNick] ->
            irc_privmsg(Sock, Channel, "I'm sorry, " ++ Nick ++ ". I'm afraid I can't do that.");

        ["deop", Target] ->
            irc_privmsg(Sock, Channel, "I see someone has been naughty..."),
            irc_deop(Sock, SChannel, Target);

        ["reload"] ->
            irc_privmsg(Sock, Channel, "By your command"),
            self() ! reload;

        ["config", "distrust", BadNick] ->
            case lists:member(BadNick, Users) of
                true ->
                    %% remove user, write config
                    GoodUsers = lists:delete(BadNick, Users),
                    ?MODULE ! {config, write, users, GoodUsers};
                _ ->
                    ok
            end;

        ["config", "trust", NewNick] ->
            %% check if nick is already in trustedUsers
            case lists:member(NewNick, Users) of
                true ->
                    ok;
                _ ->
                    NewUsers = Users ++ [NewNick],
                    ?MODULE ! {config, write, users, NewUsers}
            end;

        ["config", "list", "channels"] ->
            ?MODULE ! {config, list, Channel, channels};

        ["config", "list", "users"] ->
            ?MODULE ! {config, list, Channel, users};

        _ ->
            irc_privmsg(Sock, Channel, "Unknown command: " ++ CommandParams)
    end.

