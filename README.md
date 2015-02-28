# jarvis
An IRC bot written in Erlang

This isn't what one would exactly call "polished" yet, so for now, this is how to use jarvis:

1. Change into whatever user is going to run the bot
2. Place yourself in that users homedir
3. mkdir .jarvis.d
4. cd .jarvis.d
5. Create a file named host, the contents of which should be `{host, "an IRC server address here"}.`,
    e.g. `{host, "irc.freenode.net"}.`
6. Create a file named channels, with contents `{channels, ["channel1", "channel2", "..."]}.`
    i.e. enumerate the channels jarvis should connect to. **NOTE:** there should NOT be a hashmark in front of the channel name
7. Create a file named nick, with contents `{nick, "the nick you want your bot to have"}.`
8. Start an erl shell (I'd recommend doing it inside a screen/tmux session):
    `screen -S bot erl`
9. Compile jarvis (`c(jarvis).`)
10. Start jarvis (`jarvis:start().`)
