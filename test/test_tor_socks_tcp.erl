-module(test_tor_socks_tcp).
-export([start/0]).

%% NOTE: Test uses a publically available echo server as described in
%%       https://docs.postman-echo.com/
%% NOTE: Tor must be running on the test machine, i.e. a SOCKS5 proxy listens
%%       on 127.0.0.1:9050

start() ->
    <<"HTTP/1.1 200", _/binary>> = ping(gen_tcp),
    <<"HTTP/1.1 200", _/binary>> = ping(tor_socks_tcp).

ping(M) ->
    {ok, Socket} =
        M:connect("postman-echo.com", 80, [{active, false}, {mode, binary}]),
    ok = M:send(Socket, <<"GET /get?foo=42&bar=baz HTTP/1.0\r\n\r\n">>),
    list_to_binary(read_until_close(M, Socket)).

read_until_close(M, Socket) ->
    case M:recv(Socket, 0) of
        {ok, Packet} ->
            [Packet|read_until_close(M, Socket)];
        {error, closed} ->
            []
    end.
