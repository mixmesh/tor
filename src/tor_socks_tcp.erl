-module(tor_socks_tcp).
-export([connect/3, connect/4]).
-export([close/1]).
-export([send/2, recv/2, recv/3]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/shorthand.hrl").

-define(TOR_SOCKS_ADDRESS, {127, 0, 0, 1}).
-define(TOR_SOCKS_PORT, 9050).

-define(SOCKS5_VERSION, 16#05).

-define(NO_AUTHENTICATION, 16#0).
-define(NO_ACCEPTABLE_AUTH_METHODS, 16#FF).

-define(TCP_IP_STREAM_CONNECTION, 16#01).

-define(RESERVED, 16#00).

-define(IPV4_ADDRESS, 16#01).
-define(DOMAIN_NAME, 16#03).
-define(IPV6_ADDRESS, 16#04).

-define(REQUEST_GRANTED, 16#00).
-define(GENERAL_FAILURE, 16#01).
-define(CONNECTION_NOT_ALLOWED_BY_RULESET, 16#02).
-define(NETWORK_UNREACHABLE, 16#03).
-define(HOST_UNREACHABLE, 16#04).
-define(CONNECTION_REFUSED_BY_DESTINATION_HOST, 16#05).
-define(TTL_EXPIRED, 16#06).
-define(COMMAND_NOT_SUPPORTED_OR_PROTOCOL_ERROR, 16#07).
-define(ADDRESS_TYPE_NOT_SUPPORTED, 16#08).

%% Exported: connect

connect(DomainName, Port, Options) ->
    connect(DomainName, Port, Options, infinity).

connect(DomainName, Port, Options, Timeout) ->
    case lists:keysearch(active, 1, Options) of
        false ->
            {error, must_not_be_active};
        {value, {_, true}} ->
            {error, must_not_be_active};
        {value, {_, false}} ->
            case gen_tcp:connect(?TOR_SOCKS_ADDRESS, ?TOR_SOCKS_PORT,
                                 Options) of
                {ok, Socket} ->
                    try
                        ok = client_greeting(Socket),
                        ok = client_authentication_request(Socket, Timeout),
                        ok = client_connection_request(Socket, Timeout,
                                                       ?l2b(DomainName), Port),
                        socks_tunnel(Socket)
                    catch
                        throw:{?MODULE, Reason} ->
                            gen_tcp:close(Socket),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

client_greeting(Socket) ->
  connect_send(Socket, <<?SOCKS5_VERSION, 1, ?NO_AUTHENTICATION>>).

connect_send(Socket, Packet) ->
    case gen_tcp:send(Socket, Packet) of
        ok ->
            ok;
        {error, Reason} ->
            throw({?MODULE, {posix, Reason}})
    end.

client_authentication_request(Socket, Timeout)  ->
    case connect_recv(Socket, 2, Timeout) of
        <<?SOCKS5_VERSION, ?NO_AUTHENTICATION>> ->
            ok;
        <<?SOCKS5_VERSION, ?NO_ACCEPTABLE_AUTH_METHODS>> ->
            throw({?MODULE, {authentication, no_acceptable_auth_methods}})
    end.

connect_recv(Socket, Length, Timeout) ->
    case gen_tcp:recv(Socket, Length, Timeout) of
        {ok, Packet} ->
            Packet;
        {error, Reason} ->
            throw({?MODULE, {posix, Reason}})
    end.

client_connection_request(Socket, Timeout, DomainName, Port) ->
    DomainNameSize = size(DomainName),
    ok = connect_send(Socket,
                      ?l2b([<<?SOCKS5_VERSION,
                              ?TCP_IP_STREAM_CONNECTION,
                              ?RESERVED,
                              ?DOMAIN_NAME,
                              DomainNameSize>>,
                            DomainName,
                            <<Port:16>>])),
    <<?SOCKS5_VERSION, Status, ?RESERVED, AddressType>> =
        connect_recv(Socket, 4, Timeout),
    case AddressType of
        ?IPV4_ADDRESS ->
            _DestAddress = connect_recv(Socket, 4, Timeout);
        ?DOMAIN_NAME ->
            <<Length>> = connect_recv(Socket, 1, Timeout),
            _DestAddress = connect_recv(Socket, Length, Timeout);
        ?IPV6_ADDRESS ->
            _DestAddress = connect_recv(Socket, 2, Timeout)
    end,
    _DestPort = connect_recv(Socket, 2, Timeout),
    case Status of
        ?REQUEST_GRANTED ->
            ok;
        ?GENERAL_FAILURE ->
            throw({?MODULE, {connection, general_failure}});
        ?CONNECTION_NOT_ALLOWED_BY_RULESET ->
            throw({?MODULE, {connection, connection_not_allowed_by_ruleset}});
        ?NETWORK_UNREACHABLE ->
            throw({?MODULE, {connection, network_unreachable}});
        ?HOST_UNREACHABLE ->
            throw({?MODULE, {connection, host_unreachable}});
        ?CONNECTION_REFUSED_BY_DESTINATION_HOST ->
            throw({?MODULE, {connection, connection_refused_by_destination_host}});
        ?TTL_EXPIRED ->
            throw({?MODULE, {connection, ttl_expired}});
        ?COMMAND_NOT_SUPPORTED_OR_PROTOCOL_ERROR ->
            throw({?MODULE, {connection, command_not_supported_or_protocol_error}});
        ?ADDRESS_TYPE_NOT_SUPPORTED ->
            throw({?MODULE, {connection, address_type_not_supported}})
    end.

socks_tunnel(Socket) ->
    ?spawn_server(fun(Parent) -> {ok, {Parent, Socket}} end,
                  fun message_handler/1).

message_handler({Parent, Socket}) ->
    receive
        close ->
            gen_tcp:close(Socket),
            stop;
        {call, From, {send, Packet}} ->
            {reply, From, gen_tcp:send(Socket, Packet)};
        {call, From, {recv, Length, Timeout}} ->
            {reply, From, gen_tcp:recv(Socket, Length, Timeout)};
        {'EXIT', Parent, Reason} ->
            gen_tcp:close(Socket),
            exit(Reason);
        UnknownMessage ->
            ?error_log({unknown_message, UnknownMessage}),
            noreply
    end.

%% Exported: close

close(Pid)  ->
    Pid ! close,
    ok.

%% Exported: send

send(Pid, Packet) ->
    serv:call(Pid, {send, Packet}).

%% Exported: recv

recv(Pid, Length) ->
    serv:call(Pid, {recv, Length, infinity}).

recv(Pid, Length, Timeout) ->
    serv:call(Pid, {recv, Length, Timeout}).
