%% -*- erlang -*-
{application, tor,
 [{description, "Support library for Tor activities"},
  {vsn, "1.0"},
  {modules, [tor_socks_tcp]}
  {registered, []}
  {applications, [kernel, stdlib]}]}.
