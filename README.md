# Support library for Tor activities

## Files

<dl>
  <dt>./src/tor_socks_tcp.erl</dt>
  <dd>A simple plugin replacement for the gen_tcp module which communicates via a SOCKS5 proxy server. The pki server uses SOCKS5 as an option.</dd>
</dl>

## Unit testing

```
$ ../obscrete/bin/unit_test --config ../obscrete/etc/obscrete-no-players.conf tor_socks_tcp
```
