# Support library for Tor activities

## Files

<dl>
  <dt>./src/tor_socks_tcp.erl</dt>
  <dd>A simple plugin replacement for the gen_tcp module which communicates via a SOCKS5 proxy. The PKI server uses a SOCKS5 proxy as an option.</dd>
  <dt>./test/test_tor_socks_tcp.erl</dt>
  <dd>Test for the tor_socks_tcp module</dd>
</dl>

## Testing

`make runtest` runs all tests, i.e.

`$ ../obscrete/bin/run_test --config ../obscrete/etc/obscrete-do-nothing.conf test/`

Tests can be run individually as well:

`$ ../obscrete/bin/run_test --config ../obscrete/etc/obscrete-do-nothing.conf tor_socks_tcp`
