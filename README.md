# HyperNerd

[![Build Status](https://travis-ci.org/tsoding/HyperNerd.svg?branch=master)](https://travis-ci.org/tsoding/HyperNerd)
[![Good For Stream](https://img.shields.io/github/issues/tsoding/HyperNerd/good%20for%20stream.svg)](https://github.com/tsoding/hypernerd/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+for+stream%22)

![HyperNerd](https://i.imgur.com/07Ymbi6.png)

Second iteration of [Tsoder][tsoder]. Chat bot for [Tsoding][tsoding] streams.

## Quick Start

Native dependencies:
- OpenSSL
- zlib

```console
$ nix-shell
$ stack build
$ stack exec hlint .
$ stack exec HyperNerd secret.ini
```

### Example of a secret.ini file

```ini
[User]
nick = HyperNerd
password = 12345
channel = Tsoding
clientId = <client-id-token>
```

## Support

You can support my work via

- Twitch channel: https://www.twitch.tv/subs/tsoding
- Patreon: https://www.patreon.com/tsoding

[tsoder]: http://github.com/tsoding/tsoder
[tsoding]: https://www.twitch.tv/tsoding
