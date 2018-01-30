# HyperNerd

[![Build Status](https://travis-ci.org/tsoding/HyperNerd.svg?branch=master)](https://travis-ci.org/tsoding/HyperNerd)

![HyperNerd](https://i.imgur.com/07Ymbi6.png)

Second iteration of [Tsoder][tsoder]. Chat bot for [Tsoding][tsoding] streams.

## Quick Start

```console
$ nix-shell
$ stack build
$ stack exec HyperNerd secret.ini
```

### Example of a secret.ini file

```ini
[User]
nick = HyperNerd
password = 12345
channel = Tsoding
```

## Support

You can support my work via

- Twitch channel: https://www.twitch.tv/subs/tsoding
- Patreon: https://www.patreon.com/tsoding

[tsoder]: http://github.com/tsoding/tsoder
[tsoding]: https://www.twitch.tv/tsoding
