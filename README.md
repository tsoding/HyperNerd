# HyperNerd

[![Build Status](https://travis-ci.org/tsoding/HyperNerd.svg?branch=master)](https://travis-ci.org/tsoding/HyperNerd)
[![Good For Stream](https://img.shields.io/github/issues/tsoding/HyperNerd/good%20for%20stream.svg)](https://github.com/tsoding/hypernerd/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+for+stream%22)

![HyperNerd](https://i.imgur.com/07Ymbi6.png)

Second iteration of [Tsoder][tsoder]. Chat bot for [Tsoding][tsoding] streams.

## Quick Start

### NixOS

```console
$ nix-shell
$ cabal configure
$ cabal build
$ cabal test
$ cabal exec hlint .
$ cabal run secret.ini database.db
```

### Stack

Native dependencies:
- OpenSSL
- zlib

```console
$ stack build
$ stack exec hlint .
$ stack exec HyperNerd secret.ini database.db
```

### Example of a secret.ini file

```ini
[User]
nick = HyperNerd
password = 12345
channel = Tsoding
clientId = <client-id-token>
```

## Command Aliases

You can assign a command alias to any command:

```
<user> !test
<bot> test
<user> !addalias foo test
<user> !foo
<user> test
```

The aliases are "redirected" only one level deep meaning that transitive aliases are not supported:

```
<user> !addalias bar foo
<user> !bar
*nothing, because !bar is redirected to !foo, but further redirect from !foo to !test does not happen*
```

Motivation to not support transitive aliases is the following:
- They are not needed in most of the cases. Generally you just have a
  main command and a bunch of aliases to it.
- Support for transitive aliases requires to traverse and maintain a
  "tree" of aliases, which complicates the logic and degrades the
  performance.

## Support

You can support my work via

- Twitch channel: https://www.twitch.tv/subs/tsoding
- Patreon: https://www.patreon.com/tsoding

[tsoder]: http://github.com/tsoding/tsoder
[tsoding]: https://www.twitch.tv/tsoding
