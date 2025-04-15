# migraterl

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

A simple forward-only migrations library for Erlang, inspired by [DbUp](https://dbup.readthedocs.io/en/latest/).

## Status

[![Pipeline](https://github.com/dont-rely-on-nulls/migraterl/actions/workflows/pipelines.yml/badge.svg)](https://github.com/dont-rely-on-nulls/migraterl/actions/workflows/pipelines.yml)

### Builds

### Integrations

## Development

We have [devenv](https://devenv.sh/) setup and everything is based on [Nix](https://nixos.org/), you can check our [flake.nix](https://github.com/dont-rely-on-nulls/migraterl/blob/master/flake.nix) to learn how it looks like.

```shell
nix develop --impure
# to spawn a postgres database
devenv up
```
there's also a [justfile](https://github.com/casey/just) to manage builds and tests.
```shell
# will show all commands supported
just
```

### Documentation

TODO

### Testing

```shell
# You can either run rebar directly
rebar3 ct
# or
just t
```

## Inspiration

- [DbUp](https://dbup.readthedocs.io/en/latest/)
