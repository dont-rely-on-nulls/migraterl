# migraterl

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

A simple forward-only migrations library for Erlang, inspired by [DbUp](https://dbup.readthedocs.io/en/latest/).

## Status

[![Pipeline](https://github.com/dont-rely-on-nulls/migraterl/actions/workflows/pipelines.yml/badge.svg)](https://github.com/dont-rely-on-nulls/migraterl/actions/workflows/pipelines.yml)

### Builds

### Integrations

### Usage

Be aware that this project is a WIP, the API might change.

Assuming you plan to copy your migrations via overlays, like:

```
   {overlay, [
       {mkdir, "some_dir/migrations"},
       {copy, "some_dir/migrations/*", "\{\{output_dir\}\}/some_dir/migrations/"}
   ]}
```

Then you want to first fetch the directory from the release itself:

```erlang
    % (...)
    % Asumming 'Module' is an atom pointing to your Module's name,
    % i.e. ?MODULE
    Dir = code:lib_dir(Module),
    % Or even something like this...
    {ok, AnotherDir} = file:get_cwd(),
    % Then append the actual migration path into the release's directory
    PathSuffix = ["some_dir", "migrations"],
    Path = filename:join([Dir | PathSuffix]),

    % Make sure to also have a connection setup, we only support epgsql for now,
    % the library comes with a default connection (for testing purposes only).
    Conn = migraterl:default_connection(),

    % You can Enable/Disable certain options
    Options = #{repeatable => false},
    
    % Now we can properly setup these migrations as part of our application
    % bootstrap process...
    ok = migraterl:migrate(Conn, Path, Options),

    % (...)
```

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

### Testing

```shell
# You can either run rebar directly
rebar3 ct
# or
just t
```

## Inspiration

- [DbUp](https://dbup.readthedocs.io/en/latest/)
