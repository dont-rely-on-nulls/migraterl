set export := true
set dotenv-load := true

# Application

dbuser := "migraterl"
dbname := "migraterl"
src_dir := justfile_directory() + "/src"
test_dir := justfile_directory() + "/test"

alias t := test

# Lists all availiable targets
default:
    just --list

# ---------
# Database
# ---------

# Login into the local Database
db:
    psql -h localhost -p 5432 -U {{ dbuser }} {{ dbname }}

# Bootstraps the local nix-based postgres server
pg:
    devenv up

# --------
# Erlang

# --------
build:
    rebar3 compile

# Fetches rebar3 dependencies, updates both the rebar and nix lockfiles
deps:
    rebar3 get-deps
    rebar3 nix lock

# Runs dializer on the erlang codebase
dialyzer:
    rebar3 dialyzer

# Runs ther erlang server (inside the rebar shell)
server: build
    rebar3 shell

# Runs unit tests in the server
test:
    rebar3 ct
    rebar3 cover

# --------
# Releases
# --------

# Create a prod release of all apps
release:
    rebar3 as prod release

# Create a prod release (for nix) of the server
release-test:
    rebar3 as test release
