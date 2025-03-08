set export := true
set dotenv-load := true

# Application

dbuser := "admin"
dbname := "migraterl"
database := justfile_directory() + "/migrations"
src := justfile_directory() + "/src"
test := justfile_directory() + "/test"

# Lists all availiable targets
default:
    just --list

# ---------
# Database
# ---------

# Login into the local Database
db:
    psql -U {{ dbuser }} {{ dbname }}

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
    rebar3 eunit
    rebar3 ct

# --------
# Releases
# --------

# Create a prod release of all apps
release:
    rebar3 as prod release

# Create a prod release (for nix) of the server
release-test:
    rebar3 as test release
