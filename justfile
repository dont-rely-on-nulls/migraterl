set export := true
set dotenv-load := true

# Application

dbuser := "admin"
dbname := "migraterl"
database := justfile_directory() + "/migrations"
apps := justfile_directory() + "/apps"
server_port := "8080"

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
    rebar3 do eunit, ct

# Migrates the DB (up)
db-up:
    {{ database }}/migrate.sh -u

# Nukes the DB
db-down:
    {{ database }}/migrate.sh -d

# Populate DB
db-input:
    {{ database }}/migrate.sh -i

# Hard reset DB
db-reset: db-down db-up db-input

# --------
# Releases
# --------

# Create a prod release of all apps
release:
    rebar3 as prod release -n server

# Create a prod release (for nix) of the server
release-nix:
    rebar3 as prod tar

# ----------
# Deployment
# ----------

# Builds the deployment docker image with Nix
build-docker:
    nix build .#dockerImage
