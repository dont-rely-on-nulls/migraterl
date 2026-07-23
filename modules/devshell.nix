# Developer surface: the treefmt config (nix fmt +
# the formatting flake check) an the devenv shell.
{ inputs, ... }:
{
  perSystem =
    {
      pkgs,
      config,
      chryso,
      ...
    }:
    let
      # Name comes from the relx tuple in rebar.config (see modules/project.nix).
      app_name = (import ./project.nix { inherit (pkgs) lib; }).pname;
    in
    {
      # nix fmt + nix flake check (auto-wired by flakeModule)
      treefmt = {
        projectRootFile = "flake.nix";
        programs.erlfmt.enable = true;
        programs.nixfmt.enable = true;
      };

      devenv.shells.ci = {
        packages = with pkgs; [
          erlang
          just
          libpq
          rebar3
        ];
      };

      devenv.shells.default = {
        devenv.root =
          let
            r = builtins.getEnv "PWD";
          in
          if r != "" then r else builtins.toString ../.;

        packages = with pkgs; [
          erlfmt
          erlang-language-platform
          just
        ];

        env = {
          # https://www.erlang.org/doc/man/kernel_app.html
          ERL_AFLAGS = "-kernel shell_history enabled";
        };

        languages.erlang = {
          enable = true;
        };

        # The full unit + integration suite. Requires the postgres service to
        # be running first (CI starts it with `devenv up --detached`); the
        # flake-compat `devenv test` does not bring services up on its own.
        enterTest = ''
          rebar3 eunit
          rebar3 ct
        '';

        scripts = {
          db.exec = "just db";
          build.exec = "rebar3 compile";
          shell.exec = "rebar3 shell";
          test.exec = "just t";
          publish.exec = "just publish";
        };

        services.postgres = {
          enable = true;
          package = pkgs.postgresql_19;
          extensions = ext: [
          ];
          initdbArgs = [
            "--locale=C"
            "--encoding=UTF8"
          ];
          initialDatabases = [
            {
              name = app_name;
              user = app_name;
              pass = app_name;
            }
          ];
          port = 5432;
          listen_addresses = "127.0.0.1";
          initialScript = ''
            CREATE EXTENSION IF NOT EXISTS pg_stat_statements;
            -- The app user
            ALTER USER ${app_name} SUPERUSER CREATEROLE;
          '';

          settings = {
            shared_preload_libraries = pkgs.lib.concatStringsSep "," [
              "auto_explain"
              "pg_stat_statements"
            ];
            session_preload_libraries = "auto_explain";
            max_connections = 100;
            "auto_explain.log_min_duration" = 150;
            "auto_explain.log_analyze" = true;
            log_min_duration_statement = 0;
            log_statement = "all";
            log_directory = "log";
            log_filename = "postgresql-%Y-%m-%d.log";
            # pg_stat_statements config, nested attr sets need to be
            # converted to strings, otherwise postgresql.conf fails
            # to be generated.
            compute_query_id = "on";
            "pg_stat_statements.max" = 10000;
            "pg_stat_statements.track" = "all";
          }
          // pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
            # Async IO, io_uring or workers
            # For io_uring method (Linux only, requires liburing)
            io_method = "io_uring";
          }
          // pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin {
            # in case "io_uring" is not available
            io_method = "worker";
            # For systems with many CPU cores and high I/O latency
            io_workers = 8;
            # For smaller systems or fast local storage
            # io_workers = 2;
          };

        };

        enterShell = ''
          echo "Starting development environment..."
        '';
      };
    };
}
