{
  description = "";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-utils = {
      url = "github:numtide/flake-utils/v1.0.0";
    };

    devenv = {
      url = "github:cachix/devenv";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs =
    {
      self,
      nixpkgs,
      devenv,
      flake-utils,
      treefmt-nix,
      ...
    }@inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
          };
        };

        treefmtEval = treefmt-nix.lib.evalModule pkgs ./treefmt.nix;

        # Environment-specific packages
        linuxPkgs = with pkgs; [
          inotify-tools
        ];

        darwinPkgs = with pkgs.darwin.apple_sdk.frameworks; [
          CoreFoundation
          CoreServices
        ];

        # Erlang
        erlang = pkgs.erlang;
        app_version = "0.1.0";
        erl_app = "migraterl";
        pg_admin_user = "admin";
        pg_admin_password = "postgres";

        mkEnvVars = pkgs: erl: {
          LOCALE_ARCHIVE = pkgs.lib.optionalString pkgs.stdenv.isLinux "${pkgs.glibcLocales}/lib/locale/locale-archive";
          LANG = "en_US.UTF-8";
          # https://www.erlang.org/doc/man/kernel_app.html
          ERL_AFLAGS = "-kernel shell_history enabled";
          # Devenv sets this to something else
          # https://www.postgresql.org/docs/7.0/libpq-envars.htm
          PGHOST = "127.0.0.1";
        };
      in
      {
        # nix build
        packages = {
          devenv-up = self.devShells.${system}.default.config.procfileScript;

          # Builds the erlang project
          # nix build
          default =
            let
              deps = import ./rebar-deps.nix {
                inherit (pkgs) fetchHex fetchFromGitHub fetchgit;
                builder = pkgs.beamPackages.buildRebar3;
              };
            in
            pkgs.beamPackages.rebar3Relx {
              pname = erl_app;
              version = app_version;
              src = pkgs.lib.cleanSource ./.;
              releaseType = "release";
              profile = "prod";
              include = [
                "rebar.config"
              ];
              beamDeps = builtins.attrValues deps;
            };
        };

        # nix run
        apps = {
        };

        devShells = {
          # `nix develop .#ci`
          # reduce the number of packages to the bare minimum needed for CI
          ci = pkgs.mkShell {
            env = mkEnvVars pkgs erlang;
            buildInputs = with pkgs; [
              erlang
              just
              rsync
            ];
          };

          # `nix develop`
          default = devenv.lib.mkShell {
            inherit inputs pkgs;
            modules = [
              (
                { pkgs, lib, ... }:
                {
                  packages =
                    with pkgs;
                    [
                      just
                      sqls
                    ]
                    ++ lib.optionals stdenv.isLinux (linuxPkgs)
                    ++ lib.optionals stdenv.isDarwin darwinPkgs;

                  languages.erlang = {
                    enable = true;
                    package = erlang;
                  };

                  env = mkEnvVars pkgs erlang;

                  scripts = {
                    db.exec = "just db";
                    db-up.exec = "just db-up";
                    db-down.exec = "just db-down";
                    db-reset.exec = "just db-reset";
                  };

                  enterShell = ''
                    echo "Starting Development Environment..."
                  '';

                  services.postgres = {
                    enable = true;
                    package = pkgs.postgresql_17;
                    extensions = ext: [
                      ext.periods
                    ];
                    initdbArgs = [
                      "--locale=C"
                      "--encoding=UTF8"
                    ];
                    settings = {
                      shared_preload_libraries = "pg_stat_statements";
                      # pg_stat_statements config, nested attr sets need to be
                      # converted to strings, otherwise postgresql.conf fails
                      # to be generated.
                      compute_query_id = "on";
                      "pg_stat_statements.max" = 10000;
                      "pg_stat_statements.track" = "all";
                    };
                    initialDatabases = [
                      { name = erl_app; }
                    ];
                    port = 5432;
                    listen_addresses = "127.0.0.1";
                    initialScript = ''
                      CREATE EXTENSION IF NOT EXISTS pg_stat_statements;
                      CREATE ROLE ${pg_admin_user} WITH LOGIN SUPERUSER CREATEROLE PASSWORD '${pg_admin_password}';
                      GRANT ALL PRIVILEGES ON DATABASE ${erl_app} to ${pg_admin_user};
                    '';
                  };
                }
              )
            ];
          };
        };

        # nix fmt
        formatter = treefmtEval.config.build.wrapper;
      }
    );
}
