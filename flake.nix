{
  description = "Erlang + Nix + PG Environment";

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

        tooling =
          with pkgs;
          [ just ] ++ lib.optionals stdenv.isLinux linuxPkgs ++ lib.optionals stdenv.isDarwin darwinPkgs;

        # Erlang
        erlang = pkgs.erlang;
        app_version = "0.1.1";
        app_name = "migraterl";

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
              pname = app_name;
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
              libpq
              rebar3
            ];
          };

          # `nix develop`
          default = devenv.lib.mkShell {
            inherit inputs pkgs;
            modules = [
              (import ./devshell.nix {
                inherit pkgs tooling app_name;
              })
            ];
          };
        };

        # nix fmt
        formatter = treefmtEval.config.build.wrapper;
      }
    );
}
