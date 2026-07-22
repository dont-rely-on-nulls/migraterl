{ inputs, ... }:
{
  perSystem =
    {
      pkgs,
      inputs',
      config,
      ...
    }:
    {
      packages = {
        default =
          let
            # Name and version come from the relx tuple in rebar.config.
            project = import ./project.nix { inherit (pkgs) lib; };
            # rebar3Relx/buildRebar3 have no `rebar3` argument; each build
            # instead constructs `rebar3WithPlugins { plugins = buildPlugins; }`
            # internally. To make a plugin globally available we therefore set
            # `buildPlugins` on the release and on every dependency build.
            # pc (port_compiler) is required by fs's native inotify/fsevents port.
            buildPlugins = with pkgs.beamPackages; [ pc ];
            deps = import ../rebar-deps.nix {
              inherit (pkgs) fetchHex fetchFromGitHub fetchgit;
              builder = args: pkgs.beamPackages.buildRebar3 (args // { inherit buildPlugins; });
            };
          in
          pkgs.beamPackages.rebar3Relx {
            inherit buildPlugins;
            inherit (project) pname version;
            src = pkgs.lib.cleanSource ../.;
            releaseType = "release";
            profile = "prod";
            include = [
              "rebar.config"
            ];
            beamDeps = builtins.attrValues deps;
          };
      };
    };
}
