# treefmt.nix
{ pkgs, ... }:
{
  # Used to find the project root
  projectRootFile = "flake.nix";
  programs = {
    erlfmt.enable = true;
    just.enable = true;
    nixfmt.enable = true;
    sqruff.enable = true;
  };
}
