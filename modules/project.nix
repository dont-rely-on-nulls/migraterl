# Single source of truth for the release name and version.
#
# Parses the relx `{release, {Name, "Vsn"}, ...}` tuple out of rebar.config so
# the flake never hardcodes them. Whitespace is stripped first so the match is
# insensitive to formatting and line breaks.
{ lib }:
let
  rebarConfig = builtins.readFile ../rebar.config;
  stripped = lib.replaceStrings [ " " "\t" "\n" "\r" ] [ "" "" "" "" ] rebarConfig;
  # POSIX ERE: match literal braces via bracket expressions, not backslashes.
  m = builtins.match ''.*[{]release,[{]([a-zA-Z0-9_]+),"([^"]+)"[}].*'' stripped;
in
if m == null then
  throw "project.nix: could not parse {release, {Name, \"Vsn\"}} from rebar.config"
else
  {
    pname = builtins.elemAt m 0;
    version = builtins.elemAt m 1;
  }
