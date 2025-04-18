% ---------------
% Migration Types
% ---------------
-type version() :: non_neg_integer().
-type state() :: init | created.
-type mode() :: setup | apply_once | repeat.
-type migration() :: {Version :: version(), Filename :: filename()}.
-type sql() :: string().
-type options() :: #{repeatable => boolean()}.
