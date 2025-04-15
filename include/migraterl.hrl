% ---------------
% Migration Types
% ---------------
-type version() :: non_neg_integer().
-type sql() :: string().
-type migration() :: {Version :: version(), Filename :: filename()}.
-type options() :: #{repeatable => boolean()}.
