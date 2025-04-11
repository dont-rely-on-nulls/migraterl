% ---------------
% Migration Types
% ---------------
-type version() :: non_neg_integer().
-type sql() :: string().
-type migration() :: {Version :: version(), Filename :: filename()}.
