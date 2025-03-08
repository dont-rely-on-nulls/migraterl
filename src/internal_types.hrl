-type filename() :: file:name_all().
-type directory() :: file:name_all().
-type version() :: non_neg_integer().
-type migration() :: {Version :: version(), Filename :: file:name_all()}.
-type error() :: {error, Type :: atom()}.
