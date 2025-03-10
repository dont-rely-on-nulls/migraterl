% File/Directory-related types
-type filename() :: file:name_all().
-type directory() :: file:name_all().

% ---------------
% Migration Types
% ---------------
-type version() :: non_neg_integer().
-type sql() :: nonempty_string().
-type migration() :: {Version :: version(), Filename :: filename()}.

% Library Errors
-type error_code() :: directory_does_not_exist | directory_is_empty.
-type error() :: {error, Type :: atom()}.
