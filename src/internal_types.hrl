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
-type error_code() ::
    read_directory_failure | init_failure | upgrade_failure.
-type error() :: {error, Type :: error_code(), Reason :: any()}.
