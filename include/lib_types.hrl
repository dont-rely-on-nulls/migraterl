% ---------------
% Migration Types
% ---------------
-type version() :: non_neg_integer().
-type sql() :: string().
-type migration() :: {Version :: version(), Filename :: filename()}.

% Library Errors
-type error_code() ::
    read_directory_failure
    | init_failure
    | empty_sql_file
    | upgrade_failure
    | unmapped_case
    | db_connection_error.
-type error() :: {error, Type :: error_code(), Reason :: term()}.
