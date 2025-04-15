{
  pkgs,
  tooling ? [ ],
  app_name ? "app_user",
}:
rec {
  packages = tooling;

  languages.erlang = {
    enable = true;
  };

  scripts = {
    build.exec = "rebar3 compile";
    shell.exec = "rebar3 shell";
    test.exec = "just t";
  };

  env = {
    # https://www.erlang.org/doc/man/kernel_app.html
    ERL_AFLAGS = "-kernel shell_history enabled";
    # Devenv sets this to something else, so I'll be
    # using "PG_HOST" instead.
    # https://www.postgresql.org/docs/7.0/libpq-envars.htm
    PG_HOST = "127.0.0.1";
  };

  enterShell = ''
    echo "Starting Development Environment..."
  '';

  services.postgres = {
    enable = true;
    package = pkgs.postgresql_17;
    extensions = ext: [
      ext.periods
      ext.pg_cron
    ];
    initdbArgs = [
      "--locale=C"
      "--encoding=UTF8"
    ];
    settings = {
      shared_preload_libraries = "pg_stat_statements";
      # pg_stat_statements config, nested attr sets need to be
      # converted to strings, otherwise postgresql.conf fails
      # to be generated.
      compute_query_id = "on";
      "pg_stat_statements.max" = 10000;
      "pg_stat_statements.track" = "all";
    };
    initialDatabases = [
      {
        name = app_name;
        user = app_name;
        pass = app_name;
      }
    ];
    port = 5432;
    listen_addresses = env.PG_HOST;
    initialScript = ''
      CREATE EXTENSION IF NOT EXISTS pg_stat_statements;
      CREATE ROLE postgres WITH SUPERUSER LOGIN PASSWORD 'postgres';
      CREATE ROLE test_user LOGIN PASSWORD 'postgres';
      ALTER DATABASE ${app_name} OWNER TO postgres;
    '';
  };
}
