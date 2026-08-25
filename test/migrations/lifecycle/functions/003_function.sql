CREATE OR REPLACE FUNCTION lifecycle.event_count ()
    RETURNS bigint
    LANGUAGE SQL
    AS $$
    SELECT
        COUNT(*)
    FROM
        lifecycle.events;
$$;

