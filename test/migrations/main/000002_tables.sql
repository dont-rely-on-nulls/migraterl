-- Types
DO $$
BEGIN
    IF TO_REGTYPE('test.status') IS NULL THEN
        CREATE TYPE test.status AS ENUM (
            'READY',
            'RUNNING',
            'SUCCEEDED',
            'FAILED'
);
    END IF;
END
$$;

-- Tables
CREATE TABLE IF NOT EXISTS test.example (
    id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    status TEST.STATUS NOT NULL,
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP
);

