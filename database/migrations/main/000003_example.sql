-- Types
DO $$ BEGIN
    IF to_regtype('example.status') IS NULL THEN
        CREATE TYPE example.status AS ENUM(
            'READY',
            'RUNNING',
            'SUCCEEDED',
            'FAILED',
        );
    END IF;
END $$;

-- Tables
CREATE TABLE IF NOT EXISTS example (
    id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    status BUILD.STATUS NOT NULL,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
