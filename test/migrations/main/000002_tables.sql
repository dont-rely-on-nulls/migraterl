-- Types
DO $$ BEGIN
    IF to_regtype('test.status') IS NULL THEN
        CREATE TYPE test.status AS ENUM(
            'READY',
            'RUNNING',
            'SUCCEEDED',
            'FAILED'
        );
    END IF;
END $$;

-- Tables
CREATE TABLE IF NOT EXISTS test.example (
    id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    status TEST.STATUS NOT NULL,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
