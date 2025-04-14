-- Types
DO $$ BEGIN
    IF to_regtype('test_schema.status') IS NULL THEN
        CREATE TYPE test_schema.status AS ENUM(
            'READY',
            'RUNNING',
            'SUCCEEDED',
            'FAILED',
        );
    END IF;
END $$;

-- Tables
CREATE TABLE IF NOT EXISTS test_schema.example_table (
    id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    status test_schema.STATUS NOT NULL,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
