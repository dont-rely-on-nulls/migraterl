CREATE TABLE IF NOT EXISTS migraterl_history (
    version BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    filename TEXT UNIQUE CONSTRAINT length_upper_bound CHECK (
        LENGTH(filename) <= 255
    ) NOT NULL,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
