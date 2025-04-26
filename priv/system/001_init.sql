CREATE SCHEMA IF NOT EXISTS migraterl;

CREATE TABLE IF NOT EXISTS migraterl.history (
    directory TEXT CONSTRAINT dir_length_upper_bound CHECK (
        LENGTH(filename) <= 255
    ) NOT NULL,
    filename TEXT CONSTRAINT file_length_upper_bound CHECK (
        LENGTH(filename) <= 255
    ) NOT NULL,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    UNIQUE (directory, filename),
    PRIMARY KEY (directory, filename)
);

-- INDEXES
CREATE INDEX IF NOT EXISTS index_migraterl_dir
ON migraterl.history (directory);

-- FUNCTIONS
CREATE OR REPLACE FUNCTION migraterl.from_directory(dir TEXT)
RETURNS TABLE (
    version BIGINT,
    directory TEXT,
    filename TEXT,
    created_at TIMESTAMPTZ
) AS $$
BEGIN
    RETURN QUERY
    SELECT 
        ROW_NUMBER() OVER w AS version,
        m.directory,
        m.filename,
        m.created_at
    FROM migraterl.history AS m
    WHERE m.directory = dir
    WINDOW w AS (PARTITION BY m.directory ORDER BY m.filename);
END;
$$ LANGUAGE plpgsql STABLE;

CREATE OR REPLACE FUNCTION migraterl.last_version(dir TEXT)
RETURNS TABLE (version BIGINT) AS $$
BEGIN
    RETURN QUERY
    SELECT COALESCE(MAX(fout.version),0) as version
    FROM migraterl.from_directory(dir) AS fout;
END;
$$ LANGUAGE plpgsql STABLE;
