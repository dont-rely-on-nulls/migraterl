-- migraterl journal schema.
--
-- Requires PostgreSQL 18+: we rely on application-time temporal support
-- (WITHOUT OVERLAPS primary keys and UPDATE ... FOR PORTION OF) to keep a
-- full audit trail of every time a script was applied or re-applied without
-- any triggers or extensions.

CREATE SCHEMA IF NOT EXISTS migraterl;

-- A WITHOUT OVERLAPS primary key is enforced by a GiST exclusion index, so the
-- scalar equality columns (namespace, script_name) need GiST operator classes.
-- btree_gist is a trusted extension, installable by any role with CREATE on the
-- database.
CREATE EXTENSION IF NOT EXISTS btree_gist;

-- One logical row per (namespace, script) is "currently in force"
-- (upper(valid_period) IS NULL). Re-applying a run-on-change script closes the
-- previous period and opens a new one via UPDATE ... FOR PORTION OF, so history
-- is preserved as additional, non-overlapping rows.
CREATE TABLE IF NOT EXISTS migraterl.schema_journal (
namespace     TEXT NOT NULL
CONSTRAINT namespace_len CHECK (length(namespace) <= 255),
script_name   TEXT NOT NULL
CONSTRAINT script_name_len CHECK (length(script_name) <= 255),
content_hash  TEXT NOT NULL,
script_class  TEXT NOT NULL DEFAULT 'once'
CONSTRAINT script_class_enum CHECK (script_class IN ('once', 'on_change')),
checksum_algo TEXT NOT NULL DEFAULT 'sha256',
execution_ms  INTEGER,
applied_by    TEXT NOT NULL DEFAULT current_user,
-- application-time validity window; open-ended = currently in force
valid_period  TSTZRANGE NOT NULL DEFAULT tstzrange(current_timestamp, NULL),
-- PG18 temporal primary key: multiple rows per script are allowed as long
-- as their validity periods never overlap.
CONSTRAINT schema_journal_pk
PRIMARY KEY (namespace, script_name, valid_period WITHOUT OVERLAPS)
);

-- Fast lookup of the currently-applied set for a namespace.
CREATE INDEX IF NOT EXISTS index_schema_journal_current
ON migraterl.schema_journal (namespace, script_name)
WHERE upper(valid_period) IS NULL;

-- Convenience view: the live migration state (what has been applied "now").
CREATE OR REPLACE VIEW migraterl.current_state AS
SELECT
namespace,
script_name,
content_hash,
script_class,
lower(valid_period) AS applied_at,
execution_ms,
applied_by
FROM migraterl.schema_journal
WHERE upper(valid_period) IS NULL;
