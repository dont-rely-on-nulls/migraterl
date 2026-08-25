CREATE OR REPLACE FUNCTION test.add (a integer, b integer)
    RETURNS integer
    LANGUAGE SQL
    IMMUTABLE
    RETURNS NULL ON NULL INPUT RETURN a + b;

