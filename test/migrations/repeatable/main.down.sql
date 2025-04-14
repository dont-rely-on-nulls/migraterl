CREATE OR REPLACE FUNCTION drop_all()
RETURNS VOID AS
$$
   DECLARE rec RECORD; 
   BEGIN
       -- Get all the schemas
        FOR rec IN
        SELECT nspname FROM pg_catalog.pg_namespace WHERE (nspname NOT LIKE 'pg_%') and (nspname != 'information_schema')
           LOOP
             EXECUTE format('DROP SCHEMA "%s" CASCADE', rec.nspname);
           END LOOP; 
           RETURN; 
   END;
   $$ LANGUAGE plpgsql;
