DO $$ BEGIN 
  IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'test_role') THEN
    CREATE ROLE test_role WITH LOGIN INHERIT;
  END IF;
END $$;
