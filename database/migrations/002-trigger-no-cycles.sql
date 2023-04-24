CREATE OR REPLACE FUNCTION detect_cycle()
  RETURNS TRIGGER
  LANGUAGE plpgsql AS
$func$
BEGIN
  IF EXISTS (
      WITH RECURSIVE descendants(parent, child) AS (
          SELECT parent, child as level
          FROM subparts
          UNION
          SELECT d.parent, s.child
          FROM descendants d JOIN subparts s ON d.child=s.parent
      ) CYCLE child SET is_cycle USING path
      SELECT * FROM descendants
      WHERE parent = NEW.parent AND is_cycle IS TRUE
  )
  THEN
    RAISE EXCEPTION 'Loop detected';
  ELSE
    RETURN NEW;
  END IF;
END
$func$;

CREATE CONSTRAINT TRIGGER detect_subparts_cycle
AFTER INSERT OR UPDATE ON subparts
FOR EACH ROW EXECUTE PROCEDURE detect_cycle();
