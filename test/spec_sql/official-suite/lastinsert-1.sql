-- original: lastinsert.test
-- credit:   http://www.sqlite.org/src/tree?ci=trunk&name=test

CREATE TABLE t2(x INTEGER PRIMARY KEY, y);
    CREATE TABLE t3(a, b);
    CREATE TRIGGER after_t2 AFTER INSERT ON t2 BEGIN
      INSERT INTO t3 VALUES(new.x, new.y);
    END;
    INSERT INTO t2 VALUES(5000000000, 1);
    SELECT last_insert_rowid()
;INSERT INTO t2 VALUES(123456789012345,0);