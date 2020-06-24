-- original: whereB.test
-- credit:   http://www.sqlite.org/src/tree?ci=trunk&name=test

CREATE TABLE t1(x,y);    -- affinity of t1.y is NONE
    INSERT INTO t1 VALUES(1,99);

    CREATE TABLE t2(a, b TEXT);  -- affinity of t2.b is TEXT
    CREATE INDEX t2b ON t2(b);
    INSERT INTO t2 VALUES(2,99);

    SELECT x, a, y=b FROM t1, t2 ORDER BY +x, +a
;SELECT x, a, y=b FROM t1, t2 WHERE y=b
;SELECT x, a, y=b FROM t1, t2 WHERE b=y
;SELECT x, a, y=b FROM t1, t2 WHERE +y=+b
;DROP INDEX t2b;
    SELECT x, a, y=b FROM t1, t2 WHERE y=b
;SELECT x, a, y=b FROM t1, t2 WHERE b=y
;SELECT x, a, y=b FROM t1, t2 WHERE +y=+b
;DROP TABLE t1;
    DROP TABLE t2;

    CREATE TABLE t1(x, y TEXT);    -- affinity of t1.y is TEXT
    INSERT INTO t1 VALUES(1,99);

    CREATE TABLE t2(a, b BLOB);  -- affinity of t2.b is NONE
    CREATE INDEX t2b ON t2(b);
    INSERT INTO t2 VALUES(2,99);

    SELECT x, a, y=b FROM t1, t2 ORDER BY +x, +a
;SELECT x, a, y=b FROM t1, t2 WHERE y=b
;SELECT x, a, y=b FROM t1, t2 WHERE b=y
;SELECT x, a, y=b FROM t1, t2 WHERE +y=+b
;DROP INDEX t2b;
    SELECT x, a, y=b FROM t1, t2 WHERE y=b
;SELECT x, a, y=b FROM t1, t2 WHERE b=y
;SELECT x, a, y=b FROM t1, t2 WHERE +y=+b
;DROP TABLE t1;
    DROP TABLE t2;

    CREATE TABLE t1(x, y BLOB);    -- affinity of t1.y is NONE
    INSERT INTO t1 VALUES(1,99);

    CREATE TABLE t2(a, b BLOB);  -- affinity of t2.b is NONE
    CREATE INDEX t2b ON t2(b);
    INSERT INTO t2 VALUES(2,'99');

    SELECT x, a, y=b FROM t1, t2
;SELECT x, a, y=b FROM t1, t2 WHERE y=b
;SELECT x, a, y=b FROM t1, t2 WHERE b=y
;SELECT x, a, y=b FROM t1, t2 WHERE +y=+b
;DROP INDEX t2b;
    SELECT x, a, y=b FROM t1, t2 WHERE y=b
;SELECT x, a, y=b FROM t1, t2 WHERE b=y
;SELECT x, a, y=b FROM t1, t2 WHERE +y=+b
;DROP TABLE t1;
    DROP TABLE t2;

    CREATE TABLE t1(x, y BLOB);    -- affinity of t1.y is NONE
    INSERT INTO t1 VALUES(1,'99');

    CREATE TABLE t2(a, b NUMERIC);  -- affinity of t2.b is NUMERIC
    CREATE INDEX t2b ON t2(b);
    INSERT INTO t2 VALUES(2,99);

    SELECT x, a, y=b FROM t1, t2
;SELECT x, a, y=b FROM t1, t2 WHERE y=b
;SELECT x, a, y=b FROM t1, t2 WHERE b=y
;SELECT x, a, y=b FROM t1, t2 WHERE +y=+b
;DROP INDEX t2b;
    SELECT x, a, y=b FROM t1, t2 WHERE y=b
;SELECT x, a, y=b FROM t1, t2 WHERE b=y
;SELECT x, a, y=b FROM t1, t2 WHERE +y=+b
;DROP TABLE t1;
    DROP TABLE t2;

    CREATE TABLE t1(x, y BLOB);    -- affinity of t1.y is NONE
    INSERT INTO t1 VALUES(1,'99');

    CREATE TABLE t2(a, b INT);  -- affinity of t2.b is INTEGER
    CREATE INDEX t2b ON t2(b);
    INSERT INTO t2 VALUES(2,99);

    SELECT x, a, y=b FROM t1, t2
;SELECT x, a, y=b FROM t1, t2 WHERE y=b
;SELECT x, a, y=b FROM t1, t2 WHERE b=y
;SELECT x, a, y=b FROM t1, t2 WHERE +y=+b
;DROP INDEX t2b;
    SELECT x, a, y=b FROM t1, t2 WHERE y=b
;SELECT x, a, y=b FROM t1, t2 WHERE b=y
;SELECT x, a, y=b FROM t1, t2 WHERE +y=+b
;DROP TABLE t1;
    DROP TABLE t2;

    CREATE TABLE t1(x, y BLOB);    -- affinity of t1.y is NONE
    INSERT INTO t1 VALUES(1,'99');

    CREATE TABLE t2(a, b REAL);  -- affinity of t2.b is REAL
    CREATE INDEX t2b ON t2(b);
    INSERT INTO t2 VALUES(2,99.0);

    SELECT x, a, y=b FROM t1, t2
;SELECT x, a, y=b FROM t1, t2 WHERE y=b
;SELECT x, a, y=b FROM t1, t2 WHERE b=y
;SELECT x, a, y=b FROM t1, t2 WHERE +y=+b
;DROP INDEX t2b;
    SELECT x, a, y=b FROM t1, t2 WHERE y=b
;SELECT x, a, y=b FROM t1, t2 WHERE b=y
;SELECT x, a, y=b FROM t1, t2 WHERE +y=+b
;DROP TABLE t1;
    DROP TABLE t2;

    CREATE TABLE t1(x, y NUMERIC);  -- affinity of t1.y is NUMERIC
    INSERT INTO t1 VALUES(1,99);

    CREATE TABLE t2(a, b BLOB);  -- affinity of t2.b is NONE
    CREATE INDEX t2b ON t2(b);
    INSERT INTO t2 VALUES(2,'99');

    SELECT x, a, y=b FROM t1, t2
;SELECT x, a, y=b FROM t1, t2 WHERE y=b
;SELECT x, a, y=b FROM t1, t2 WHERE b=y
;SELECT x, a, y=b FROM t1, t2 WHERE +y=+b
;DROP INDEX t2b;
    SELECT x, a, y=b FROM t1, t2 WHERE y=b
;SELECT x, a, y=b FROM t1, t2 WHERE b=y
;SELECT x, a, y=b FROM t1, t2 WHERE +y=+b
;DROP TABLE t1;
    DROP TABLE t2;

    CREATE TABLE t1(x, y INT);  -- affinity of t1.y is INTEGER
    INSERT INTO t1 VALUES(1,99);

    CREATE TABLE t2(a, b BLOB);  -- affinity of t2.b is NONE
    CREATE INDEX t2b ON t2(b);
    INSERT INTO t2 VALUES(2,'99');

    SELECT x, a, y=b FROM t1, t2
;SELECT x, a, y=b FROM t1, t2 WHERE y=b
;SELECT x, a, y=b FROM t1, t2 WHERE b=y
;SELECT x, a, y=b FROM t1, t2 WHERE +y=+b
;DROP INDEX t2b;
    SELECT x, a, y=b FROM t1, t2 WHERE y=b
;SELECT x, a, y=b FROM t1, t2 WHERE b=y
;SELECT x, a, y=b FROM t1, t2 WHERE +y=+b
;DROP TABLE t1;
    DROP TABLE t2;

    CREATE TABLE t1(x, y REAL);  -- affinity of t1.y is REAL
    INSERT INTO t1 VALUES(1,99.0);

    CREATE TABLE t2(a, b BLOB);  -- affinity of t2.b is NONE
    CREATE INDEX t2b ON t2(b);
    INSERT INTO t2 VALUES(2,'99');

    SELECT x, a, y=b FROM t1, t2
;SELECT x, a, y=b FROM t1, t2 WHERE y=b
;SELECT x, a, y=b FROM t1, t2 WHERE b=y
;SELECT x, a, y=b FROM t1, t2 WHERE +y=+b
;DROP INDEX t2b;
    SELECT x, a, y=b FROM t1, t2 WHERE y=b
;SELECT x, a, y=b FROM t1, t2 WHERE b=y
;SELECT x, a, y=b FROM t1, t2 WHERE +y=+b;