-- original: fts3af.test
-- credit:   http://www.sqlite.org/src/tree?ci=trunk&name=test

CREATE VIRTUAL TABLE t1 USING fts3(content);
  INSERT INTO t1 (rowid, content) VALUES(1, 'one');
  INSERT INTO t1 (rowid, content) VALUES(2, 'two');
  INSERT INTO t1 (rowid, content) VALUES(3, 'one two');
  INSERT INTO t1 (rowid, content) VALUES(4, 'three');
  INSERT INTO t1 (rowid, content) VALUES(5, 'one three');
  INSERT INTO t1 (rowid, content) VALUES(6, 'two three');
  INSERT INTO t1 (rowid, content) VALUES(7, 'one two three');
  DELETE FROM t1 WHERE rowid = 4;
  INSERT INTO t1 (rowid, content) VALUES(8, 'four');
  UPDATE t1 SET content = 'update one three' WHERE rowid = 1;
  INSERT INTO t1 (rowid, content) VALUES(9, 'one four');
  INSERT INTO t1 (rowid, content) VALUES(10, 'two four');
  DELETE FROM t1 WHERE rowid = 7;
  INSERT INTO t1 (rowid, content) VALUES(11, 'one two four');
  INSERT INTO t1 (rowid, content) VALUES(12, 'three four');
  INSERT INTO t1 (rowid, content) VALUES(13, 'one three four');
  DELETE FROM t1 WHERE rowid = 10;
  INSERT INTO t1 (rowid, content) VALUES(14, 'two three four');
  INSERT INTO t1 (rowid, content) VALUES(15, 'one two three four');
  UPDATE t1 SET content = 'update two five' WHERE rowid = 8;
  INSERT INTO t1 (rowid, content) VALUES(16, 'five');
  DELETE FROM t1 WHERE rowid = 13;
  INSERT INTO t1 (rowid, content) VALUES(17, 'one five');
  INSERT INTO t1 (rowid, content) VALUES(18, 'two five');
  INSERT INTO t1 (rowid, content) VALUES(19, 'one two five');
  DELETE FROM t1 WHERE rowid = 16;
  INSERT INTO t1 (rowid, content) VALUES(20, 'three five');
  INSERT INTO t1 (rowid, content) VALUES(21, 'one three five');
  INSERT INTO t1 (rowid, content) VALUES(22, 'two three five');
  DELETE FROM t1 WHERE rowid = 19;
  UPDATE t1 SET content = 'update' WHERE rowid = 15
;SELECT COUNT(*) FROM t1
;SELECT rowid FROM t1 WHERE content MATCH 'update'
;SELECT rowid FROM t1 WHERE content MATCH 'one'
;SELECT rowid FROM t1 WHERE content MATCH 'two'
;SELECT rowid FROM t1 WHERE content MATCH 'three'
;SELECT rowid FROM t1 WHERE content MATCH 'four'
;SELECT rowid FROM t1 WHERE content MATCH 'five';