-- original: tkt3424.test
-- credit:   http://www.sqlite.org/src/tree?ci=trunk&name=test

CREATE TABLE names(id INTEGER, data TEXT, code TEXT);
    INSERT INTO names VALUES(1,'E1','AAA');
    INSERT INTO names VALUES(2,NULL,'BBB');

    CREATE TABLE orig(code TEXT, data TEXT);
    INSERT INTO orig VALUES('AAA','E1');
    INSERT INTO orig VALUES('AAA','E2');
    INSERT INTO orig VALUES('AAA','E3');
    INSERT INTO orig VALUES('AAA','E4');
    INSERT INTO orig VALUES('AAA','E5')
;SELECT * FROM 
    names LEFT OUTER JOIN orig
    ON names.data = orig.data AND names.code = orig.code
;CREATE INDEX udx_orig_code_data ON orig(code, data)
;SELECT * FROM 
    names LEFT OUTER JOIN orig
    ON names.data = orig.data AND names.code = orig.code;