"use strict";

require("babel-polyfill");

var _ramda = require("ramda");

// Needed to allow its use in older versions of Node and Browsers.
var INDENT = "\t";
var LINE_END = "\n"; // This allows calling a function recursivly based on node type. Some of the
// nodes have non-standard types and so we need to modify how we call the
// Generator.

var recurse = (0, _ramda.curry)(function (Generator, n) {
  switch (n.type) {
    case "function":
      return Generator["function"](n);

    case "module":
      return Generator.module(n);

    case "assignment":
      return Generator.assignment(n);

    case "event":
      return Generator.event(n);

    default:
      return Generator[n.type][n.variant](n);
  }
});
var mapr = (0, _ramda.compose)(_ramda.map, recurse);

var datatype = function datatype(n) {
  return n.variant;
};

var returnNewLine = (0, _ramda.join)("\n");
var joinList = (0, _ramda.join)(", ");
var terminateStatements = (0, _ramda.map)((0, _ramda.concat)(_ramda.__, ";"));

var containsSelect = function containsSelect(s) {
  return s.indexOf("SELECT") !== -1;
};

var isOfFormat = function isOfFormat(n) {
  return (0, _ramda.compose)((0, _ramda.equals)(n), (0, _ramda.prop)("format"));
};

var escapeTextLiteral = function escapeTextLiteral(s) {
  return s.replace(/'/g, "''");
};

var Generator = {
  assignment: function assignment(n) {
    var recurser = recurse(Generator);
    var target = recurser(n.target);
    var value = recurser(n.value);
    return "".concat(target, " = ").concat(value);
  },
  statement: {
    list: function list(n) {
      var recourseOverList = mapr(Generator);
      var statements = (0, _ramda.compose)(returnNewLine, terminateStatements, recourseOverList);
      return statements(n.statement);
    },
    select: function select(n) {
      var recurser = recurse(Generator);
      var recourseList = mapr(Generator);
      var argsList = (0, _ramda.compose)(joinList, recourseList);
      var str = [""];

      if (n["with"]) {
        var withS = recourseList(n["with"]);

        var isRecursive = function isRecursive(n) {
          return (0, _ramda.isArrayLike)(n) ? (0, _ramda.compose)((0, _ramda.contains)("recursive"), (0, _ramda.pluck)("variant"))(n) : _ramda.F;
        };

        var w = isRecursive(n["with"]) ? "WITH RECURSIVE" : "WITH";
        str.push("".concat(w, " ").concat(withS).concat(LINE_END));
      }

      str.push("SELECT ");

      if (n.result) {
        var results = argsList(n.result);
        str.push("".concat(results).concat(LINE_END));
      }

      if (n.from) {
        var from = recurser(n.from);
        str.push("".concat(INDENT, "FROM (").concat(from, ")").concat(LINE_END));
      }

      if (n.where) {
        var where = recurser((0, _ramda.head)(n.where));
        str.push("".concat(INDENT, "WHERE ").concat(where).concat(LINE_END));
      }

      if (n.group) {
        var group = recurser(n.group);
        str.push("".concat(INDENT, "GROUP BY ").concat(group).concat(LINE_END));
      }

      if (n.having) {
        var having = recurser(n.having);
        str.push("".concat(INDENT, "HAVING ").concat(having).concat(LINE_END));
      }

      if (n.order) {
        var order = recourseList(n.order);
        str.push("".concat(INDENT, "ORDER BY ").concat(order).concat(LINE_END));
      }

      if (n.limit) {
        var limit = recurser(n.limit);
        str.push("".concat(INDENT).concat(limit));
      }

      return str.join("");
    },
    compound: function compound(n) {
      var recourseList = mapr(Generator);
      var statement = recurse(Generator)(n.statement);
      var compound = recourseList(n.compound).join("");
      var order = n.order ? "".concat(INDENT, "ORDER BY ").concat(recourseList(n.order)).concat(LINE_END) : "";
      return "".concat(statement).concat(compound, " ").concat(order);
    },
    create: function create(n) {
      var recurser = recurse(Generator);
      var isCreateIndex = isOfFormat("index");
      var isCreateTable = isOfFormat("table");
      var isCreateView = isOfFormat("view");
      var isCreateVirtual = isOfFormat("virtual");
      var isCreateTrigger = isOfFormat("trigger");

      if (isCreateTrigger(n)) {
        var m = mapr(Generator);
        var target = recurser(n.target);
        var by = n.by ? "FOR EACH ".concat(n.by) : "";
        var event = recurser(n.event);
        var on = recurser(n.on);
        var action = (0, _ramda.compose)((0, _ramda.join)(";\n"), m)(n.action);
        var when = recurser(n.when);
        var temporary = !!n.temporary ? "TEMPORARY" : "";
        var condition = n.condition ? m(n.condition) : "";
        return "CREATE ".concat(temporary, " TRIGGER ").concat(condition, " ").concat(target, " ").concat(event, " ON ").concat(on, " ").concat(by, " WHEN ").concat(when, " BEGIN ").concat(action, "; END");
      }

      if (isCreateVirtual(n)) {
        var _target = recurser(n.target);

        var result = recurser(n.result);
        return "CREATE VIRTUAL TABLE ".concat(_target, " USING ").concat(result);
      }

      if (isCreateView(n)) {
        var viewName = recurser(n.target);

        var _result = recurser(n.result);

        return "CREATE VIEW ".concat(viewName).concat(LINE_END, "AS ").concat(_result);
      }

      if (isCreateIndex(n)) {
        var indexName = n.target.name;
        var onColumns = recurser(n.on);
        var where = recurser((0, _ramda.head)(n.where));
        return "CREATE INDEX ".concat(indexName).concat(LINE_END, "ON ").concat(onColumns).concat(LINE_END, "WHERE ").concat(where);
      }

      if (isCreateTable(n)) {
        var tableName = recurse(Generator)(n.name);
        var definitionsList = (0, _ramda.compose)((0, _ramda.join)(",".concat(LINE_END)), mapr(Generator));
        var definitions = definitionsList(n.definition); // Can probable be refactored to be a bit more elegant... :/

        var defaultCreateSyntax = "CREATE TABLE ".concat(tableName, " (").concat(LINE_END).concat(definitions).concat(LINE_END, ")");
        var createTableFromSelect = "CREATE TABLE ".concat(tableName, " AS").concat(LINE_END).concat(definitions).concat(LINE_END);
        return containsSelect(definitions) ? createTableFromSelect : defaultCreateSyntax;
      }

      return "";
    },
    insert: function insert(n) {
      var recurser = recurse(Generator);
      var into = recurser(n.into); // This is an insert into default values

      if (n.result.variant === "default") {
        return "INSERT INTO ".concat(into).concat(LINE_END, "DEFAULT VALUES");
      } // This is an insert into select


      if (n.result.variant === "select") {
        var _result2 = recurser(n.result);

        return "INSERT INTO ".concat(into).concat(LINE_END).concat(_result2);
      } // Otherwise we build up the values to be inserted


      var addBrackets = (0, _ramda.map)(function (s) {
        return "(".concat(s, ")");
      });
      var valuesList = (0, _ramda.compose)((0, _ramda.join)(",".concat(LINE_END)), addBrackets, mapr(Generator));
      var result = valuesList(n.result);
      return "INSERT INTO ".concat(into).concat(LINE_END, "VALUES ").concat(result);
    },
    "delete": function _delete(n) {
      var recurser = recurse(Generator);
      var str = ["DELETE "];

      if (n.from) {
        var from = recurser(n.from);
        str.push("".concat(INDENT, "FROM ").concat(from).concat(LINE_END));
      }

      if (n.where) {
        var whereNode = (0, _ramda.head)(n.where);
        var where = recurser(whereNode);
        str.push("".concat(INDENT, "WHERE ").concat(where).concat(LINE_END));
      }

      if (n.limit) {
        var limit = recurser(n.limit);
        str.push("".concat(INDENT).concat(limit));
      }

      return str.join("");
    },
    drop: function drop(n) {
      var recurser = recurse(Generator);
      var condition = n.condition.length > 0 ? mapr(Generator)(n.condition) : "";
      var target = recurser(n.target);
      return "DROP ".concat(n.format, " ").concat(condition, " ").concat(target);
    },
    update: function update(n) {
      var recurser = recurse(Generator);
      var into = recurser(n.into);
      var setS = mapr(Generator)(n.set);
      var str = ["UPDATE ".concat(into, " SET ").concat(setS)];

      if (n.where) {
        var whereNode = (0, _ramda.head)(n.where);
        var where = recurser(whereNode);
        str.push("".concat(INDENT, "WHERE ").concat(where).concat(LINE_END));
      }

      if (n.limit) {
        var limit = recurser(n.limit);
        str.push("".concat(INDENT).concat(limit));
      }

      return str.join("");
    },
    transaction: function transaction(n) {
      var isOfActionType = function isOfActionType(type) {
        return function (action) {
          return action === type;
        };
      };

      var isBegin = isOfActionType("begin");
      var isRollback = isOfActionType("rollback");

      if (isBegin(n.action)) {
        return "".concat(n.action, " ").concat(n.defer, " TRANSACTION");
      }

      if (isRollback(n.action)) {
        return "ROLLBACK TRANSACTION TO SAVEPOINT ".concat(n.savepoint.name);
      }

      return "COMMIT";
    },
    release: function release(n) {
      var recurser = recurse(Generator);
      var savepoint = recurser(n.target.savepoint);
      return "RELEASE SAVEPOINT ".concat(savepoint);
    },
    savepoint: function savepoint(n) {
      var recurser = recurse(Generator);
      var savepoint = recurser(n.target.savepoint);
      return "SAVEPOINT ".concat(savepoint);
    }
  },
  compound: {
    union: function union(n) {
      var statement = recurse(Generator)(n.statement);
      return "".concat((0, _ramda.toUpper)(n.variant)).concat(LINE_END).concat(statement);
    },

    get "union all"() {
      return this.union;
    },

    get except() {
      return this.union;
    },

    get intersect() {
      return this.union;
    }

  },
  identifier: {
    star: function star(n) {
      return n.name;
    },
    table: function table(n) {
      var alias = n.alias ? "AS ".concat(n.alias) : "";
      var index = n.index ? recurse(Generator)(n.index) : "";
      return "`".concat(n.name, "` ").concat(alias, " ").concat(index);
    },
    index: function index(n) {
      return "INDEXED BY ".concat(n.name);
    },
    column: function column(n) {
      var recurser = recurse(Generator);
      var alias = n.alias ? "AS `".concat(n.alias, "`") : "";
      var index = n.index ? recurser(n.index) : "";
      return "`".concat(n.name, "` ").concat(alias, " ").concat(index);
    },
    "function": function _function(n) {
      return n.name;
    },
    expression: function expression(n) {
      var m = mapr(Generator);
      return "`".concat(n.name, "`(").concat(m(n.columns), ")");
    },
    view: function view(n) {
      return n.name;
    },
    savepoint: function savepoint(n) {
      return n.name;
    },
    trigger: function trigger(n) {
      return "\"".concat(n.name, "\"");
    }
  },
  literal: {
    text: function text(n) {
      return "'".concat(escapeTextLiteral(n.value), "'");
    },
    decimal: function decimal(n) {
      return "".concat(n.value);
    },
    "null": function _null() {
      return "NULL";
    }
  },
  expression: {
    operation: function operation(n) {
      var recurser = recurse(Generator);
      var isUnaryOperation = isOfFormat("unary");

      if (isUnaryOperation(n)) {
        var expression = recurser(n.expression);
        var operator = n.operator ? "".concat(n.operator) : "";

        var _alias = n.alias ? "AS [".concat(n.alias, "]") : "";

        return "".concat(operator, " ").concat(expression, " ").concat(_alias);
      }

      var isBetween = function isBetween(n) {
        return n.operation === "between";
      };

      var isExpression = function isExpression(n) {
        return n.type === "expression";
      };

      var side = function side(s) {
        var sideOp = recurser(n[s]);
        return !isBetween(n) && (isExpression(n[s]) || containsSelect(sideOp)) ? "(".concat(sideOp, ")") : sideOp;
      };

      var left = side("left");
      var right = side("right");
      var alias = n.alias ? "AS `".concat(n.alias, "`") : "";
      return "".concat(left, " ").concat(n.operation, " ").concat(right, " ").concat(alias);
    },
    list: function list(n) {
      var argsList = (0, _ramda.compose)(joinList, mapr(Generator));
      return argsList(n.expression);
    },
    order: function order(n) {
      var recurser = recurse(Generator);
      var expression = recurser(n.expression);
      var direction = n.direction;
      return "".concat(expression, " ").concat((0, _ramda.toUpper)(direction));
    },
    limit: function limit(n) {
      var recurser = recurse(Generator);
      var limit = recurser(n.start);
      var offset = n.offset ? "OFFSET ".concat(recurser(n.offset)) : "";
      return "LIMIT ".concat(limit).concat(LINE_END).concat(INDENT).concat(offset);
    },
    cast: function cast(n) {
      var recurser = recurse(Generator);
      var expression = recurser(n.expression);
      var as = recurser(n.as);
      var alias = n.alias ? "AS [".concat(n.alias, "]") : "";
      return "CAST(".concat(expression, " AS ").concat(as, ")").concat(alias);
    },
    common: function common(n) {
      var recurser = recurse(Generator);
      var expression = recurser(n.expression);
      var target = recurser(n.target);
      return "".concat(target, " AS (").concat(expression, ")");
    },
    "case": function _case(n) {
      var recurser = recurse(Generator);
      var mapConditions = (0, _ramda.compose)((0, _ramda.join)(LINE_END), mapr(Generator));
      var discriminant = n.discriminant ? recurser(n.discriminant) : "";
      var conditions = mapConditions(n.expression);
      var alias = n.alias ? "AS [".concat(n.alias, "]") : "";
      return "CASE ".concat(discriminant, " ").concat(conditions, " END ").concat(alias);
    },
    recursive: function recursive(n) {
      var recurser = recurse(Generator);
      var target = recurser(n.target);
      var expression = recurser(n.expression);
      return "".concat(target, " AS (").concat(expression, ")");
    },
    exists: function exists(n) {
      return n.operator;
    }
  },
  condition: {
    when: function when(n) {
      var recurser = recurse(Generator);
      var when = recurser(n.condition);
      var then = recurser(n.consequent);
      return "WHEN ".concat(when, " THEN ").concat(then);
    },
    "else": function _else(n) {
      var recurser = recurse(Generator);
      var elseS = recurser(n.consequent);
      return "ELSE ".concat(elseS);
    },
    "if": function _if(n) {
      var recurser = recurse(Generator);
      var exists = recurser(n.condition);
      return "IF ".concat(exists);
    }
  },
  "function": function _function(n) {
    var recurser = recurse(Generator);
    var name = (0, _ramda.toUpper)(recurser(n.name));
    var args = recurser(n.args);
    var alias = n.alias ? "AS `".concat(n.alias, "`") : "";
    return "".concat(name, "(").concat(args, ") ").concat(alias);
  },
  module: function module(n) {
    var recurser = recurse(Generator);
    var args = recurser(n.args);
    var alias = n.alias ? "AS `".concat(n.alias, "`") : "";
    return "".concat(n.name, "(").concat(args, ") ").concat(alias);
  },
  event: function event(_ref) {
    var _event = _ref.event,
        occurs = _ref.occurs,
        of = _ref.of;
    var processedOf = of ? "OF ".concat(mapr(Generator)(of)) : "";
    return "".concat(occurs, " ").concat(_event, " ").concat(processedOf);
  },
  map: {
    join: function join(n) {
      var recurser = recurse(Generator);
      var source = recurser(n.source);
      var sourceAlias = n.source.alias ? n.source.alias : "";
      var join = recurser((0, _ramda.head)(n.map)); // Its a select subquery

      if (containsSelect(source)) {
        var subquery = "(".concat(source, ") AS ").concat(sourceAlias, " ").concat(join);
        return subquery;
      } // Its an inner join.


      return "".concat(source, " ").concat(join);
    }
  },
  join: {
    join: function join(n) {
      var recurser = recurse(Generator);
      var source = recurser(n.source);
      var constraint = recurser(n.constraint);
      return "".concat(INDENT, "JOIN ").concat(source).concat(LINE_END).concat(constraint);
    },
    "inner join": function innerJoin(n) {
      var recurser = recurse(Generator);
      var source = recurser(n.source);
      var sourceAlias = n.source.alias ? " AS ".concat(n.source.alias) : "";
      var constraint = recurser(n.constraint);
      return "".concat(INDENT, "INNER JOIN (").concat(source, ")").concat(sourceAlias).concat(LINE_END).concat(constraint);
    },
    "left outer join": function leftOuterJoin(n) {
      var recurser = recurse(Generator);
      var source = recurser(n.source);
      var constraint = recurser(n.constraint);
      return "".concat(INDENT, "LEFT OUTER JOIN ").concat(source).concat(LINE_END).concat(constraint);
    },
    "cross join": function crossJoin(n) {
      var recurser = recurse(Generator);
      var source = recurser(n.source);
      return ", ".concat(source);
    }
  },
  constraint: {
    join: function join(n) {
      var isFormatUsing = isOfFormat("using");
      var isFormatOn = isOfFormat("on");
      var recurser = recurse(Generator);

      if (isFormatOn(n)) {
        var on = recurser(n.on);
        return "".concat(INDENT, "ON ").concat(on).concat(LINE_END);
      }

      if (isFormatUsing(n)) {
        var using = mapr(Generator)(n.using.columns);
        return "".concat(INDENT, "USING (").concat(using, ")").concat(LINE_END);
      }

      return "";
    },
    "primary key": function primaryKey() {
      return "PRIMARY KEY";
    },
    "not null": function notNull() {
      return "NOT NULL";
    },
    unique: function unique() {
      return "UNIQUE";
    },
    check: function check(n) {
      var check = recurse(Generator)(n.expression);
      return "CHECK (".concat(check, ")");
    },
    "foreign key": function foreignKey(n) {
      var recurser = recurse(Generator);
      var ref = recurser(n.references);
      return "REFERENCES ".concat(ref);
    },
    "null": function _null() {
      return "NULL";
    }
  },
  definition: {
    column: function column(n) {
      var recurser = recurse(Generator);
      var datatype = (0, _ramda.isArrayLike)(n.datatype) ? mapr(Generator, n.datatype) : recurser(n.datatype);
      var constraintsList = (0, _ramda.compose)((0, _ramda.join)(" "), (0, _ramda.map)(recurser));
      var constraints = constraintsList(n.definition);
      return "".concat(n.name, " ").concat(datatype, " ").concat(constraints);
    },
    constraint: function constraint(n) {
      var recurser = recurse(Generator);

      var checkConstraint = function checkConstraint(type) {
        return function (n) {
          if ((0, _ramda.isEmpty)(n)) {
            return _ramda.F;
          }

          var constraintType = (0, _ramda.compose)((0, _ramda.prop)("variant"), _ramda.head);
          return (0, _ramda.equals)(constraintType(n), type);
        };
      };

      var isForeignKey = checkConstraint("foreign key");
      var isPrimaryKey = checkConstraint("primary key");

      if (isForeignKey(n.definition)) {
        var childKey = recurser((0, _ramda.head)(n.columns));
        var parentKey = recurser((0, _ramda.head)(n.definition));
        return "FOREIGN KEY (".concat(childKey, ") ").concat(parentKey);
      }

      if (isPrimaryKey(n.definition)) {
        var field = recurser((0, _ramda.head)(n.columns));
        var conflict = (0, _ramda.prop)("conflict", (0, _ramda.head)(n.definition));
        return "PRIMARY KEY (".concat(field, ") ON CONFLICT ").concat(conflict);
      }

      return recurser((0, _ramda.head)(n.definition));
    }
  },
  datatype: {
    "int": datatype,
    varchar: function varchar(n) {
      var arg = recurse(Generator)(n.args);
      return "".concat(n.variant, "(").concat(arg, ")");
    },
    blob: datatype,
    "double": datatype,
    int8: datatype,
    text: datatype,
    tinyint: datatype,
    smallint: datatype,
    mediumint: datatype,
    bigint: datatype,
    int4: datatype,
    integer: datatype,
    time: datatype,
    timestamp: datatype,
    datetime: datatype,
    date: datatype,
    "boolean": datatype,
    decimal: function decimal(n) {
      var arg = recurse(Generator)(n.args);
      return "".concat(n.variant, "(").concat(arg, ")");
    },
    numeric: datatype,
    real: datatype,
    "float": datatype,
    "double precision": datatype,
    clob: function clob(n) {
      var arg = recurse(Generator)(n.args);
      return "".concat(n.variant, "(").concat(arg, ")");
    },
    longtext: datatype,
    mediumtext: datatype,
    tinytext: datatype,
    "char": function char(n) {
      var arg = recurse(Generator)(n.args);
      return "".concat(n.variant, "(").concat(arg, ")");
    },
    nvarchar: function nvarchar(n) {
      var arg = recurse(Generator)(n.args);
      return "".concat(n.variant, "(").concat(arg, ")");
    }
  }
};
module.exports = {
  version: require("./package.json").version,
  generate: function generate(n) {
    return Generator[n.type][n.variant](n);
  }
};