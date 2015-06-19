// Global context object and root of the language types used by the parser
var Blether = {
	"classes": {
		"Object": {
            "classMethods": {},
			"methods": {}
		}
	}
};

Blether.ParseError = function(err) {
	return {
		"name": "Blether Parse Error",
		"message": err.msg,
		"line": err.line,
		"column": err.column
	};
};

Blether.ParseError.prototype = new Error();
Blether.ParseError.prototype.toString = function() {
	return this.message + ":" + this.line + ":" + this.column;
};

Blether.Context = function() {
	this.currentClass = null;
	this.instanceVars = [];
	this.temps = [];
	this.returnContext = null;
};

Blether.ContextMgr = function() {
	var contextStack = [ new Blether.Context() ];
	contextStack[0].returnContext = "root";

	this.pushClass = function(className, instVars) {
		var newContext = new Blether.Context();
		newContext.className = className;
		newContext.instanceVars = instVars;
		this.push(newContext);
	};

	this.pushMethod = function(temps) {
		var newContext = new Blether.Context();
		newContext.returnContext = "method";
		newContext.temps = temps;
		this.push(newContext);
	};

	this.pushTemps = function(temps) {
		var newContext = new Blether.Context();
		newContext.temps = temps;
		this.push(newContext);
	};

	this.pushBlock = function(temps) {
		var newContext = new Blether.Context();
		newContext.returnContext = "block";
		newContext.temps = temps;
		this.push(newContext);
	};

	this.push = function(context) {
		contextStack.push(context);
	};

	this.pop = function() {
		if (contextStack.length === 1) {
			throw "Cannot pop the context beyond the initial state!";
		}
		contextStack.pop();
	};

	var receiverDepth = 0;

	this.pushReceiver = function() {
		receiverDepth++; // Not inlining in the next line for my sanity
		return "_receiver" + receiverDepth + "$";
	};

	this.popReceiver = function() {
		if (receiverDepth === 0) {
			throw "receiverDepth is already zero and cannot be popped further";
		}
		receiverDepth--;
	};

	this.getTemps = function() {
		return contextStack.reduceRight(function(prev, curr) {
			return prev.concat(curr.temps);
		}, []);
	};

	this.getInstanceVars = function() {
		return contextStack.reduceRight(function(prev, curr) {
			return prev.concat(curr.instanceVars);
		}, []);
	};

	this.currentClass = function() {
		for (var i = contextStack.length - 1; i >= 0; i--) {
			if (contextStack[i].className !== null) {
				return contextStack[i].className;
			}
		}

		return null;
	};

	this.returnContext = function() {
		for (var i = contextStack.length - 1; i >= 0; i--) {
			if (contextStack[i].returnContext !== null) {
				return contextStack[i].returnContext;
			}
		}

		return null;
	};
};


/* The language parts */

Blether.Node = function() {
	this.line = null;
	this.column = null;
	this.source = null;
};

Blether.Node.prototype.at = function(line, column, source) {
	this.line = line;
	this.column = column;
	this.source = source;

	return this;
};

/**
 * For most nodes, there is no different between visiting them and
 * invoking them. However, Blocks for example might choose to simplify
 * themselves if they know they going to be immediately invokes and are
 * sufficiently simple.
 */
Blether.Node.prototype.invoke = function() {
	return this.visit.apply(this, arguments);
};

Blether.Node.prototype.find = function(fn) {
	return fn(this) ? [this] : [];
};

Blether.Node.prototype.isMethod = function() { return false; };

//------------------------------------------------------------------------------

Blether.String = function(string) {
	this._type = "String";
	this.value = string;
};

Blether.String.prototype = new Blether.Node();
Blether.String.prototype.toString = function() { return this.value; };
Blether.String.prototype.visit = function(visitor) { return visitor.visitString(this); };

//------------------------------------------------------------------------------

Blether.Symbol = function(string) {
	this._type = "Symbol";
	this.value = string;
};

Blether.Symbol.prototype = new Blether.Node();
Blether.Symbol.prototype.toString = function() { return this.value; };
Blether.Symbol.prototype.visit = function(visitor) { return visitor.visitSymbol(this); };

//------------------------------------------------------------------------------

Blether.Number = function(string) {
	this._type = "Number";
	this.value = string;
};

Blether.Number.prototype = new Blether.Node();
Blether.Number.prototype.toString = function() { return this.value; };
Blether.Number.prototype.visit = function(visitor) { return visitor.visitNumber(this); };

//------------------------------------------------------------------------------

Blether.Array = function(array) {
	this._type = "Array";
	this.value = array;
};

Blether.Array.prototype = new Blether.Node();
Blether.Array.prototype.toString = function() {
	return "[" + this.value.map(function(each) { return "\"" + each + "\""; }).join(", ") + "]";
};
Blether.Array.prototype.visit = function(visitor) { return visitor.visitArray(this); };

//------------------------------------------------------------------------------

Blether.DynamicArray = function(array) {
	this._type = "DynamicArray";
	this.values = array || [];
};

Blether.DynamicArray.prototype = new Blether.Array();
Blether.DynamicArray.prototype.visit = function(visitor) { return visitor.visitDynamicArray(this); };

//------------------------------------------------------------------------------

Blether.DynamicDictionary = function(array) {
	this._type = "DynamicDictionary";
	this.values = array || [];
};

Blether.DynamicDictionary.prototype = new Blether.Node();
Blether.DynamicDictionary.prototype.visit = function(visitor) { return visitor.visitDynamicDictionary(this); };

//------------------------------------------------------------------------------

Blether.UndefinedObject = function() {
	this._type = "UndefinedObject";
};

Blether.UndefinedObject.prototype = new Blether.Node();
Blether.UndefinedObject.prototype.toString = function() { return "nil"; };
Blether.UndefinedObject.prototype.visit = function(visitor) { return visitor.visitUndefinedObject(this); };

//------------------------------------------------------------------------------

Blether.Boolean = function(bool) {
	this._type = "Boolean";
	this.value = bool;
};

Blether.Boolean.prototype = new Blether.Node();
Blether.Boolean.prototype.toString = function() { return this.value.toString(); };
Blether.Boolean.prototype.visit = function(visitor) { return visitor.visitBoolean(this); };

//------------------------------------------------------------------------------

Blether.Variable = function(id) {
	this._type = "Variable";
	this.value = id;
};

Blether.Variable.prototype = new Blether.Node();
Blether.Variable.prototype.toString = function() { return this.value; };
Blether.Variable.prototype.visit = function(visitor) { return visitor.visitVariable(this); };

//------------------------------------------------------------------------------

Blether.UnaryPattern = function(selector) {
	this._type = "UnaryPattern";
	this.selector = selector;
};

Blether.UnaryPattern.prototype = new Blether.Node();
Blether.UnaryPattern.prototype.toString = function() { return this.selector; };
Blether.UnaryPattern.prototype.visit = function(visitor) { return visitor.visitUnaryPattern(this); };

//------------------------------------------------------------------------------

Blether.BinaryPattern = function(selector, arg) {
	this._type = "BinaryPattern";
	this.selector = selector;
	this.arg = arg;
};

Blether.BinaryPattern.prototype = new Blether.Node();
Blether.BinaryPattern.prototype.toString = function() { return this.selector + " - " + this.arg; };
Blether.BinaryPattern.prototype.visit = function(visitor) { return visitor.visitBinaryPattern(this); };

//------------------------------------------------------------------------------

Blether.KeywordPattern = function(keywordPairs) {
	this._type = "KeywordPattern";
	this.pairs = keywordPairs;
	this.selector = "#";

	for(var i = 0; i < this.pairs.length; i++) {
		this.selector += this.pairs[i].key;
	}
};

Blether.KeywordPattern.prototype = new Blether.Node();
Blether.KeywordPattern.prototype.toString = function() { return this.selector; };
Blether.KeywordPattern.prototype.visit = function(visitor) { return visitor.visitKeywordPattern(this); };

//------------------------------------------------------------------------------

Blether.Assignment = function(variable, expression) {
	this._type = "Assignment";
	this.variable = variable;
	this.expression = expression;
};

Blether.Assignment.prototype = new Blether.Node();
Blether.Assignment.prototype.visit = function(visitor) { return visitor.visitAssignment(this); };

//------------------------------------------------------------------------------

Blether.Return = function(expression) {
	this._type = "Return";
	this.value = expression;
};

Blether.Return.prototype = new Blether.Node();
Blether.Return.prototype.visit = function(visitor) { return visitor.visitReturn(this); };

//------------------------------------------------------------------------------

Blether.Sequence = function(temps, statements) {
	this._type = "Sequence";
	this.temps      = temps      || [];
	this.statements = statements || [];
};

Blether.Sequence.prototype = new Blether.Node();
Blether.Sequence.prototype.visit = function(visitor) { return visitor.visitSequence(this); };

Blether.Sequence.prototype.find = function(fn) {
	var ret = [];
	if (fn(this)) {
		ret.push(this);
	}

	return this.statements.reduce(function(prev, curr) {
		return prev.concat(curr.find(fn));
	}, ret);
};

//------------------------------------------------------------------------------

Blether.Statement = function(expression) {
	this._type = "Statement";
	this.expression = expression;
};

Blether.Statement.prototype = new Blether.Node();
Blether.Statement.prototype.visit = function(visitor) { return visitor.visitStatement(this); };

Blether.Statement.prototype.find = function(fn) {
	var ret = [];
	if (fn(this)) {
		ret.push(this);
	}

	return ret.concat(this.expression.find(fn));
};

//------------------------------------------------------------------------------

Blether.Block = function(paramList, sequence) {
	this._type = "Block";
	this.params = paramList || [];
	this.sequence = sequence;
};

Blether.Block.prototype = new Blether.Node();
Blether.Block.prototype.visit = function(visitor) { return visitor.visitBlock(this); };
Blether.Block.prototype.invoke = function(visitor) {
	var invokeArgs = [this];
	for (var i = 1; i < arguments.length; i++) {
		invokeArgs.push(arguments[i]);
	}
	return visitor.invokeBlock.apply(visitor, invokeArgs);
};

Blether.Block.prototype.find = function(fn) {
	var ret = [];
	if (fn(this)) {
		ret.push(this);
	}

	return ret.concat(this.sequence.find(fn));
};

//------------------------------------------------------------------------------

Blether.Send = function(selector, args) {
	this._type = "Send";
	this.receiver = null;
	this.args = args || [];
	this.selector = selector;
};

Blether.Send.prototype = new Blether.Node();
Blether.Send.prototype.visit = function(visitor) {
	return visitor.visitSend(this);
};

Blether.Send.prototype.toString = function() {
	var argsStr = "[" + this.args.map(function(each){return "\"" + each + "\"";}).join(", ") + "]";
	return "Send(#" + this.selector +
		(this.received ? " to " + this.receiver : "") +
		(this.args.length > 0 ? " with args " + argsStr : "") + ")";
};

Blether.Send.prototype.setReceiver = function(anObject) {
	if (this.receiver === null) {
		this.receiver = anObject;
	}
	else {
		this.receiver.setReceiver(anObject);
	}
	return this;
};

Blether.Send.prototype.find = function(fn) {
	var ret = [];
	if (fn(this)) {
		ret.push(this);
	}

	ret = this.args.reduce(function(prev, curr) {
		return prev.concat(curr.find(fn));
	}, ret);

	return ret.concat(this.receiver.find(fn));
};

//------------------------------------------------------------------------------

Blether.Cascade = function(receiver, messages) {
	this._type = "Cascade";
	this.receiver = receiver;
	this.messages = messages || [];
};

Blether.Cascade.prototype = new Blether.Node();
Blether.Cascade.prototype.visit = function(visitor) { return visitor.visitCascade(this); };

//------------------------------------------------------------------------------

Blether.JsStatement = function(javascript) {
	this._type = "JsStatement";
	this.javascript = javascript;
};

Blether.JsStatement.prototype = new Blether.Node();
Blether.JsStatement.prototype.visit = function(visitor) { return visitor.visitJsStatement(this); };

//------------------------------------------------------------------------------

Blether.Method = function(selector, sequence) {
	this._type = "Method";
	this.selector = selector;
	this.sequence = sequence;
};

Blether.Method.prototype = new Blether.Node();
Blether.Method.prototype.visit = function(visitor) { return visitor.visitMethod(this); };

Blether.Method.prototype.find = function(fn) {
	var ret = [];
	if (fn(this)) {
		ret.push(this);
	}

	return ret.concat(this.sequence.find(fn));
};

//------------------------------------------------------------------------------

Blether.Comment = function(comment) {
	this._type = "Comment";
	this.comment = comment;
};

Blether.Comment.prototype = new Blether.Node();
Blether.Comment.prototype.visit = function(visitor) { return visitor.visitComment(this); };

//------------------------------------------------------------------------------

Blether.MethodDeclaration = function(className, body) {
	this._type = "MethodDeclaration";
	this.className = className;
	this.body = body;
	this.context = Blether;

	this.context.classes[className].methods[body.selector] = this;
};

Blether.MethodDeclaration.prototype = new Blether.Node();
Blether.MethodDeclaration.prototype.visit = function(visitor) {
	return visitor.visitMethodDeclaration(this);
};

Blether.MethodDeclaration.prototype.getClass = function() {
	return this.context.classes[this.className];
};

Blether.MethodDeclaration.prototype.find = function(fn) {
	var ret = [];
	if (fn(this)) {
		ret.push(this);
	}

	return ret.concat(this.body.find(fn));
};

Blether.MethodDeclaration.prototype.isMethod = function() { return true; };

//------------------------------------------------------------------------------

Blether.ClassMethodDeclaration = function(className, body) {
	this._type = "ClassMethodDeclaration";
	this.className = className;
	this.body = body;
	this.context = Blether;

	this.context.classes[className].classMethods[body.selector] = this;
};

Blether.ClassMethodDeclaration.prototype = new Blether.Node();
Blether.ClassMethodDeclaration.prototype.visit = function(visitor) {
	return visitor.visitClassMethodDeclaration(this);
};

Blether.ClassMethodDeclaration.prototype.getClass = function() {
	return this.context.classes[this.className];
};

Blether.ClassMethodDeclaration.prototype.find = function(fn) {
	var ret = [];
	if (fn(this)) {
		ret.push(this);
	}

	return ret.concat(this.body.find(fn));
};

Blether.ClassMethodDeclaration.prototype.isMethod = function() { return true; };

//------------------------------------------------------------------------------

Blether.ClassDeclaration = function(className, superClass, varNames) {
	this._type = "ClassDeclaration";
	this.className = className;
	this.superClass = superClass;
	this.varNames = varNames;
	this.classMethods = {};
	this.methods = {};
	this.context = Blether;
};

Blether.ClassDeclaration.prototype = new Blether.Node();
Blether.ClassDeclaration.prototype.visit = function(visitor) { return visitor.visitClassDeclaration(this); };
Blether.ClassDeclaration.prototype.getMethods = function() {
	return this.context.classes[this.className].methods;
};
Blether.ClassDeclaration.prototype.getClassMethods = function() {
	return this.context.classes[this.className].classMethods;
};

//------------------------------------------------------------------------------

Blether.Program = function(elements) {
	this._type = "Program";
	this.elements = elements;
};

Blether.Program.prototype = new Blether.Node();
Blether.Program.prototype.visit = function(visitor) { return visitor.visitProgram(this); };

Blether.Program.prototype.find = function(fn) {
	var ret = [];
	if (fn(this)) {
		ret.push(this);
	}

	return this.elements.reduce(function(prev, curr) {
		return prev.concat(curr.find(fn));
	}, ret);
};

//------------------------------------------------------------------------------

Blether.VariableDeclaration = function(vars) {
	this._type = "VariableDeclaration";
	this.variables = vars;
};

Blether.VariableDeclaration.prototype = new Blether.Node();
Blether.VariableDeclaration.prototype.visit = function(visitor) { return visitor.visitVariableDeclaration(this); };



var BletherParser = (function() {
  /*
   * Generated by PEG.js 0.8.0.
   *
   * http://pegjs.majda.cz/
   */

  function peg$subclass(child, parent) {
    function ctor() { this.constructor = child; }
    ctor.prototype = parent.prototype;
    child.prototype = new ctor();
  }

  function SyntaxError(message, expected, found, offset, line, column) {
    this.message  = message;
    this.expected = expected;
    this.found    = found;
    this.offset   = offset;
    this.line     = line;
    this.column   = column;

    this.name     = "SyntaxError";
  }

  peg$subclass(SyntaxError, Error);

  function parse(input) {
    var options = arguments.length > 1 ? arguments[1] : {},

        peg$FAILED = {},

        peg$startRuleFunctions = { start: peg$parsestart },
        peg$startRuleFunction  = peg$parsestart,

        peg$c0 = [],
        peg$c1 = peg$FAILED,
        peg$c2 = /^[ \t\x0B\f\xA0\uFEFF\n\r\u2028\u2029]/,
        peg$c3 = { type: "class", value: "[ \\t\\x0B\\f\\xA0\\uFEFF\\n\\r\\u2028\\u2029]", description: "[ \\t\\x0B\\f\\xA0\\uFEFF\\n\\r\\u2028\\u2029]" },
        peg$c4 = "\"",
        peg$c5 = { type: "literal", value: "\"", description: "\"\\\"\"" },
        peg$c6 = "\"\"",
        peg$c7 = { type: "literal", value: "\"\"", description: "\"\\\"\\\"\"" },
        peg$c8 = function() {return '"'},
        peg$c9 = /^[^"]/,
        peg$c10 = { type: "class", value: "[^\"]", description: "[^\"]" },
        peg$c11 = /^[a-zA-Z]/,
        peg$c12 = { type: "class", value: "[a-zA-Z]", description: "[a-zA-Z]" },
        peg$c13 = /^[a-zA-Z0-9]/,
        peg$c14 = { type: "class", value: "[a-zA-Z0-9]", description: "[a-zA-Z0-9]" },
        peg$c15 = function(first, others) {
        	return first + others.join("");
        },
        peg$c16 = ":",
        peg$c17 = { type: "literal", value: ":", description: "\":\"" },
        peg$c18 = function(first, last) {return first + last;},
        peg$c19 = /^[a-zA-Z0-9:]/,
        peg$c20 = { type: "class", value: "[a-zA-Z0-9:]", description: "[a-zA-Z0-9:]" },
        peg$c21 = function(first, others) {return first + others.join("");},
        peg$c22 = /^[A-Z]/,
        peg$c23 = { type: "class", value: "[A-Z]", description: "[A-Z]" },
        peg$c24 = "'",
        peg$c25 = { type: "literal", value: "'", description: "\"'\"" },
        peg$c26 = "''",
        peg$c27 = { type: "literal", value: "''", description: "\"''\"" },
        peg$c28 = function() {return "'";},
        peg$c29 = /^[^']/,
        peg$c30 = { type: "class", value: "[^']", description: "[^']" },
        peg$c31 = function(val) {
        	return new Blether.String(val.join("")).at(line(), column(), text());
        },
        peg$c32 = "$",
        peg$c33 = { type: "literal", value: "$", description: "\"$\"" },
        peg$c34 = { type: "any", description: "any character" },
        peg$c35 = function(char) {
        	return new Blether.String(char).at(line(), column(), text());
        },
        peg$c36 = "#",
        peg$c37 = { type: "literal", value: "#", description: "\"#\"" },
        peg$c38 = function(rest) {return rest;},
        peg$c39 = function(node) {return node.value;},
        peg$c40 = function(val) {
        	return new Blether.Symbol(val).at(line(), column(), text());
        },
        peg$c41 = function(val) {
        	return new Blether.Number(val).at(line(), column(), text());
        },
        peg$c42 = "e",
        peg$c43 = { type: "literal", value: "e", description: "\"e\"" },
        peg$c44 = function(n) {return parseFloat(n.join(""));},
        peg$c45 = null,
        peg$c46 = "-",
        peg$c47 = { type: "literal", value: "-", description: "\"-\"" },
        peg$c48 = "16r",
        peg$c49 = { type: "literal", value: "16r", description: "\"16r\"" },
        peg$c50 = /^[0-9a-fA-F]/,
        peg$c51 = { type: "class", value: "[0-9a-fA-F]", description: "[0-9a-fA-F]" },
        peg$c52 = function(neg, num) {return parseInt(((neg || '') + num.join("")), 16);},
        peg$c53 = /^[0-9]/,
        peg$c54 = { type: "class", value: "[0-9]", description: "[0-9]" },
        peg$c55 = ".",
        peg$c56 = { type: "literal", value: ".", description: "\".\"" },
        peg$c57 = function(neg, digits, dec) {return parseFloat(((neg || '') + digits.join("") + "." + dec.join("")), 10);},
        peg$c58 = function(neg, digits) {return (parseInt((neg || '') + digits.join(""), 10));},
        peg$c59 = "#(",
        peg$c60 = { type: "literal", value: "#(", description: "\"#(\"" },
        peg$c61 = "(",
        peg$c62 = { type: "literal", value: "(", description: "\"(\"" },
        peg$c63 = function(lit) {return lit; },
        peg$c64 = ")",
        peg$c65 = { type: "literal", value: ")", description: "\")\"" },
        peg$c66 = function(lits) {
        	return new Blether.Array(lits).at(line(), column(), text());
        },
        peg$c67 = function(sym) { return sym; },
        peg$c68 = function(syms) {
        	return new Blether.Array(syms).at(line(), column(), text());
        },
        peg$c69 = "{",
        peg$c70 = { type: "literal", value: "{", description: "\"{\"" },
        peg$c71 = "}",
        peg$c72 = { type: "literal", value: "}", description: "\"}\"" },
        peg$c73 = function(expressions) {
        	return new Blether.DynamicArray(expressions).at(line(), column(), text());
        },
        peg$c74 = "#{",
        peg$c75 = { type: "literal", value: "#{", description: "\"#{\"" },
        peg$c76 = function(expressions) {
        	return new Blether.DynamicDictionary(expressions).at(line(), column(), text());
        },
        peg$c77 = "true",
        peg$c78 = { type: "literal", value: "true", description: "\"true\"" },
        peg$c79 = function() {return true;},
        peg$c80 = "false",
        peg$c81 = { type: "literal", value: "false", description: "\"false\"" },
        peg$c82 = function() {return false;},
        peg$c83 = function(val) {
        	return new Blether.Boolean(val).at(line(), column(), text());
        },
        peg$c84 = "nil",
        peg$c85 = { type: "literal", value: "nil", description: "\"nil\"" },
        peg$c86 = function(val) {
        	return new Blether.UndefinedObject().at(line(), column(), text());
        },
        peg$c87 = function(identifier) {
        	if (identifier === "this") {
        		throw Blether.ParseError({
        			"line": line(),
        			"column": column(),
        			"msg": "Use [self] instead of [this]"
        		});
        	}

        	return new Blether.Variable(identifier).at(line(), column(), text());
        },
        peg$c88 = function(key, arg) {return {key:key, arg:arg};},
        peg$c89 = /^[\\+*\/=><,@%~|&\-]/,
        peg$c90 = { type: "class", value: "[\\\\+*\\/=><,@%~|&\\-]", description: "[\\\\+*\\/=><,@%~|&\\-]" },
        peg$c91 = function(bin) { return bin.join(""); },
        peg$c92 = function(pairs) {
        	return new Blether.KeywordPattern(pairs).at(line(), column(), text());
        },
        peg$c93 = function(selector, arg) {
        	return new Blether.BinaryPattern(selector, arg).at(line(), column(), text());
        },
        peg$c94 = function(selector) {
        	return new Blether.UnaryPattern(selector).at(line(), column(), text());
        },
        peg$c95 = function(chain) { return chain },
        peg$c96 = function(message) {
        	if (chain) {
        		return chain.setReceiver(message);
        	}
        	else {
        		return new Blether.send(message).at(line(), column(), text());
        	}
        },
        peg$c97 = function(mess) { return mess },
        peg$c98 = function(send, message) {
        	if (message) {
        		return message.setReceiver(send);
        	}
        	else {
        		return send;
        	}
        },
        peg$c99 = function(expression) {return expression;},
        peg$c100 = function(first, others) { return [first].concat(others); },
        peg$c101 = ":=",
        peg$c102 = { type: "literal", value: ":=", description: "\":=\"" },
        peg$c103 = function(variable, expression) {
        	return new Blether.Assignment(variable, expression).at(line(), column(), text());
        },
        peg$c104 = "^",
        peg$c105 = { type: "literal", value: "^", description: "\"^\"" },
        peg$c106 = function(expression) {
        	return new Blether.Return(expression).at(line(), column(), text());
        },
        peg$c107 = "|",
        peg$c108 = { type: "literal", value: "|", description: "\"|\"" },
        peg$c109 = function(variable) {return variable;},
        peg$c110 = function(vars) {

        	vars.forEach(function(each) {
        		if (each === "self" || each === "this") {
        			throw Blether.ParseError({
        				"line": line(),
        				"column": column(),
        				"msg": "Cannot name a variable [" + each + "]"
        			});
        		}
        	});

        	return vars;
        },
        peg$c111 = function(param) {return param;},
        peg$c112 = function(params) {return params;},
        peg$c113 = function(ret) {return [ret];},
        peg$c114 = function(exps, ret) {
        	var expressions = exps;
        	expressions.push(ret);
        	return expressions;
        },
        peg$c115 = function(expressions) {
        	return expressions || [];
        },
        peg$c116 = function(e) { return e },
        peg$c117 = function(expr) {
        	return new Blether.Statement(expr).at(line(), column(), text());
        },
        peg$c118 = function(temps, statements) {
        	return new Blether.Sequence(temps, statements).at(line(), column(), text());
        },
        peg$c119 = "[",
        peg$c120 = { type: "literal", value: "[", description: "\"[\"" },
        peg$c121 = "]",
        peg$c122 = { type: "literal", value: "]", description: "\"]\"" },
        peg$c123 = function(params, sequence) {
        	return new Blether.Block(params, sequence).at(line(), column(), text());
        },
        peg$c124 = void 0,
        peg$c125 = function(selector) {
        	return new Blether.Send(selector).at(line(), column(), text());
        },
        peg$c126 = function(message, tail) {
        	if (tail) {
        		return tail.setReceiver(message);
        	}
        	else {
        		return message;
        	}
        },
        peg$c127 = function(receiver, tail) {
        	if (tail) {
        		return tail.setReceiver(receiver);
        	}
        	else {
        		return receiver;
        	}
        },
        peg$c128 = function(selector, arg) {
        	return new Blether.Send(selector, [arg]).at(line(), column(), text());
        },
        peg$c129 = function(pairs) {
        	var selector = [];
        	var args = [];
        	for(var i = 0; i < pairs.length; i++) {
        		selector.push(pairs[i].key);
        		args.push(pairs[i].arg);
        	}

        	return new Blether.Send(selector.join(""), args).at(line(), column(), text());
        },
        peg$c130 = function(receiver, tail) {
        	return tail.setReceiver(receiver);
        },
        peg$c131 = ";",
        peg$c132 = { type: "literal", value: ";", description: "\";\"" },
        peg$c133 = function(mess) {return mess;},
        peg$c134 = function(send, cascade) {
        	return new Blether.Cascade(send.receiver, [send].concat(cascade)).at(line(), column(), text());
        },
        peg$c135 = "<",
        peg$c136 = { type: "literal", value: "<", description: "\"<\"" },
        peg$c137 = ">>",
        peg$c138 = { type: "literal", value: ">>", description: "\">>\"" },
        peg$c139 = function() {return ">";},
        peg$c140 = /^[^>]/,
        peg$c141 = { type: "class", value: "[^>]", description: "[^>]" },
        peg$c142 = ">",
        peg$c143 = { type: "literal", value: ">", description: "\">\"" },
        peg$c144 = function(val) {
        	return new Blether.JsStatement(val.join("")).at(line(), column(), text());
        },
        peg$c145 = function(pattern, sequence) {
        	return new Blether.Method(pattern, sequence).at(line(), column(), text());
        },
        peg$c146 = function(send) { return send.selector === "->" },
        peg$c147 = function(send) {
        	return [send.receiver, send.args[0]];
        },
        peg$c148 = function(first, others) {
        	return first.concat.apply(first, others);
        },
        peg$c149 = "subclass:",
        peg$c150 = { type: "literal", value: "subclass:", description: "\"subclass:\"" },
        peg$c151 = "variables:",
        peg$c152 = { type: "literal", value: "variables:", description: "\"variables:\"" },
        peg$c153 = "instanceVariableNames:",
        peg$c154 = { type: "literal", value: "instanceVariableNames:", description: "\"instanceVariableNames:\"" },
        peg$c155 = "classVariableNames:",
        peg$c156 = { type: "literal", value: "classVariableNames:", description: "\"classVariableNames:\"" },
        peg$c157 = "poolDictionaries:",
        peg$c158 = { type: "literal", value: "poolDictionaries:", description: "\"poolDictionaries:\"" },
        peg$c159 = function(superClass, className, varNames) {
        	if (!className.value.match(/^[A-Z]/)) {
        		throw Blether.ParseError({
        			"line": line(),
        			"column": column(),
        			"msg": "Class name " + className + " must start with a capital"
        		});
        	}

        	/*
        	if (!Blether.classes[superClass]) {
        		throw Blether.ParseError({
        			"line": line(),
        			"column": column(),
        			"msg": "Super-class " + superClass + " not defined"
        		});
        	}
        	*/

        	if (Blether.classes[className]) {
        		throw Blether.ParseError({
        			"line": line(),
        			"column": column(),
        			"msg": "Class " + className + " already defined"
        		});
        	}

        	return Blether.classes[className] = new Blether.ClassDeclaration(
        		className,
        		superClass,
        		varNames).at(line(), column(), text());
        },
        peg$c160 = "!",
        peg$c161 = { type: "literal", value: "!", description: "\"!\"" },
        peg$c162 = "class",
        peg$c163 = { type: "literal", value: "class", description: "\"class\"" },
        peg$c164 = function(className, classMethod, body) {

        	if (className !== "Object" && !Blether.classes[className]) {
        		throw Blether.ParseError({
        			"line": line(),
        			"column": column(),
        			"msg": "Unknown class " + className
        		});
        	}

            if (classMethod) {
                if (Blether.classes[className].classMethods.hasOwnProperty(body.selector)) {
                    throw Blether.ParseError({
                        "line": line(),
                        "column": column(),
                        "msg": "Class method " + body.selector + " already defined on class " + className
                    });
                }

                return Blether.classes[className].classMethods[body.selector] =
                    new Blether.ClassMethodDeclaration(className, body).at(line(), column(), text());
            }

        	if (Blether.classes[className].methods.hasOwnProperty(body.selector)) {
        		throw Blether.ParseError({
        			"line": line(),
        			"column": column(),
        			"msg": "Method " + body.selector + " already defined on class " + className
        		});
        	}

        	return Blether.classes[className].methods[body.selector] =
        		new Blether.MethodDeclaration(className, body).at(line(), column(), text());
        },
        peg$c165 = function(element) {
        	return element
        },
        peg$c166 = function(decl) { return new Blether.VariableDeclaration(decl); },
        peg$c167 = function(first, others) {
        	return new Blether.Program([first].concat(others)).at(line(), column(), text());
        },

        peg$currPos          = 0,
        peg$reportedPos      = 0,
        peg$cachedPos        = 0,
        peg$cachedPosDetails = { line: 1, column: 1, seenCR: false },
        peg$maxFailPos       = 0,
        peg$maxFailExpected  = [],
        peg$silentFails      = 0,

        peg$result;

    if ("startRule" in options) {
      if (!(options.startRule in peg$startRuleFunctions)) {
        throw new Error("Can't start parsing from rule \"" + options.startRule + "\".");
      }

      peg$startRuleFunction = peg$startRuleFunctions[options.startRule];
    }

    function text() {
      return input.substring(peg$reportedPos, peg$currPos);
    }

    function offset() {
      return peg$reportedPos;
    }

    function line() {
      return peg$computePosDetails(peg$reportedPos).line;
    }

    function column() {
      return peg$computePosDetails(peg$reportedPos).column;
    }

    function expected(description) {
      throw peg$buildException(
        null,
        [{ type: "other", description: description }],
        peg$reportedPos
      );
    }

    function error(message) {
      throw peg$buildException(message, null, peg$reportedPos);
    }

    function peg$computePosDetails(pos) {
      function advance(details, startPos, endPos) {
        var p, ch;

        for (p = startPos; p < endPos; p++) {
          ch = input.charAt(p);
          if (ch === "\n") {
            if (!details.seenCR) { details.line++; }
            details.column = 1;
            details.seenCR = false;
          } else if (ch === "\r" || ch === "\u2028" || ch === "\u2029") {
            details.line++;
            details.column = 1;
            details.seenCR = true;
          } else {
            details.column++;
            details.seenCR = false;
          }
        }
      }

      if (peg$cachedPos !== pos) {
        if (peg$cachedPos > pos) {
          peg$cachedPos = 0;
          peg$cachedPosDetails = { line: 1, column: 1, seenCR: false };
        }
        advance(peg$cachedPosDetails, peg$cachedPos, pos);
        peg$cachedPos = pos;
      }

      return peg$cachedPosDetails;
    }

    function peg$fail(expected) {
      if (peg$currPos < peg$maxFailPos) { return; }

      if (peg$currPos > peg$maxFailPos) {
        peg$maxFailPos = peg$currPos;
        peg$maxFailExpected = [];
      }

      peg$maxFailExpected.push(expected);
    }

    function peg$buildException(message, expected, pos) {
      function cleanupExpected(expected) {
        var i = 1;

        expected.sort(function(a, b) {
          if (a.description < b.description) {
            return -1;
          } else if (a.description > b.description) {
            return 1;
          } else {
            return 0;
          }
        });

        while (i < expected.length) {
          if (expected[i - 1] === expected[i]) {
            expected.splice(i, 1);
          } else {
            i++;
          }
        }
      }

      function buildMessage(expected, found) {
        function stringEscape(s) {
          function hex(ch) { return ch.charCodeAt(0).toString(16).toUpperCase(); }

          return s
            .replace(/\\/g,   '\\\\')
            .replace(/"/g,    '\\"')
            .replace(/\x08/g, '\\b')
            .replace(/\t/g,   '\\t')
            .replace(/\n/g,   '\\n')
            .replace(/\f/g,   '\\f')
            .replace(/\r/g,   '\\r')
            .replace(/[\x00-\x07\x0B\x0E\x0F]/g, function(ch) { return '\\x0' + hex(ch); })
            .replace(/[\x10-\x1F\x80-\xFF]/g,    function(ch) { return '\\x'  + hex(ch); })
            .replace(/[\u0180-\u0FFF]/g,         function(ch) { return '\\u0' + hex(ch); })
            .replace(/[\u1080-\uFFFF]/g,         function(ch) { return '\\u'  + hex(ch); });
        }

        var expectedDescs = new Array(expected.length),
            expectedDesc, foundDesc, i;

        for (i = 0; i < expected.length; i++) {
          expectedDescs[i] = expected[i].description;
        }

        expectedDesc = expected.length > 1
          ? expectedDescs.slice(0, -1).join(", ")
              + " or "
              + expectedDescs[expected.length - 1]
          : expectedDescs[0];

        foundDesc = found ? "\"" + stringEscape(found) + "\"" : "end of input";

        return "Expected " + expectedDesc + " but " + foundDesc + " found.";
      }

      var posDetails = peg$computePosDetails(pos),
          found      = pos < input.length ? input.charAt(pos) : null;

      if (expected !== null) {
        cleanupExpected(expected);
      }

      return new SyntaxError(
        message !== null ? message : buildMessage(expected, found),
        expected,
        found,
        pos,
        posDetails.line,
        posDetails.column
      );
    }

    function peg$parsestart() {
      var s0;

      s0 = peg$parseprogram();

      return s0;
    }

    function peg$parseseparator() {
      var s0, s1;

      s0 = [];
      if (peg$c2.test(input.charAt(peg$currPos))) {
        s1 = input.charAt(peg$currPos);
        peg$currPos++;
      } else {
        s1 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c3); }
      }
      if (s1 !== peg$FAILED) {
        while (s1 !== peg$FAILED) {
          s0.push(s1);
          if (peg$c2.test(input.charAt(peg$currPos))) {
            s1 = input.charAt(peg$currPos);
            peg$currPos++;
          } else {
            s1 = peg$FAILED;
            if (peg$silentFails === 0) { peg$fail(peg$c3); }
          }
        }
      } else {
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parsecomments() {
      var s0, s1, s2, s3, s4;

      s0 = peg$currPos;
      if (input.charCodeAt(peg$currPos) === 34) {
        s1 = peg$c4;
        peg$currPos++;
      } else {
        s1 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c5); }
      }
      if (s1 !== peg$FAILED) {
        s2 = [];
        s3 = peg$currPos;
        if (input.substr(peg$currPos, 2) === peg$c6) {
          s4 = peg$c6;
          peg$currPos += 2;
        } else {
          s4 = peg$FAILED;
          if (peg$silentFails === 0) { peg$fail(peg$c7); }
        }
        if (s4 !== peg$FAILED) {
          peg$reportedPos = s3;
          s4 = peg$c8();
        }
        s3 = s4;
        if (s3 === peg$FAILED) {
          if (peg$c9.test(input.charAt(peg$currPos))) {
            s3 = input.charAt(peg$currPos);
            peg$currPos++;
          } else {
            s3 = peg$FAILED;
            if (peg$silentFails === 0) { peg$fail(peg$c10); }
          }
        }
        while (s3 !== peg$FAILED) {
          s2.push(s3);
          s3 = peg$currPos;
          if (input.substr(peg$currPos, 2) === peg$c6) {
            s4 = peg$c6;
            peg$currPos += 2;
          } else {
            s4 = peg$FAILED;
            if (peg$silentFails === 0) { peg$fail(peg$c7); }
          }
          if (s4 !== peg$FAILED) {
            peg$reportedPos = s3;
            s4 = peg$c8();
          }
          s3 = s4;
          if (s3 === peg$FAILED) {
            if (peg$c9.test(input.charAt(peg$currPos))) {
              s3 = input.charAt(peg$currPos);
              peg$currPos++;
            } else {
              s3 = peg$FAILED;
              if (peg$silentFails === 0) { peg$fail(peg$c10); }
            }
          }
        }
        if (s2 !== peg$FAILED) {
          if (input.charCodeAt(peg$currPos) === 34) {
            s3 = peg$c4;
            peg$currPos++;
          } else {
            s3 = peg$FAILED;
            if (peg$silentFails === 0) { peg$fail(peg$c5); }
          }
          if (s3 !== peg$FAILED) {
            s1 = [s1, s2, s3];
            s0 = s1;
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parsews() {
      var s0, s1;

      s0 = [];
      s1 = peg$parseseparator();
      if (s1 === peg$FAILED) {
        s1 = peg$parsecomments();
      }
      while (s1 !== peg$FAILED) {
        s0.push(s1);
        s1 = peg$parseseparator();
        if (s1 === peg$FAILED) {
          s1 = peg$parsecomments();
        }
      }

      return s0;
    }

    function peg$parseidentifier() {
      var s0, s1, s2, s3;

      s0 = peg$currPos;
      if (peg$c11.test(input.charAt(peg$currPos))) {
        s1 = input.charAt(peg$currPos);
        peg$currPos++;
      } else {
        s1 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c12); }
      }
      if (s1 !== peg$FAILED) {
        s2 = [];
        if (peg$c13.test(input.charAt(peg$currPos))) {
          s3 = input.charAt(peg$currPos);
          peg$currPos++;
        } else {
          s3 = peg$FAILED;
          if (peg$silentFails === 0) { peg$fail(peg$c14); }
        }
        while (s3 !== peg$FAILED) {
          s2.push(s3);
          if (peg$c13.test(input.charAt(peg$currPos))) {
            s3 = input.charAt(peg$currPos);
            peg$currPos++;
          } else {
            s3 = peg$FAILED;
            if (peg$silentFails === 0) { peg$fail(peg$c14); }
          }
        }
        if (s2 !== peg$FAILED) {
          peg$reportedPos = s0;
          s1 = peg$c15(s1, s2);
          s0 = s1;
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parsekeyword() {
      var s0, s1, s2;

      s0 = peg$currPos;
      s1 = peg$parseidentifier();
      if (s1 !== peg$FAILED) {
        if (input.charCodeAt(peg$currPos) === 58) {
          s2 = peg$c16;
          peg$currPos++;
        } else {
          s2 = peg$FAILED;
          if (peg$silentFails === 0) { peg$fail(peg$c17); }
        }
        if (s2 !== peg$FAILED) {
          peg$reportedPos = s0;
          s1 = peg$c18(s1, s2);
          s0 = s1;
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parseselector() {
      var s0, s1, s2, s3;

      s0 = peg$currPos;
      if (peg$c11.test(input.charAt(peg$currPos))) {
        s1 = input.charAt(peg$currPos);
        peg$currPos++;
      } else {
        s1 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c12); }
      }
      if (s1 !== peg$FAILED) {
        s2 = [];
        if (peg$c19.test(input.charAt(peg$currPos))) {
          s3 = input.charAt(peg$currPos);
          peg$currPos++;
        } else {
          s3 = peg$FAILED;
          if (peg$silentFails === 0) { peg$fail(peg$c20); }
        }
        while (s3 !== peg$FAILED) {
          s2.push(s3);
          if (peg$c19.test(input.charAt(peg$currPos))) {
            s3 = input.charAt(peg$currPos);
            peg$currPos++;
          } else {
            s3 = peg$FAILED;
            if (peg$silentFails === 0) { peg$fail(peg$c20); }
          }
        }
        if (s2 !== peg$FAILED) {
          peg$reportedPos = s0;
          s1 = peg$c21(s1, s2);
          s0 = s1;
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parseclassName() {
      var s0, s1, s2, s3;

      s0 = peg$currPos;
      if (peg$c22.test(input.charAt(peg$currPos))) {
        s1 = input.charAt(peg$currPos);
        peg$currPos++;
      } else {
        s1 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c23); }
      }
      if (s1 !== peg$FAILED) {
        s2 = [];
        if (peg$c13.test(input.charAt(peg$currPos))) {
          s3 = input.charAt(peg$currPos);
          peg$currPos++;
        } else {
          s3 = peg$FAILED;
          if (peg$silentFails === 0) { peg$fail(peg$c14); }
        }
        while (s3 !== peg$FAILED) {
          s2.push(s3);
          if (peg$c13.test(input.charAt(peg$currPos))) {
            s3 = input.charAt(peg$currPos);
            peg$currPos++;
          } else {
            s3 = peg$FAILED;
            if (peg$silentFails === 0) { peg$fail(peg$c14); }
          }
        }
        if (s2 !== peg$FAILED) {
          peg$reportedPos = s0;
          s1 = peg$c21(s1, s2);
          s0 = s1;
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parsestring() {
      var s0, s1, s2, s3, s4;

      s0 = peg$currPos;
      if (input.charCodeAt(peg$currPos) === 39) {
        s1 = peg$c24;
        peg$currPos++;
      } else {
        s1 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c25); }
      }
      if (s1 !== peg$FAILED) {
        s2 = [];
        s3 = peg$currPos;
        if (input.substr(peg$currPos, 2) === peg$c26) {
          s4 = peg$c26;
          peg$currPos += 2;
        } else {
          s4 = peg$FAILED;
          if (peg$silentFails === 0) { peg$fail(peg$c27); }
        }
        if (s4 !== peg$FAILED) {
          peg$reportedPos = s3;
          s4 = peg$c28();
        }
        s3 = s4;
        if (s3 === peg$FAILED) {
          if (peg$c29.test(input.charAt(peg$currPos))) {
            s3 = input.charAt(peg$currPos);
            peg$currPos++;
          } else {
            s3 = peg$FAILED;
            if (peg$silentFails === 0) { peg$fail(peg$c30); }
          }
        }
        while (s3 !== peg$FAILED) {
          s2.push(s3);
          s3 = peg$currPos;
          if (input.substr(peg$currPos, 2) === peg$c26) {
            s4 = peg$c26;
            peg$currPos += 2;
          } else {
            s4 = peg$FAILED;
            if (peg$silentFails === 0) { peg$fail(peg$c27); }
          }
          if (s4 !== peg$FAILED) {
            peg$reportedPos = s3;
            s4 = peg$c28();
          }
          s3 = s4;
          if (s3 === peg$FAILED) {
            if (peg$c29.test(input.charAt(peg$currPos))) {
              s3 = input.charAt(peg$currPos);
              peg$currPos++;
            } else {
              s3 = peg$FAILED;
              if (peg$silentFails === 0) { peg$fail(peg$c30); }
            }
          }
        }
        if (s2 !== peg$FAILED) {
          if (input.charCodeAt(peg$currPos) === 39) {
            s3 = peg$c24;
            peg$currPos++;
          } else {
            s3 = peg$FAILED;
            if (peg$silentFails === 0) { peg$fail(peg$c25); }
          }
          if (s3 !== peg$FAILED) {
            peg$reportedPos = s0;
            s1 = peg$c31(s2);
            s0 = s1;
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parsecharacter() {
      var s0, s1, s2;

      s0 = peg$currPos;
      if (input.charCodeAt(peg$currPos) === 36) {
        s1 = peg$c32;
        peg$currPos++;
      } else {
        s1 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c33); }
      }
      if (s1 !== peg$FAILED) {
        if (input.length > peg$currPos) {
          s2 = input.charAt(peg$currPos);
          peg$currPos++;
        } else {
          s2 = peg$FAILED;
          if (peg$silentFails === 0) { peg$fail(peg$c34); }
        }
        if (s2 !== peg$FAILED) {
          peg$reportedPos = s0;
          s1 = peg$c35(s2);
          s0 = s1;
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parsesymbol() {
      var s0, s1, s2;

      s0 = peg$currPos;
      if (input.charCodeAt(peg$currPos) === 35) {
        s1 = peg$c36;
        peg$currPos++;
      } else {
        s1 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c37); }
      }
      if (s1 !== peg$FAILED) {
        s2 = peg$parsebareSymbol();
        if (s2 !== peg$FAILED) {
          peg$reportedPos = s0;
          s1 = peg$c38(s2);
          s0 = s1;
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parsebareSymbol() {
      var s0, s1, s2;

      s0 = peg$currPos;
      s1 = peg$parseselector();
      if (s1 === peg$FAILED) {
        s1 = peg$parsebinarySelector();
        if (s1 === peg$FAILED) {
          s1 = peg$currPos;
          s2 = peg$parsestring();
          if (s2 !== peg$FAILED) {
            peg$reportedPos = s1;
            s2 = peg$c39(s2);
          }
          s1 = s2;
        }
      }
      if (s1 !== peg$FAILED) {
        peg$reportedPos = s0;
        s1 = peg$c40(s1);
      }
      s0 = s1;

      return s0;
    }

    function peg$parsenumber() {
      var s0, s1;

      s0 = peg$currPos;
      s1 = peg$parsenumberExp();
      if (s1 === peg$FAILED) {
        s1 = peg$parsehex();
        if (s1 === peg$FAILED) {
          s1 = peg$parsefloat();
          if (s1 === peg$FAILED) {
            s1 = peg$parseinteger();
          }
        }
      }
      if (s1 !== peg$FAILED) {
        peg$reportedPos = s0;
        s1 = peg$c41(s1);
      }
      s0 = s1;

      return s0;
    }

    function peg$parsenumberExp() {
      var s0, s1, s2, s3, s4;

      s0 = peg$currPos;
      s1 = peg$currPos;
      s2 = peg$parsefloat();
      if (s2 === peg$FAILED) {
        s2 = peg$parseinteger();
      }
      if (s2 !== peg$FAILED) {
        if (input.charCodeAt(peg$currPos) === 101) {
          s3 = peg$c42;
          peg$currPos++;
        } else {
          s3 = peg$FAILED;
          if (peg$silentFails === 0) { peg$fail(peg$c43); }
        }
        if (s3 !== peg$FAILED) {
          s4 = peg$parseinteger();
          if (s4 !== peg$FAILED) {
            s2 = [s2, s3, s4];
            s1 = s2;
          } else {
            peg$currPos = s1;
            s1 = peg$c1;
          }
        } else {
          peg$currPos = s1;
          s1 = peg$c1;
        }
      } else {
        peg$currPos = s1;
        s1 = peg$c1;
      }
      if (s1 !== peg$FAILED) {
        peg$reportedPos = s0;
        s1 = peg$c44(s1);
      }
      s0 = s1;

      return s0;
    }

    function peg$parsehex() {
      var s0, s1, s2, s3, s4;

      s0 = peg$currPos;
      if (input.charCodeAt(peg$currPos) === 45) {
        s1 = peg$c46;
        peg$currPos++;
      } else {
        s1 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c47); }
      }
      if (s1 === peg$FAILED) {
        s1 = peg$c45;
      }
      if (s1 !== peg$FAILED) {
        if (input.substr(peg$currPos, 3) === peg$c48) {
          s2 = peg$c48;
          peg$currPos += 3;
        } else {
          s2 = peg$FAILED;
          if (peg$silentFails === 0) { peg$fail(peg$c49); }
        }
        if (s2 !== peg$FAILED) {
          s3 = [];
          if (peg$c50.test(input.charAt(peg$currPos))) {
            s4 = input.charAt(peg$currPos);
            peg$currPos++;
          } else {
            s4 = peg$FAILED;
            if (peg$silentFails === 0) { peg$fail(peg$c51); }
          }
          if (s4 !== peg$FAILED) {
            while (s4 !== peg$FAILED) {
              s3.push(s4);
              if (peg$c50.test(input.charAt(peg$currPos))) {
                s4 = input.charAt(peg$currPos);
                peg$currPos++;
              } else {
                s4 = peg$FAILED;
                if (peg$silentFails === 0) { peg$fail(peg$c51); }
              }
            }
          } else {
            s3 = peg$c1;
          }
          if (s3 !== peg$FAILED) {
            peg$reportedPos = s0;
            s1 = peg$c52(s1, s3);
            s0 = s1;
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parsefloat() {
      var s0, s1, s2, s3, s4, s5;

      s0 = peg$currPos;
      if (input.charCodeAt(peg$currPos) === 45) {
        s1 = peg$c46;
        peg$currPos++;
      } else {
        s1 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c47); }
      }
      if (s1 === peg$FAILED) {
        s1 = peg$c45;
      }
      if (s1 !== peg$FAILED) {
        s2 = [];
        if (peg$c53.test(input.charAt(peg$currPos))) {
          s3 = input.charAt(peg$currPos);
          peg$currPos++;
        } else {
          s3 = peg$FAILED;
          if (peg$silentFails === 0) { peg$fail(peg$c54); }
        }
        if (s3 !== peg$FAILED) {
          while (s3 !== peg$FAILED) {
            s2.push(s3);
            if (peg$c53.test(input.charAt(peg$currPos))) {
              s3 = input.charAt(peg$currPos);
              peg$currPos++;
            } else {
              s3 = peg$FAILED;
              if (peg$silentFails === 0) { peg$fail(peg$c54); }
            }
          }
        } else {
          s2 = peg$c1;
        }
        if (s2 !== peg$FAILED) {
          if (input.charCodeAt(peg$currPos) === 46) {
            s3 = peg$c55;
            peg$currPos++;
          } else {
            s3 = peg$FAILED;
            if (peg$silentFails === 0) { peg$fail(peg$c56); }
          }
          if (s3 !== peg$FAILED) {
            s4 = [];
            if (peg$c53.test(input.charAt(peg$currPos))) {
              s5 = input.charAt(peg$currPos);
              peg$currPos++;
            } else {
              s5 = peg$FAILED;
              if (peg$silentFails === 0) { peg$fail(peg$c54); }
            }
            if (s5 !== peg$FAILED) {
              while (s5 !== peg$FAILED) {
                s4.push(s5);
                if (peg$c53.test(input.charAt(peg$currPos))) {
                  s5 = input.charAt(peg$currPos);
                  peg$currPos++;
                } else {
                  s5 = peg$FAILED;
                  if (peg$silentFails === 0) { peg$fail(peg$c54); }
                }
              }
            } else {
              s4 = peg$c1;
            }
            if (s4 !== peg$FAILED) {
              peg$reportedPos = s0;
              s1 = peg$c57(s1, s2, s4);
              s0 = s1;
            } else {
              peg$currPos = s0;
              s0 = peg$c1;
            }
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parseinteger() {
      var s0, s1, s2, s3;

      s0 = peg$currPos;
      if (input.charCodeAt(peg$currPos) === 45) {
        s1 = peg$c46;
        peg$currPos++;
      } else {
        s1 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c47); }
      }
      if (s1 === peg$FAILED) {
        s1 = peg$c45;
      }
      if (s1 !== peg$FAILED) {
        s2 = [];
        if (peg$c53.test(input.charAt(peg$currPos))) {
          s3 = input.charAt(peg$currPos);
          peg$currPos++;
        } else {
          s3 = peg$FAILED;
          if (peg$silentFails === 0) { peg$fail(peg$c54); }
        }
        if (s3 !== peg$FAILED) {
          while (s3 !== peg$FAILED) {
            s2.push(s3);
            if (peg$c53.test(input.charAt(peg$currPos))) {
              s3 = input.charAt(peg$currPos);
              peg$currPos++;
            } else {
              s3 = peg$FAILED;
              if (peg$silentFails === 0) { peg$fail(peg$c54); }
            }
          }
        } else {
          s2 = peg$c1;
        }
        if (s2 !== peg$FAILED) {
          peg$reportedPos = s0;
          s1 = peg$c58(s1, s2);
          s0 = s1;
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parseliteralArray() {
      var s0, s1, s2;

      s0 = peg$currPos;
      if (input.substr(peg$currPos, 2) === peg$c59) {
        s1 = peg$c59;
        peg$currPos += 2;
      } else {
        s1 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c60); }
      }
      if (s1 !== peg$FAILED) {
        s2 = peg$parseliteralArrayRest();
        if (s2 !== peg$FAILED) {
          peg$reportedPos = s0;
          s1 = peg$c38(s2);
          s0 = s1;
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parsebareLiteralArray() {
      var s0, s1, s2;

      s0 = peg$currPos;
      if (input.charCodeAt(peg$currPos) === 40) {
        s1 = peg$c61;
        peg$currPos++;
      } else {
        s1 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c62); }
      }
      if (s1 !== peg$FAILED) {
        s2 = peg$parseliteralArrayRest();
        if (s2 !== peg$FAILED) {
          peg$reportedPos = s0;
          s1 = peg$c38(s2);
          s0 = s1;
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parseliteralArrayRest() {
      var s0, s1, s2, s3, s4;

      s0 = peg$currPos;
      s1 = [];
      s2 = peg$currPos;
      s3 = peg$parsews();
      if (s3 !== peg$FAILED) {
        s4 = peg$parseparseTimeLiteral();
        if (s4 === peg$FAILED) {
          s4 = peg$parsebareLiteralArray();
          if (s4 === peg$FAILED) {
            s4 = peg$parsebareSymbol();
          }
        }
        if (s4 !== peg$FAILED) {
          peg$reportedPos = s2;
          s3 = peg$c63(s4);
          s2 = s3;
        } else {
          peg$currPos = s2;
          s2 = peg$c1;
        }
      } else {
        peg$currPos = s2;
        s2 = peg$c1;
      }
      while (s2 !== peg$FAILED) {
        s1.push(s2);
        s2 = peg$currPos;
        s3 = peg$parsews();
        if (s3 !== peg$FAILED) {
          s4 = peg$parseparseTimeLiteral();
          if (s4 === peg$FAILED) {
            s4 = peg$parsebareLiteralArray();
            if (s4 === peg$FAILED) {
              s4 = peg$parsebareSymbol();
            }
          }
          if (s4 !== peg$FAILED) {
            peg$reportedPos = s2;
            s3 = peg$c63(s4);
            s2 = s3;
          } else {
            peg$currPos = s2;
            s2 = peg$c1;
          }
        } else {
          peg$currPos = s2;
          s2 = peg$c1;
        }
      }
      if (s1 !== peg$FAILED) {
        s2 = peg$parsews();
        if (s2 !== peg$FAILED) {
          if (input.charCodeAt(peg$currPos) === 41) {
            s3 = peg$c64;
            peg$currPos++;
          } else {
            s3 = peg$FAILED;
            if (peg$silentFails === 0) { peg$fail(peg$c65); }
          }
          if (s3 !== peg$FAILED) {
            peg$reportedPos = s0;
            s1 = peg$c66(s1);
            s0 = s1;
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parsesymbolArray() {
      var s0, s1, s2, s3, s4, s5;

      s0 = peg$currPos;
      if (input.substr(peg$currPos, 2) === peg$c59) {
        s1 = peg$c59;
        peg$currPos += 2;
      } else {
        s1 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c60); }
      }
      if (s1 !== peg$FAILED) {
        s2 = [];
        s3 = peg$currPos;
        s4 = peg$parsews();
        if (s4 !== peg$FAILED) {
          s5 = peg$parsesymbol();
          if (s5 === peg$FAILED) {
            s5 = peg$parsebareSymbol();
          }
          if (s5 !== peg$FAILED) {
            peg$reportedPos = s3;
            s4 = peg$c67(s5);
            s3 = s4;
          } else {
            peg$currPos = s3;
            s3 = peg$c1;
          }
        } else {
          peg$currPos = s3;
          s3 = peg$c1;
        }
        while (s3 !== peg$FAILED) {
          s2.push(s3);
          s3 = peg$currPos;
          s4 = peg$parsews();
          if (s4 !== peg$FAILED) {
            s5 = peg$parsesymbol();
            if (s5 === peg$FAILED) {
              s5 = peg$parsebareSymbol();
            }
            if (s5 !== peg$FAILED) {
              peg$reportedPos = s3;
              s4 = peg$c67(s5);
              s3 = s4;
            } else {
              peg$currPos = s3;
              s3 = peg$c1;
            }
          } else {
            peg$currPos = s3;
            s3 = peg$c1;
          }
        }
        if (s2 !== peg$FAILED) {
          s3 = peg$parsews();
          if (s3 !== peg$FAILED) {
            if (input.charCodeAt(peg$currPos) === 41) {
              s4 = peg$c64;
              peg$currPos++;
            } else {
              s4 = peg$FAILED;
              if (peg$silentFails === 0) { peg$fail(peg$c65); }
            }
            if (s4 !== peg$FAILED) {
              peg$reportedPos = s0;
              s1 = peg$c68(s2);
              s0 = s1;
            } else {
              peg$currPos = s0;
              s0 = peg$c1;
            }
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parsedynamicArray() {
      var s0, s1, s2, s3, s4, s5, s6;

      s0 = peg$currPos;
      if (input.charCodeAt(peg$currPos) === 123) {
        s1 = peg$c69;
        peg$currPos++;
      } else {
        s1 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c70); }
      }
      if (s1 !== peg$FAILED) {
        s2 = peg$parsews();
        if (s2 !== peg$FAILED) {
          s3 = peg$parseexpressions();
          if (s3 === peg$FAILED) {
            s3 = peg$c45;
          }
          if (s3 !== peg$FAILED) {
            s4 = peg$parsews();
            if (s4 !== peg$FAILED) {
              if (input.charCodeAt(peg$currPos) === 46) {
                s5 = peg$c55;
                peg$currPos++;
              } else {
                s5 = peg$FAILED;
                if (peg$silentFails === 0) { peg$fail(peg$c56); }
              }
              if (s5 === peg$FAILED) {
                s5 = peg$c45;
              }
              if (s5 !== peg$FAILED) {
                if (input.charCodeAt(peg$currPos) === 125) {
                  s6 = peg$c71;
                  peg$currPos++;
                } else {
                  s6 = peg$FAILED;
                  if (peg$silentFails === 0) { peg$fail(peg$c72); }
                }
                if (s6 !== peg$FAILED) {
                  peg$reportedPos = s0;
                  s1 = peg$c73(s3);
                  s0 = s1;
                } else {
                  peg$currPos = s0;
                  s0 = peg$c1;
                }
              } else {
                peg$currPos = s0;
                s0 = peg$c1;
              }
            } else {
              peg$currPos = s0;
              s0 = peg$c1;
            }
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parsedynamicDictionary() {
      var s0, s1, s2, s3, s4, s5;

      s0 = peg$currPos;
      if (input.substr(peg$currPos, 2) === peg$c74) {
        s1 = peg$c74;
        peg$currPos += 2;
      } else {
        s1 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c75); }
      }
      if (s1 !== peg$FAILED) {
        s2 = peg$parsews();
        if (s2 !== peg$FAILED) {
          s3 = peg$parseassociations();
          if (s3 === peg$FAILED) {
            s3 = peg$c45;
          }
          if (s3 !== peg$FAILED) {
            s4 = peg$parsews();
            if (s4 !== peg$FAILED) {
              if (input.charCodeAt(peg$currPos) === 125) {
                s5 = peg$c71;
                peg$currPos++;
              } else {
                s5 = peg$FAILED;
                if (peg$silentFails === 0) { peg$fail(peg$c72); }
              }
              if (s5 !== peg$FAILED) {
                peg$reportedPos = s0;
                s1 = peg$c76(s3);
                s0 = s1;
              } else {
                peg$currPos = s0;
                s0 = peg$c1;
              }
            } else {
              peg$currPos = s0;
              s0 = peg$c1;
            }
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parsepseudoBooleanVariable() {
      var s0, s1, s2;

      s0 = peg$currPos;
      s1 = peg$currPos;
      if (input.substr(peg$currPos, 4) === peg$c77) {
        s2 = peg$c77;
        peg$currPos += 4;
      } else {
        s2 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c78); }
      }
      if (s2 !== peg$FAILED) {
        peg$reportedPos = s1;
        s2 = peg$c79();
      }
      s1 = s2;
      if (s1 === peg$FAILED) {
        s1 = peg$currPos;
        if (input.substr(peg$currPos, 5) === peg$c80) {
          s2 = peg$c80;
          peg$currPos += 5;
        } else {
          s2 = peg$FAILED;
          if (peg$silentFails === 0) { peg$fail(peg$c81); }
        }
        if (s2 !== peg$FAILED) {
          peg$reportedPos = s1;
          s2 = peg$c82();
        }
        s1 = s2;
      }
      if (s1 !== peg$FAILED) {
        peg$reportedPos = s0;
        s1 = peg$c83(s1);
      }
      s0 = s1;

      return s0;
    }

    function peg$parsepseudoNilVariable() {
      var s0, s1;

      s0 = peg$currPos;
      if (input.substr(peg$currPos, 3) === peg$c84) {
        s1 = peg$c84;
        peg$currPos += 3;
      } else {
        s1 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c85); }
      }
      if (s1 !== peg$FAILED) {
        peg$reportedPos = s0;
        s1 = peg$c86(s1);
      }
      s0 = s1;

      return s0;
    }

    function peg$parsepseudoVariable() {
      var s0;

      s0 = peg$parsepseudoBooleanVariable();
      if (s0 === peg$FAILED) {
        s0 = peg$parsepseudoNilVariable();
      }

      return s0;
    }

    function peg$parseparseTimeLiteral() {
      var s0;

      s0 = peg$parsepseudoVariable();
      if (s0 === peg$FAILED) {
        s0 = peg$parsenumber();
        if (s0 === peg$FAILED) {
          s0 = peg$parseliteralArray();
          if (s0 === peg$FAILED) {
            s0 = peg$parsestring();
            if (s0 === peg$FAILED) {
              s0 = peg$parsesymbol();
              if (s0 === peg$FAILED) {
                s0 = peg$parsecharacter();
              }
            }
          }
        }
      }

      return s0;
    }

    function peg$parseruntimeLiteral() {
      var s0;

      s0 = peg$parsedynamicDictionary();
      if (s0 === peg$FAILED) {
        s0 = peg$parsedynamicArray();
        if (s0 === peg$FAILED) {
          s0 = peg$parseblock();
        }
      }

      return s0;
    }

    function peg$parseliteral() {
      var s0;

      s0 = peg$parseruntimeLiteral();
      if (s0 === peg$FAILED) {
        s0 = peg$parseparseTimeLiteral();
      }

      return s0;
    }

    function peg$parsevariable() {
      var s0, s1;

      s0 = peg$currPos;
      s1 = peg$parseidentifier();
      if (s1 !== peg$FAILED) {
        peg$reportedPos = s0;
        s1 = peg$c87(s1);
      }
      s0 = s1;

      return s0;
    }

    function peg$parsekeywordPair() {
      var s0, s1, s2, s3, s4;

      s0 = peg$currPos;
      s1 = peg$parsews();
      if (s1 !== peg$FAILED) {
        s2 = peg$parsekeyword();
        if (s2 !== peg$FAILED) {
          s3 = peg$parsews();
          if (s3 !== peg$FAILED) {
            s4 = peg$parsebinarySend();
            if (s4 !== peg$FAILED) {
              peg$reportedPos = s0;
              s1 = peg$c88(s2, s4);
              s0 = s1;
            } else {
              peg$currPos = s0;
              s0 = peg$c1;
            }
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parsebinarySelector() {
      var s0, s1, s2;

      s0 = peg$currPos;
      s1 = [];
      if (peg$c89.test(input.charAt(peg$currPos))) {
        s2 = input.charAt(peg$currPos);
        peg$currPos++;
      } else {
        s2 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c90); }
      }
      if (s2 !== peg$FAILED) {
        while (s2 !== peg$FAILED) {
          s1.push(s2);
          if (peg$c89.test(input.charAt(peg$currPos))) {
            s2 = input.charAt(peg$currPos);
            peg$currPos++;
          } else {
            s2 = peg$FAILED;
            if (peg$silentFails === 0) { peg$fail(peg$c90); }
          }
        }
      } else {
        s1 = peg$c1;
      }
      if (s1 !== peg$FAILED) {
        peg$reportedPos = s0;
        s1 = peg$c91(s1);
      }
      s0 = s1;

      return s0;
    }

    function peg$parsekeywordPattern() {
      var s0, s1, s2, s3, s4, s5, s6;

      s0 = peg$currPos;
      s1 = [];
      s2 = peg$currPos;
      s3 = peg$parsews();
      if (s3 !== peg$FAILED) {
        s4 = peg$parsekeyword();
        if (s4 !== peg$FAILED) {
          s5 = peg$parsews();
          if (s5 !== peg$FAILED) {
            s6 = peg$parseidentifier();
            if (s6 !== peg$FAILED) {
              peg$reportedPos = s2;
              s3 = peg$c88(s4, s6);
              s2 = s3;
            } else {
              peg$currPos = s2;
              s2 = peg$c1;
            }
          } else {
            peg$currPos = s2;
            s2 = peg$c1;
          }
        } else {
          peg$currPos = s2;
          s2 = peg$c1;
        }
      } else {
        peg$currPos = s2;
        s2 = peg$c1;
      }
      if (s2 !== peg$FAILED) {
        while (s2 !== peg$FAILED) {
          s1.push(s2);
          s2 = peg$currPos;
          s3 = peg$parsews();
          if (s3 !== peg$FAILED) {
            s4 = peg$parsekeyword();
            if (s4 !== peg$FAILED) {
              s5 = peg$parsews();
              if (s5 !== peg$FAILED) {
                s6 = peg$parseidentifier();
                if (s6 !== peg$FAILED) {
                  peg$reportedPos = s2;
                  s3 = peg$c88(s4, s6);
                  s2 = s3;
                } else {
                  peg$currPos = s2;
                  s2 = peg$c1;
                }
              } else {
                peg$currPos = s2;
                s2 = peg$c1;
              }
            } else {
              peg$currPos = s2;
              s2 = peg$c1;
            }
          } else {
            peg$currPos = s2;
            s2 = peg$c1;
          }
        }
      } else {
        s1 = peg$c1;
      }
      if (s1 !== peg$FAILED) {
        peg$reportedPos = s0;
        s1 = peg$c92(s1);
      }
      s0 = s1;

      return s0;
    }

    function peg$parsebinaryPattern() {
      var s0, s1, s2, s3, s4;

      s0 = peg$currPos;
      s1 = peg$parsews();
      if (s1 !== peg$FAILED) {
        s2 = peg$parsebinarySelector();
        if (s2 !== peg$FAILED) {
          s3 = peg$parsews();
          if (s3 !== peg$FAILED) {
            s4 = peg$parseidentifier();
            if (s4 !== peg$FAILED) {
              peg$reportedPos = s0;
              s1 = peg$c93(s2, s4);
              s0 = s1;
            } else {
              peg$currPos = s0;
              s0 = peg$c1;
            }
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parseunaryPattern() {
      var s0, s1, s2;

      s0 = peg$currPos;
      s1 = peg$parsews();
      if (s1 !== peg$FAILED) {
        s2 = peg$parseidentifier();
        if (s2 !== peg$FAILED) {
          peg$reportedPos = s0;
          s1 = peg$c94(s2);
          s0 = s1;
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parseexpressionChain() {
      var s0, s1, s2, s3, s4;

      s0 = peg$currPos;
      s1 = peg$parsemessage();
      if (s1 !== peg$FAILED) {
        s2 = peg$currPos;
        s3 = peg$parsews();
        if (s3 !== peg$FAILED) {
          s4 = peg$parseexpressionChain();
          if (s4 !== peg$FAILED) {
            peg$reportedPos = s2;
            s3 = peg$c95(s4);
            s2 = s3;
          } else {
            peg$currPos = s2;
            s2 = peg$c1;
          }
        } else {
          peg$currPos = s2;
          s2 = peg$c1;
        }
        if (s2 === peg$FAILED) {
          s2 = peg$c45;
        }
        if (s2 !== peg$FAILED) {
          peg$reportedPos = s0;
          s1 = peg$c96(s1);
          s0 = s1;
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parseexpression2() {
      var s0, s1, s2, s3, s4;

      s0 = peg$currPos;
      s1 = peg$parsekeywordSend();
      if (s1 === peg$FAILED) {
        s1 = peg$parsebinarySend();
      }
      if (s1 !== peg$FAILED) {
        s2 = peg$currPos;
        s3 = peg$parsews();
        if (s3 !== peg$FAILED) {
          s4 = peg$parseexpressionChain();
          if (s4 !== peg$FAILED) {
            peg$reportedPos = s2;
            s3 = peg$c97(s4);
            s2 = s3;
          } else {
            peg$currPos = s2;
            s2 = peg$c1;
          }
        } else {
          peg$currPos = s2;
          s2 = peg$c1;
        }
        if (s2 === peg$FAILED) {
          s2 = peg$c45;
        }
        if (s2 !== peg$FAILED) {
          peg$reportedPos = s0;
          s1 = peg$c98(s1, s2);
          s0 = s1;
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parseexpression() {
      var s0;

      s0 = peg$parseassignment();
      if (s0 === peg$FAILED) {
        s0 = peg$parsecascade();
        if (s0 === peg$FAILED) {
          s0 = peg$parseexpression2();
        }
      }

      return s0;
    }

    function peg$parseexpressionList() {
      var s0, s1, s2, s3, s4;

      s0 = peg$currPos;
      s1 = peg$parsews();
      if (s1 !== peg$FAILED) {
        if (input.charCodeAt(peg$currPos) === 46) {
          s2 = peg$c55;
          peg$currPos++;
        } else {
          s2 = peg$FAILED;
          if (peg$silentFails === 0) { peg$fail(peg$c56); }
        }
        if (s2 !== peg$FAILED) {
          s3 = peg$parsews();
          if (s3 !== peg$FAILED) {
            s4 = peg$parseexpression();
            if (s4 !== peg$FAILED) {
              peg$reportedPos = s0;
              s1 = peg$c99(s4);
              s0 = s1;
            } else {
              peg$currPos = s0;
              s0 = peg$c1;
            }
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parseexpressions() {
      var s0, s1, s2, s3;

      s0 = peg$currPos;
      s1 = peg$parseexpression();
      if (s1 !== peg$FAILED) {
        s2 = [];
        s3 = peg$parseexpressionList();
        while (s3 !== peg$FAILED) {
          s2.push(s3);
          s3 = peg$parseexpressionList();
        }
        if (s2 !== peg$FAILED) {
          peg$reportedPos = s0;
          s1 = peg$c100(s1, s2);
          s0 = s1;
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parseassignment() {
      var s0, s1, s2, s3, s4, s5;

      s0 = peg$currPos;
      s1 = peg$parsevariable();
      if (s1 !== peg$FAILED) {
        s2 = peg$parsews();
        if (s2 !== peg$FAILED) {
          if (input.substr(peg$currPos, 2) === peg$c101) {
            s3 = peg$c101;
            peg$currPos += 2;
          } else {
            s3 = peg$FAILED;
            if (peg$silentFails === 0) { peg$fail(peg$c102); }
          }
          if (s3 !== peg$FAILED) {
            s4 = peg$parsews();
            if (s4 !== peg$FAILED) {
              s5 = peg$parseexpression();
              if (s5 !== peg$FAILED) {
                peg$reportedPos = s0;
                s1 = peg$c103(s1, s5);
                s0 = s1;
              } else {
                peg$currPos = s0;
                s0 = peg$c1;
              }
            } else {
              peg$currPos = s0;
              s0 = peg$c1;
            }
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parseret() {
      var s0, s1, s2, s3, s4, s5;

      s0 = peg$currPos;
      if (input.charCodeAt(peg$currPos) === 94) {
        s1 = peg$c104;
        peg$currPos++;
      } else {
        s1 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c105); }
      }
      if (s1 !== peg$FAILED) {
        s2 = peg$parsews();
        if (s2 !== peg$FAILED) {
          s3 = peg$parseexpression();
          if (s3 !== peg$FAILED) {
            s4 = peg$parsews();
            if (s4 !== peg$FAILED) {
              if (input.charCodeAt(peg$currPos) === 46) {
                s5 = peg$c55;
                peg$currPos++;
              } else {
                s5 = peg$FAILED;
                if (peg$silentFails === 0) { peg$fail(peg$c56); }
              }
              if (s5 === peg$FAILED) {
                s5 = peg$c45;
              }
              if (s5 !== peg$FAILED) {
                peg$reportedPos = s0;
                s1 = peg$c106(s3);
                s0 = s1;
              } else {
                peg$currPos = s0;
                s0 = peg$c1;
              }
            } else {
              peg$currPos = s0;
              s0 = peg$c1;
            }
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parsetemps() {
      var s0, s1, s2, s3, s4, s5;

      s0 = peg$currPos;
      if (input.charCodeAt(peg$currPos) === 124) {
        s1 = peg$c107;
        peg$currPos++;
      } else {
        s1 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c108); }
      }
      if (s1 !== peg$FAILED) {
        s2 = [];
        s3 = peg$currPos;
        s4 = peg$parsews();
        if (s4 !== peg$FAILED) {
          s5 = peg$parseidentifier();
          if (s5 !== peg$FAILED) {
            peg$reportedPos = s3;
            s4 = peg$c109(s5);
            s3 = s4;
          } else {
            peg$currPos = s3;
            s3 = peg$c1;
          }
        } else {
          peg$currPos = s3;
          s3 = peg$c1;
        }
        while (s3 !== peg$FAILED) {
          s2.push(s3);
          s3 = peg$currPos;
          s4 = peg$parsews();
          if (s4 !== peg$FAILED) {
            s5 = peg$parseidentifier();
            if (s5 !== peg$FAILED) {
              peg$reportedPos = s3;
              s4 = peg$c109(s5);
              s3 = s4;
            } else {
              peg$currPos = s3;
              s3 = peg$c1;
            }
          } else {
            peg$currPos = s3;
            s3 = peg$c1;
          }
        }
        if (s2 !== peg$FAILED) {
          s3 = peg$parsews();
          if (s3 !== peg$FAILED) {
            if (input.charCodeAt(peg$currPos) === 124) {
              s4 = peg$c107;
              peg$currPos++;
            } else {
              s4 = peg$FAILED;
              if (peg$silentFails === 0) { peg$fail(peg$c108); }
            }
            if (s4 !== peg$FAILED) {
              peg$reportedPos = s0;
              s1 = peg$c110(s2);
              s0 = s1;
            } else {
              peg$currPos = s0;
              s0 = peg$c1;
            }
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parseblockParamList() {
      var s0, s1, s2, s3, s4, s5, s6;

      s0 = peg$currPos;
      s1 = [];
      s2 = peg$currPos;
      s3 = peg$parsews();
      if (s3 !== peg$FAILED) {
        if (input.charCodeAt(peg$currPos) === 58) {
          s4 = peg$c16;
          peg$currPos++;
        } else {
          s4 = peg$FAILED;
          if (peg$silentFails === 0) { peg$fail(peg$c17); }
        }
        if (s4 !== peg$FAILED) {
          s5 = peg$parsews();
          if (s5 !== peg$FAILED) {
            s6 = peg$parseidentifier();
            if (s6 !== peg$FAILED) {
              peg$reportedPos = s2;
              s3 = peg$c111(s6);
              s2 = s3;
            } else {
              peg$currPos = s2;
              s2 = peg$c1;
            }
          } else {
            peg$currPos = s2;
            s2 = peg$c1;
          }
        } else {
          peg$currPos = s2;
          s2 = peg$c1;
        }
      } else {
        peg$currPos = s2;
        s2 = peg$c1;
      }
      if (s2 !== peg$FAILED) {
        while (s2 !== peg$FAILED) {
          s1.push(s2);
          s2 = peg$currPos;
          s3 = peg$parsews();
          if (s3 !== peg$FAILED) {
            if (input.charCodeAt(peg$currPos) === 58) {
              s4 = peg$c16;
              peg$currPos++;
            } else {
              s4 = peg$FAILED;
              if (peg$silentFails === 0) { peg$fail(peg$c17); }
            }
            if (s4 !== peg$FAILED) {
              s5 = peg$parsews();
              if (s5 !== peg$FAILED) {
                s6 = peg$parseidentifier();
                if (s6 !== peg$FAILED) {
                  peg$reportedPos = s2;
                  s3 = peg$c111(s6);
                  s2 = s3;
                } else {
                  peg$currPos = s2;
                  s2 = peg$c1;
                }
              } else {
                peg$currPos = s2;
                s2 = peg$c1;
              }
            } else {
              peg$currPos = s2;
              s2 = peg$c1;
            }
          } else {
            peg$currPos = s2;
            s2 = peg$c1;
          }
        }
      } else {
        s1 = peg$c1;
      }
      if (s1 !== peg$FAILED) {
        s2 = peg$parsews();
        if (s2 !== peg$FAILED) {
          if (input.charCodeAt(peg$currPos) === 124) {
            s3 = peg$c107;
            peg$currPos++;
          } else {
            s3 = peg$FAILED;
            if (peg$silentFails === 0) { peg$fail(peg$c108); }
          }
          if (s3 !== peg$FAILED) {
            peg$reportedPos = s0;
            s1 = peg$c112(s1);
            s0 = s1;
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parsesubexpression() {
      var s0, s1, s2, s3, s4, s5;

      s0 = peg$currPos;
      if (input.charCodeAt(peg$currPos) === 40) {
        s1 = peg$c61;
        peg$currPos++;
      } else {
        s1 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c62); }
      }
      if (s1 !== peg$FAILED) {
        s2 = peg$parsews();
        if (s2 !== peg$FAILED) {
          s3 = peg$parseexpression();
          if (s3 !== peg$FAILED) {
            s4 = peg$parsews();
            if (s4 !== peg$FAILED) {
              if (input.charCodeAt(peg$currPos) === 41) {
                s5 = peg$c64;
                peg$currPos++;
              } else {
                s5 = peg$FAILED;
                if (peg$silentFails === 0) { peg$fail(peg$c65); }
              }
              if (s5 !== peg$FAILED) {
                peg$reportedPos = s0;
                s1 = peg$c99(s3);
                s0 = s1;
              } else {
                peg$currPos = s0;
                s0 = peg$c1;
              }
            } else {
              peg$currPos = s0;
              s0 = peg$c1;
            }
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parsestatements() {
      var s0, s1, s2, s3, s4, s5, s6, s7;

      s0 = peg$currPos;
      s1 = peg$parseret();
      if (s1 !== peg$FAILED) {
        s2 = [];
        if (input.charCodeAt(peg$currPos) === 46) {
          s3 = peg$c55;
          peg$currPos++;
        } else {
          s3 = peg$FAILED;
          if (peg$silentFails === 0) { peg$fail(peg$c56); }
        }
        while (s3 !== peg$FAILED) {
          s2.push(s3);
          if (input.charCodeAt(peg$currPos) === 46) {
            s3 = peg$c55;
            peg$currPos++;
          } else {
            s3 = peg$FAILED;
            if (peg$silentFails === 0) { peg$fail(peg$c56); }
          }
        }
        if (s2 !== peg$FAILED) {
          peg$reportedPos = s0;
          s1 = peg$c113(s1);
          s0 = s1;
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }
      if (s0 === peg$FAILED) {
        s0 = peg$currPos;
        s1 = peg$parseexpressions();
        if (s1 !== peg$FAILED) {
          s2 = peg$parsews();
          if (s2 !== peg$FAILED) {
            s3 = [];
            if (input.charCodeAt(peg$currPos) === 46) {
              s4 = peg$c55;
              peg$currPos++;
            } else {
              s4 = peg$FAILED;
              if (peg$silentFails === 0) { peg$fail(peg$c56); }
            }
            if (s4 !== peg$FAILED) {
              while (s4 !== peg$FAILED) {
                s3.push(s4);
                if (input.charCodeAt(peg$currPos) === 46) {
                  s4 = peg$c55;
                  peg$currPos++;
                } else {
                  s4 = peg$FAILED;
                  if (peg$silentFails === 0) { peg$fail(peg$c56); }
                }
              }
            } else {
              s3 = peg$c1;
            }
            if (s3 !== peg$FAILED) {
              s4 = peg$parsews();
              if (s4 !== peg$FAILED) {
                s5 = peg$parseret();
                if (s5 !== peg$FAILED) {
                  s6 = [];
                  if (input.charCodeAt(peg$currPos) === 46) {
                    s7 = peg$c55;
                    peg$currPos++;
                  } else {
                    s7 = peg$FAILED;
                    if (peg$silentFails === 0) { peg$fail(peg$c56); }
                  }
                  while (s7 !== peg$FAILED) {
                    s6.push(s7);
                    if (input.charCodeAt(peg$currPos) === 46) {
                      s7 = peg$c55;
                      peg$currPos++;
                    } else {
                      s7 = peg$FAILED;
                      if (peg$silentFails === 0) { peg$fail(peg$c56); }
                    }
                  }
                  if (s6 !== peg$FAILED) {
                    peg$reportedPos = s0;
                    s1 = peg$c114(s1, s5);
                    s0 = s1;
                  } else {
                    peg$currPos = s0;
                    s0 = peg$c1;
                  }
                } else {
                  peg$currPos = s0;
                  s0 = peg$c1;
                }
              } else {
                peg$currPos = s0;
                s0 = peg$c1;
              }
            } else {
              peg$currPos = s0;
              s0 = peg$c1;
            }
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
        if (s0 === peg$FAILED) {
          s0 = peg$currPos;
          s1 = peg$parseexpressions();
          if (s1 === peg$FAILED) {
            s1 = peg$c45;
          }
          if (s1 !== peg$FAILED) {
            s2 = [];
            if (input.charCodeAt(peg$currPos) === 46) {
              s3 = peg$c55;
              peg$currPos++;
            } else {
              s3 = peg$FAILED;
              if (peg$silentFails === 0) { peg$fail(peg$c56); }
            }
            while (s3 !== peg$FAILED) {
              s2.push(s3);
              if (input.charCodeAt(peg$currPos) === 46) {
                s3 = peg$c55;
                peg$currPos++;
              } else {
                s3 = peg$FAILED;
                if (peg$silentFails === 0) { peg$fail(peg$c56); }
              }
            }
            if (s2 !== peg$FAILED) {
              peg$reportedPos = s0;
              s1 = peg$c115(s1);
              s0 = s1;
            } else {
              peg$currPos = s0;
              s0 = peg$c1;
            }
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        }
      }

      return s0;
    }

    function peg$parsestatement() {
      var s0, s1, s2, s3;

      s0 = peg$currPos;
      s1 = peg$currPos;
      s2 = peg$parseexpression();
      if (s2 !== peg$FAILED) {
        if (input.charCodeAt(peg$currPos) === 46) {
          s3 = peg$c55;
          peg$currPos++;
        } else {
          s3 = peg$FAILED;
          if (peg$silentFails === 0) { peg$fail(peg$c56); }
        }
        if (s3 !== peg$FAILED) {
          peg$reportedPos = s1;
          s2 = peg$c116(s2);
          s1 = s2;
        } else {
          peg$currPos = s1;
          s1 = peg$c1;
        }
      } else {
        peg$currPos = s1;
        s1 = peg$c1;
      }
      if (s1 === peg$FAILED) {
        s1 = peg$parsejsStatement();
      }
      if (s1 !== peg$FAILED) {
        peg$reportedPos = s0;
        s1 = peg$c117(s1);
      }
      s0 = s1;

      return s0;
    }

    function peg$parsesequence() {
      var s0;

      s0 = peg$parsejsStatement();
      if (s0 === peg$FAILED) {
        s0 = peg$parsestSequence();
      }

      return s0;
    }

    function peg$parsestSequence() {
      var s0, s1, s2, s3, s4;

      s0 = peg$currPos;
      s1 = peg$parsetemps();
      if (s1 === peg$FAILED) {
        s1 = peg$c45;
      }
      if (s1 !== peg$FAILED) {
        s2 = peg$parsews();
        if (s2 !== peg$FAILED) {
          s3 = peg$parsestatements();
          if (s3 === peg$FAILED) {
            s3 = peg$c45;
          }
          if (s3 !== peg$FAILED) {
            s4 = peg$parsews();
            if (s4 !== peg$FAILED) {
              peg$reportedPos = s0;
              s1 = peg$c118(s1, s3);
              s0 = s1;
            } else {
              peg$currPos = s0;
              s0 = peg$c1;
            }
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parseblock() {
      var s0, s1, s2, s3, s4, s5, s6;

      s0 = peg$currPos;
      if (input.charCodeAt(peg$currPos) === 91) {
        s1 = peg$c119;
        peg$currPos++;
      } else {
        s1 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c120); }
      }
      if (s1 !== peg$FAILED) {
        s2 = peg$parseblockParamList();
        if (s2 === peg$FAILED) {
          s2 = peg$c45;
        }
        if (s2 !== peg$FAILED) {
          s3 = peg$parsews();
          if (s3 !== peg$FAILED) {
            s4 = peg$parsesequence();
            if (s4 === peg$FAILED) {
              s4 = peg$c45;
            }
            if (s4 !== peg$FAILED) {
              s5 = peg$parsews();
              if (s5 !== peg$FAILED) {
                if (input.charCodeAt(peg$currPos) === 93) {
                  s6 = peg$c121;
                  peg$currPos++;
                } else {
                  s6 = peg$FAILED;
                  if (peg$silentFails === 0) { peg$fail(peg$c122); }
                }
                if (s6 !== peg$FAILED) {
                  peg$reportedPos = s0;
                  s1 = peg$c123(s2, s4);
                  s0 = s1;
                } else {
                  peg$currPos = s0;
                  s0 = peg$c1;
                }
              } else {
                peg$currPos = s0;
                s0 = peg$c1;
              }
            } else {
              peg$currPos = s0;
              s0 = peg$c1;
            }
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parseoperand() {
      var s0;

      s0 = peg$parseliteral();
      if (s0 === peg$FAILED) {
        s0 = peg$parsevariable();
        if (s0 === peg$FAILED) {
          s0 = peg$parsesubexpression();
        }
      }

      return s0;
    }

    function peg$parseunaryMessage() {
      var s0, s1, s2, s3, s4;

      s0 = peg$currPos;
      s1 = peg$parsews();
      if (s1 !== peg$FAILED) {
        s2 = peg$parseidentifier();
        if (s2 !== peg$FAILED) {
          s3 = peg$currPos;
          peg$silentFails++;
          if (input.charCodeAt(peg$currPos) === 58) {
            s4 = peg$c16;
            peg$currPos++;
          } else {
            s4 = peg$FAILED;
            if (peg$silentFails === 0) { peg$fail(peg$c17); }
          }
          peg$silentFails--;
          if (s4 === peg$FAILED) {
            s3 = peg$c124;
          } else {
            peg$currPos = s3;
            s3 = peg$c1;
          }
          if (s3 !== peg$FAILED) {
            peg$reportedPos = s0;
            s1 = peg$c125(s2);
            s0 = s1;
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parseunaryTail() {
      var s0, s1, s2, s3, s4;

      s0 = peg$currPos;
      s1 = peg$parseunaryMessage();
      if (s1 !== peg$FAILED) {
        s2 = peg$parsews();
        if (s2 !== peg$FAILED) {
          s3 = peg$parseunaryTail();
          if (s3 === peg$FAILED) {
            s3 = peg$c45;
          }
          if (s3 !== peg$FAILED) {
            s4 = peg$parsews();
            if (s4 !== peg$FAILED) {
              peg$reportedPos = s0;
              s1 = peg$c126(s1, s3);
              s0 = s1;
            } else {
              peg$currPos = s0;
              s0 = peg$c1;
            }
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parseunarySend() {
      var s0, s1, s2, s3;

      s0 = peg$currPos;
      s1 = peg$parseoperand();
      if (s1 !== peg$FAILED) {
        s2 = peg$parsews();
        if (s2 !== peg$FAILED) {
          s3 = peg$parseunaryTail();
          if (s3 === peg$FAILED) {
            s3 = peg$c45;
          }
          if (s3 !== peg$FAILED) {
            peg$reportedPos = s0;
            s1 = peg$c127(s1, s3);
            s0 = s1;
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parsebinaryMessage() {
      var s0, s1, s2, s3, s4;

      s0 = peg$currPos;
      s1 = peg$parsews();
      if (s1 !== peg$FAILED) {
        s2 = peg$parsebinarySelector();
        if (s2 !== peg$FAILED) {
          s3 = peg$parsews();
          if (s3 !== peg$FAILED) {
            s4 = peg$parseunarySend();
            if (s4 === peg$FAILED) {
              s4 = peg$parseoperand();
            }
            if (s4 !== peg$FAILED) {
              peg$reportedPos = s0;
              s1 = peg$c128(s2, s4);
              s0 = s1;
            } else {
              peg$currPos = s0;
              s0 = peg$c1;
            }
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parsebinaryTail() {
      var s0, s1, s2;

      s0 = peg$currPos;
      s1 = peg$parsebinaryMessage();
      if (s1 !== peg$FAILED) {
        s2 = peg$parsebinaryTail();
        if (s2 === peg$FAILED) {
          s2 = peg$c45;
        }
        if (s2 !== peg$FAILED) {
          peg$reportedPos = s0;
          s1 = peg$c126(s1, s2);
          s0 = s1;
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parsebinarySend() {
      var s0, s1, s2;

      s0 = peg$currPos;
      s1 = peg$parseunarySend();
      if (s1 !== peg$FAILED) {
        s2 = peg$parsebinaryTail();
        if (s2 === peg$FAILED) {
          s2 = peg$c45;
        }
        if (s2 !== peg$FAILED) {
          peg$reportedPos = s0;
          s1 = peg$c127(s1, s2);
          s0 = s1;
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parsekeywordMessage() {
      var s0, s1, s2;

      s0 = peg$currPos;
      s1 = [];
      s2 = peg$parsekeywordPair();
      if (s2 !== peg$FAILED) {
        while (s2 !== peg$FAILED) {
          s1.push(s2);
          s2 = peg$parsekeywordPair();
        }
      } else {
        s1 = peg$c1;
      }
      if (s1 !== peg$FAILED) {
        peg$reportedPos = s0;
        s1 = peg$c129(s1);
      }
      s0 = s1;

      return s0;
    }

    function peg$parsekeywordSend() {
      var s0, s1, s2;

      s0 = peg$currPos;
      s1 = peg$parsebinarySend();
      if (s1 !== peg$FAILED) {
        s2 = peg$parsekeywordMessage();
        if (s2 !== peg$FAILED) {
          peg$reportedPos = s0;
          s1 = peg$c130(s1, s2);
          s0 = s1;
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parsemessage() {
      var s0;

      s0 = peg$parsebinaryMessage();
      if (s0 === peg$FAILED) {
        s0 = peg$parseunaryMessage();
        if (s0 === peg$FAILED) {
          s0 = peg$parsekeywordMessage();
        }
      }

      return s0;
    }

    function peg$parsecascade() {
      var s0, s1, s2, s3, s4, s5, s6, s7, s8;

      s0 = peg$currPos;
      s1 = peg$parsews();
      if (s1 !== peg$FAILED) {
        s2 = peg$parsekeywordSend();
        if (s2 === peg$FAILED) {
          s2 = peg$parsebinarySend();
        }
        if (s2 !== peg$FAILED) {
          s3 = [];
          s4 = peg$currPos;
          s5 = peg$parsews();
          if (s5 !== peg$FAILED) {
            if (input.charCodeAt(peg$currPos) === 59) {
              s6 = peg$c131;
              peg$currPos++;
            } else {
              s6 = peg$FAILED;
              if (peg$silentFails === 0) { peg$fail(peg$c132); }
            }
            if (s6 !== peg$FAILED) {
              s7 = peg$parsews();
              if (s7 !== peg$FAILED) {
                s8 = peg$parsemessage();
                if (s8 !== peg$FAILED) {
                  peg$reportedPos = s4;
                  s5 = peg$c133(s8);
                  s4 = s5;
                } else {
                  peg$currPos = s4;
                  s4 = peg$c1;
                }
              } else {
                peg$currPos = s4;
                s4 = peg$c1;
              }
            } else {
              peg$currPos = s4;
              s4 = peg$c1;
            }
          } else {
            peg$currPos = s4;
            s4 = peg$c1;
          }
          if (s4 !== peg$FAILED) {
            while (s4 !== peg$FAILED) {
              s3.push(s4);
              s4 = peg$currPos;
              s5 = peg$parsews();
              if (s5 !== peg$FAILED) {
                if (input.charCodeAt(peg$currPos) === 59) {
                  s6 = peg$c131;
                  peg$currPos++;
                } else {
                  s6 = peg$FAILED;
                  if (peg$silentFails === 0) { peg$fail(peg$c132); }
                }
                if (s6 !== peg$FAILED) {
                  s7 = peg$parsews();
                  if (s7 !== peg$FAILED) {
                    s8 = peg$parsemessage();
                    if (s8 !== peg$FAILED) {
                      peg$reportedPos = s4;
                      s5 = peg$c133(s8);
                      s4 = s5;
                    } else {
                      peg$currPos = s4;
                      s4 = peg$c1;
                    }
                  } else {
                    peg$currPos = s4;
                    s4 = peg$c1;
                  }
                } else {
                  peg$currPos = s4;
                  s4 = peg$c1;
                }
              } else {
                peg$currPos = s4;
                s4 = peg$c1;
              }
            }
          } else {
            s3 = peg$c1;
          }
          if (s3 !== peg$FAILED) {
            peg$reportedPos = s0;
            s1 = peg$c134(s2, s3);
            s0 = s1;
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parsejsStatement() {
      var s0, s1, s2, s3, s4;

      s0 = peg$currPos;
      if (input.charCodeAt(peg$currPos) === 60) {
        s1 = peg$c135;
        peg$currPos++;
      } else {
        s1 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c136); }
      }
      if (s1 !== peg$FAILED) {
        s2 = [];
        s3 = peg$currPos;
        if (input.substr(peg$currPos, 2) === peg$c137) {
          s4 = peg$c137;
          peg$currPos += 2;
        } else {
          s4 = peg$FAILED;
          if (peg$silentFails === 0) { peg$fail(peg$c138); }
        }
        if (s4 !== peg$FAILED) {
          peg$reportedPos = s3;
          s4 = peg$c139();
        }
        s3 = s4;
        if (s3 === peg$FAILED) {
          if (peg$c140.test(input.charAt(peg$currPos))) {
            s3 = input.charAt(peg$currPos);
            peg$currPos++;
          } else {
            s3 = peg$FAILED;
            if (peg$silentFails === 0) { peg$fail(peg$c141); }
          }
        }
        while (s3 !== peg$FAILED) {
          s2.push(s3);
          s3 = peg$currPos;
          if (input.substr(peg$currPos, 2) === peg$c137) {
            s4 = peg$c137;
            peg$currPos += 2;
          } else {
            s4 = peg$FAILED;
            if (peg$silentFails === 0) { peg$fail(peg$c138); }
          }
          if (s4 !== peg$FAILED) {
            peg$reportedPos = s3;
            s4 = peg$c139();
          }
          s3 = s4;
          if (s3 === peg$FAILED) {
            if (peg$c140.test(input.charAt(peg$currPos))) {
              s3 = input.charAt(peg$currPos);
              peg$currPos++;
            } else {
              s3 = peg$FAILED;
              if (peg$silentFails === 0) { peg$fail(peg$c141); }
            }
          }
        }
        if (s2 !== peg$FAILED) {
          if (input.charCodeAt(peg$currPos) === 62) {
            s3 = peg$c142;
            peg$currPos++;
          } else {
            s3 = peg$FAILED;
            if (peg$silentFails === 0) { peg$fail(peg$c143); }
          }
          if (s3 !== peg$FAILED) {
            peg$reportedPos = s0;
            s1 = peg$c144(s2);
            s0 = s1;
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parsemethod() {
      var s0, s1, s2, s3, s4;

      s0 = peg$currPos;
      s1 = peg$parsekeywordPattern();
      if (s1 === peg$FAILED) {
        s1 = peg$parsebinaryPattern();
        if (s1 === peg$FAILED) {
          s1 = peg$parseunaryPattern();
        }
      }
      if (s1 !== peg$FAILED) {
        s2 = peg$parsews();
        if (s2 !== peg$FAILED) {
          s3 = peg$parsesequence();
          if (s3 === peg$FAILED) {
            s3 = peg$c45;
          }
          if (s3 !== peg$FAILED) {
            s4 = peg$parsews();
            if (s4 !== peg$FAILED) {
              peg$reportedPos = s0;
              s1 = peg$c145(s1, s3);
              s0 = s1;
            } else {
              peg$currPos = s0;
              s0 = peg$c1;
            }
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parseassociationSend() {
      var s0, s1, s2;

      s0 = peg$currPos;
      s1 = peg$parsebinarySend();
      if (s1 !== peg$FAILED) {
        peg$reportedPos = peg$currPos;
        s2 = peg$c146(s1);
        if (s2) {
          s2 = peg$c124;
        } else {
          s2 = peg$c1;
        }
        if (s2 !== peg$FAILED) {
          peg$reportedPos = s0;
          s1 = peg$c147(s1);
          s0 = s1;
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parseassociationList() {
      var s0, s1, s2, s3, s4;

      s0 = peg$currPos;
      s1 = peg$parsews();
      if (s1 !== peg$FAILED) {
        if (input.charCodeAt(peg$currPos) === 46) {
          s2 = peg$c55;
          peg$currPos++;
        } else {
          s2 = peg$FAILED;
          if (peg$silentFails === 0) { peg$fail(peg$c56); }
        }
        if (s2 !== peg$FAILED) {
          s3 = peg$parsews();
          if (s3 !== peg$FAILED) {
            s4 = peg$parseassociationSend();
            if (s4 !== peg$FAILED) {
              peg$reportedPos = s0;
              s1 = peg$c99(s4);
              s0 = s1;
            } else {
              peg$currPos = s0;
              s0 = peg$c1;
            }
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parseassociations() {
      var s0, s1, s2, s3;

      s0 = peg$currPos;
      s1 = peg$parseassociationSend();
      if (s1 !== peg$FAILED) {
        s2 = [];
        s3 = peg$parseassociationList();
        while (s3 !== peg$FAILED) {
          s2.push(s3);
          s3 = peg$parseassociationList();
        }
        if (s2 !== peg$FAILED) {
          peg$reportedPos = s0;
          s1 = peg$c148(s1, s2);
          s0 = s1;
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parseclassDeclaration() {
      var s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18;

      s0 = peg$currPos;
      s1 = peg$parseselector();
      if (s1 !== peg$FAILED) {
        s2 = peg$parsews();
        if (s2 !== peg$FAILED) {
          if (input.substr(peg$currPos, 9) === peg$c149) {
            s3 = peg$c149;
            peg$currPos += 9;
          } else {
            s3 = peg$FAILED;
            if (peg$silentFails === 0) { peg$fail(peg$c150); }
          }
          if (s3 !== peg$FAILED) {
            s4 = peg$parsews();
            if (s4 !== peg$FAILED) {
              s5 = peg$parsesymbol();
              if (s5 !== peg$FAILED) {
                s6 = peg$parsews();
                if (s6 !== peg$FAILED) {
                  if (input.substr(peg$currPos, 10) === peg$c151) {
                    s7 = peg$c151;
                    peg$currPos += 10;
                  } else {
                    s7 = peg$FAILED;
                    if (peg$silentFails === 0) { peg$fail(peg$c152); }
                  }
                  if (s7 === peg$FAILED) {
                    if (input.substr(peg$currPos, 22) === peg$c153) {
                      s7 = peg$c153;
                      peg$currPos += 22;
                    } else {
                      s7 = peg$FAILED;
                      if (peg$silentFails === 0) { peg$fail(peg$c154); }
                    }
                  }
                  if (s7 !== peg$FAILED) {
                    s8 = peg$parsews();
                    if (s8 !== peg$FAILED) {
                      s9 = peg$parsesymbolArray();
                      if (s9 !== peg$FAILED) {
                        s10 = peg$currPos;
                        s11 = peg$parsews();
                        if (s11 !== peg$FAILED) {
                          if (input.substr(peg$currPos, 19) === peg$c155) {
                            s12 = peg$c155;
                            peg$currPos += 19;
                          } else {
                            s12 = peg$FAILED;
                            if (peg$silentFails === 0) { peg$fail(peg$c156); }
                          }
                          if (s12 !== peg$FAILED) {
                            s13 = peg$parsews();
                            if (s13 !== peg$FAILED) {
                              s14 = peg$parsesymbolArray();
                              if (s14 !== peg$FAILED) {
                                s15 = peg$parsews();
                                if (s15 !== peg$FAILED) {
                                  if (input.substr(peg$currPos, 17) === peg$c157) {
                                    s16 = peg$c157;
                                    peg$currPos += 17;
                                  } else {
                                    s16 = peg$FAILED;
                                    if (peg$silentFails === 0) { peg$fail(peg$c158); }
                                  }
                                  if (s16 !== peg$FAILED) {
                                    s17 = peg$parsews();
                                    if (s17 !== peg$FAILED) {
                                      s18 = peg$parsesymbolArray();
                                      if (s18 !== peg$FAILED) {
                                        s11 = [s11, s12, s13, s14, s15, s16, s17, s18];
                                        s10 = s11;
                                      } else {
                                        peg$currPos = s10;
                                        s10 = peg$c1;
                                      }
                                    } else {
                                      peg$currPos = s10;
                                      s10 = peg$c1;
                                    }
                                  } else {
                                    peg$currPos = s10;
                                    s10 = peg$c1;
                                  }
                                } else {
                                  peg$currPos = s10;
                                  s10 = peg$c1;
                                }
                              } else {
                                peg$currPos = s10;
                                s10 = peg$c1;
                              }
                            } else {
                              peg$currPos = s10;
                              s10 = peg$c1;
                            }
                          } else {
                            peg$currPos = s10;
                            s10 = peg$c1;
                          }
                        } else {
                          peg$currPos = s10;
                          s10 = peg$c1;
                        }
                        if (s10 === peg$FAILED) {
                          s10 = peg$c45;
                        }
                        if (s10 !== peg$FAILED) {
                          s11 = peg$parsews();
                          if (s11 !== peg$FAILED) {
                            if (input.charCodeAt(peg$currPos) === 46) {
                              s12 = peg$c55;
                              peg$currPos++;
                            } else {
                              s12 = peg$FAILED;
                              if (peg$silentFails === 0) { peg$fail(peg$c56); }
                            }
                            if (s12 !== peg$FAILED) {
                              peg$reportedPos = s0;
                              s1 = peg$c159(s1, s5, s9);
                              s0 = s1;
                            } else {
                              peg$currPos = s0;
                              s0 = peg$c1;
                            }
                          } else {
                            peg$currPos = s0;
                            s0 = peg$c1;
                          }
                        } else {
                          peg$currPos = s0;
                          s0 = peg$c1;
                        }
                      } else {
                        peg$currPos = s0;
                        s0 = peg$c1;
                      }
                    } else {
                      peg$currPos = s0;
                      s0 = peg$c1;
                    }
                  } else {
                    peg$currPos = s0;
                    s0 = peg$c1;
                  }
                } else {
                  peg$currPos = s0;
                  s0 = peg$c1;
                }
              } else {
                peg$currPos = s0;
                s0 = peg$c1;
              }
            } else {
              peg$currPos = s0;
              s0 = peg$c1;
            }
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parseclassAndMethod() {
      var s0, s1, s2, s3, s4, s5, s6, s7, s8;

      s0 = peg$currPos;
      if (input.charCodeAt(peg$currPos) === 33) {
        s1 = peg$c160;
        peg$currPos++;
      } else {
        s1 = peg$FAILED;
        if (peg$silentFails === 0) { peg$fail(peg$c161); }
      }
      if (s1 !== peg$FAILED) {
        s2 = peg$parseclassName();
        if (s2 !== peg$FAILED) {
          s3 = peg$parsews();
          if (s3 !== peg$FAILED) {
            s4 = peg$currPos;
            if (input.substr(peg$currPos, 5) === peg$c162) {
              s5 = peg$c162;
              peg$currPos += 5;
            } else {
              s5 = peg$FAILED;
              if (peg$silentFails === 0) { peg$fail(peg$c163); }
            }
            if (s5 !== peg$FAILED) {
              s6 = [];
              s7 = peg$parseseparator();
              if (s7 !== peg$FAILED) {
                while (s7 !== peg$FAILED) {
                  s6.push(s7);
                  s7 = peg$parseseparator();
                }
              } else {
                s6 = peg$c1;
              }
              if (s6 !== peg$FAILED) {
                s5 = [s5, s6];
                s4 = s5;
              } else {
                peg$currPos = s4;
                s4 = peg$c1;
              }
            } else {
              peg$currPos = s4;
              s4 = peg$c1;
            }
            if (s4 === peg$FAILED) {
              s4 = peg$c45;
            }
            if (s4 !== peg$FAILED) {
              s5 = peg$parsemethod();
              if (s5 !== peg$FAILED) {
                if (input.charCodeAt(peg$currPos) === 33) {
                  s6 = peg$c160;
                  peg$currPos++;
                } else {
                  s6 = peg$FAILED;
                  if (peg$silentFails === 0) { peg$fail(peg$c161); }
                }
                if (s6 !== peg$FAILED) {
                  s7 = peg$parsews();
                  if (s7 === peg$FAILED) {
                    s7 = peg$c45;
                  }
                  if (s7 !== peg$FAILED) {
                    if (input.charCodeAt(peg$currPos) === 46) {
                      s8 = peg$c55;
                      peg$currPos++;
                    } else {
                      s8 = peg$FAILED;
                      if (peg$silentFails === 0) { peg$fail(peg$c56); }
                    }
                    if (s8 !== peg$FAILED) {
                      peg$reportedPos = s0;
                      s1 = peg$c164(s2, s4, s5);
                      s0 = s1;
                    } else {
                      peg$currPos = s0;
                      s0 = peg$c1;
                    }
                  } else {
                    peg$currPos = s0;
                    s0 = peg$c1;
                  }
                } else {
                  peg$currPos = s0;
                  s0 = peg$c1;
                }
              } else {
                peg$currPos = s0;
                s0 = peg$c1;
              }
            } else {
              peg$currPos = s0;
              s0 = peg$c1;
            }
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parseprogramElement() {
      var s0, s1, s2, s3;

      s0 = peg$currPos;
      s1 = peg$parsews();
      if (s1 === peg$FAILED) {
        s1 = peg$c45;
      }
      if (s1 !== peg$FAILED) {
        s2 = peg$parseclassAndMethod();
        if (s2 === peg$FAILED) {
          s2 = peg$parseclassDeclaration();
          if (s2 === peg$FAILED) {
            s2 = peg$parsestatement();
          }
        }
        if (s2 !== peg$FAILED) {
          s3 = peg$parsews();
          if (s3 === peg$FAILED) {
            s3 = peg$c45;
          }
          if (s3 !== peg$FAILED) {
            peg$reportedPos = s0;
            s1 = peg$c165(s2);
            s0 = s1;
          } else {
            peg$currPos = s0;
            s0 = peg$c1;
          }
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }

    function peg$parseprogram() {
      var s0, s1, s2, s3, s4;

      s0 = peg$currPos;
      s1 = peg$currPos;
      s2 = peg$parsews();
      if (s2 === peg$FAILED) {
        s2 = peg$c45;
      }
      if (s2 !== peg$FAILED) {
        s3 = peg$parsetemps();
        if (s3 !== peg$FAILED) {
          s4 = peg$parsews();
          if (s4 === peg$FAILED) {
            s4 = peg$c45;
          }
          if (s4 !== peg$FAILED) {
            peg$reportedPos = s1;
            s2 = peg$c166(s3);
            s1 = s2;
          } else {
            peg$currPos = s1;
            s1 = peg$c1;
          }
        } else {
          peg$currPos = s1;
          s1 = peg$c1;
        }
      } else {
        peg$currPos = s1;
        s1 = peg$c1;
      }
      if (s1 === peg$FAILED) {
        s1 = peg$parseprogramElement();
      }
      if (s1 !== peg$FAILED) {
        s2 = [];
        s3 = peg$parseprogramElement();
        while (s3 !== peg$FAILED) {
          s2.push(s3);
          s3 = peg$parseprogramElement();
        }
        if (s2 !== peg$FAILED) {
          peg$reportedPos = s0;
          s1 = peg$c167(s1, s2);
          s0 = s1;
        } else {
          peg$currPos = s0;
          s0 = peg$c1;
        }
      } else {
        peg$currPos = s0;
        s0 = peg$c1;
      }

      return s0;
    }


    /* Reinitiase global data */
    Blether.classes = {
    	"Object": {
            "classMethods": {},
    		"methods": {}
    	}
    }


    peg$result = peg$startRuleFunction();

    if (peg$result !== peg$FAILED && peg$currPos === input.length) {
      return peg$result;
    } else {
      if (peg$result !== peg$FAILED && peg$currPos < input.length) {
        peg$fail({ type: "end", description: "end of input" });
      }

      throw peg$buildException(null, peg$maxFailExpected, peg$maxFailPos);
    }
  }

  return {
    SyntaxError: SyntaxError,
    parse:       parse
  };
})();
/* exported convertSelector */
function convertSelector(selector) {

	var mapping = {
		"\\": "backslash",
		"+": "plus",
		"*": "multiply",
		"/": "divide",
		"=": "equals",
		">": "greater_than",
		"<": "less_than",
		"@": "at",
		"%": "modulo",
		"~": "tilde",
		"|": "or",
		"&": "and",
		"-": "minus"
	};

	var javascriptEquivalents = {
		"do:": "forEach",
		"doWithIndex:": "forEach",
		"select:": "filter",
		"," : "concat"
	};

	if (javascriptEquivalents.hasOwnProperty(selector)) {
		return javascriptEquivalents[selector];
	}

	if (selector.match(/[\\+*/=><,@%~|&-]/)) {
		return "__" + selector.split("").map(function(e) { return mapping[e]; }).join("_") + "$";
	}

	if (selector.match(/:/)) {
		var selector_parts = selector.split(":").filter(function(each) { return each.length > 0; });

		if (selector_parts.length > 1) {
			selector = selector_parts.join("_") + "$";
		}
		else {
			selector = selector_parts[0];
		}
	}

	return selector;
}

/*exported BletherReturnOperatorVisitor*/
var BletherReturnOperatorVisitor = function() {

	function returnFalse() { return false }
	function returnTrue() { return true }

    this.visit = function(node) {
		return node.visit(this);
	};
	
    this.visitProgram = function(node) {
		for (var i = 0; i < node.elements.length; i++) {
			if (node.elements[i].visit(this)) {
				return true;
			}
		}
		return false;
	};
	
    this.visitClassDeclaration = function(node) {
		for (var method in node.methods) {
			if (node.methods.hasOwnProperty(method)) {
				if (node.methods[method].visit(this)) {
					return true;
				}
			}
		}
		return false;
	};
	
    this.visitMethodDeclaration = function(node) {
		return node.body.visit(this);
	};

    this.visitMethod = function(node) {
		for (var i = 0; i < node.sequences.length; i++) {
			if (node.sequences[i].visit(this)) {
				return true;
			}
		}

		return false;
	};
	
    this.visitStatement = function(node) {
		return node.expression.visit(this);
	};

    this.visitSequence = function(node) {
		for (var i = 0; i < node.statements.length; i++) {
			if (node.statements[i].visit(this)) {
				return true;
			}
		}

		return false;
	};
	
    this.visitSend = function(node) {
		// This happens with a Cascade
		if (node.receiver !== null) {
			if (node.receiver.visit(this)) {
				return true;
			}
		}

		for (var i = 0; i < node.args.length; i++) {
			if (node.args[i].visit(this)) {
				return true;
			}
		}

		return false;
	};
	
    this.visitAssignment = function(node) {
		return node.expression.visit(this);
	};
	
    this.visitBlock = function(node) {
		return node.sequence.visit(this);
	};
	
    this.visitCascade = function(node) {
		if (node.receiver.visit(this)) {
			return true;
		}

		for (var i = 0; i < node.messages.length; i++) {
			if (node.messages[i].visit(this)) {
				return true;
			}
		}

		return false;
	};
	
    this.visitDynamicArray = function(node) {
		for (var i = 0; i < node.values.length; i++) {
			if (node.values[i].visit(this)) {
				return true;
			}
		}

		return false;
	};
	
    this.visitDynamicDictionary = function(node) {
		for (var key in node.values) {
			if (node.values.hasOwnProperty(key)) {
				if (node.values[key].visit(this)) {
					return true;
				}
			}
		}
		return false;
	};

	this.visitReturn = returnTrue;
	
    this.visitBinaryPattern = returnFalse;
    this.visitKeywordPattern = returnFalse;
    this.visitString = returnFalse;
    this.visitSymbol = returnFalse;
    this.visitUnaryPattern = returnFalse;
    this.visitNumber = returnFalse;
    this.visitUndefinedObject = returnFalse;
    this.visitBoolean = returnFalse;
    this.visitArray = returnFalse;
    this.visitJsStatement = returnFalse;
    this.visitVariableDeclaration = returnFalse;
    this.visitVariable = returnFalse;
};

var BletherTranslator = function() {
	"use strict";

	function checkBlockMaxParamCount(block, max, selector) {
		if (block.params.length > max) {
			throw Blether.ParseError({
				"line": block.line,
				"column": block.column,
				"msg": "Too many parameters specified in block argument to " + selector
			});
		}
	}

	this.visit = function(node) {
		return node.visit(this);
	};

	this.visitProgram = function(node) {
		var self = this;

		this.context = new Blether.ContextMgr();

		var output = "";

		node.elements.forEach(function(childNode) {
			// Skip methods since these are visited inside classes
			if (childNode._type === "ClassMethodDeclaration" || childNode._type === "MethodDeclaration") {
				// Unless they're defined on Object
				if (typeof childNode.getClass().superClass !== "undefined") {
					return;
				}
			}

			output += childNode.visit(self);
		});

		return output;
	};

	this.visitClassDeclaration = function(node) {
		var self = this;
		var output = "";

		var hasSuper = node.superClass !== "Object";

		var instanceNames = node.varNames.value;

		output += "function " + node.className + "(";
		output += instanceNames.map(function(each) { return "_" + each.value }).join(", ");
		output += ") {\n";

		instanceNames.forEach(function(each) {
			var name = each.value;
			output += "this." + name + " = _" + name + ";\n";
		});

		output += "};\n\n";

		if (hasSuper) {
			output += node.className + ".prototype = Object.create(" + node.superClass + ".prototype);\n";
			output += node.className + ".prototype.constructor = " + node.superClass + ";\n\n";
		}

		this.context.pushClass(node.className.value, instanceNames.map(function(e) {return e.value}));

		var methods = node.getMethods();

		for (var prop in methods) {
			if (methods.hasOwnProperty(prop)) {
				output += methods[prop].visit(self);
			}
		}

		var classMethods = node.getClassMethods();

		for (var clProp in classMethods) {
			if (classMethods.hasOwnProperty(clProp)) {
				output += classMethods[clProp].visit(self);
			}
		}

		this.context.pop();

		return output;
	};

	this.visitMethodDeclaration = function(node) {

		var methodName = node.body.selector.visit(this)[0];

		var output = node.className + ".prototype." + methodName + " = ";

		output += node.body.visit(this);

		output += ";\n\n";

		return output;
	};

	this.visitClassMethodDeclaration = function(node) {

		var methodName = node.body.selector.visit(this)[0];

		var output = node.className + "." + methodName + " = ";

		output += node.body.visit(this);

		output += ";\n\n";

		return output;
	};

	this.visitString = function(node) {
		return "\"" + node.value.replace(/"/g, "\\\"") + "\"";
	};

	this.visitMethod = function(node) {

		var argumentNames = node.selector.visit(this)[1];

		this.context.pushMethod(argumentNames);

		var output = "function(" + argumentNames.join(", ") + ") {\n";

		// FIXME: This should happen through use of the visitor pattern,
		// but the checks below for return operator depth break it.
		if (node.sequence._type === "JsStatement") {
			output += node.sequence.visit(this);
			output += "\n}";
			return output;
		}

		output += "var self = this;\n";

		// If method has return caret, wrap with try block
		var hasReturnOperator = false;
		var returnOperatorVisitor = new BletherReturnOperatorVisitor();

		for (var i = 0; i < node.sequence.statements.length; i++ ) {
			var each = node.sequence.statements[i];
			if (each._type !== "Return" && returnOperatorVisitor.visit(each)) {
				hasReturnOperator = true;
				break;
			}
		}

		if (hasReturnOperator) {
			output += "try {\n";
		}

		var self = this;

		output += node.sequence.visit(self);

		if (hasReturnOperator) {
			output += "}\n";
			output += "catch (e) {\n";
			output += "  if (e instanceof STReturnValue) {\n";
			output += "    return e.value;\n";
			output += "  }\n";
			output += "  else {\n";
			output += "    throw e;\n";
			output += "  }\n";
			output += "}\n";
		}

		output += "}";

		this.context.pop();

		return output;
	};

	this.visitUnaryPattern = function(node) {
		return[convertSelector(node.selector), []];
	};

	this.visitBinaryPattern = function(node) {
		return [convertSelector(node.selector), [node.arg]];
	};

	this.visitKeywordPattern = function(node) {
		var self = this;
		var keywords = node.pairs.map(function(p) { return p.key });
		var params = node.pairs.map(function(p) { return p.arg });

		var argCounts = {};
		params.forEach(function(p) {
			argCounts[p] = (argCounts[p] || 0) + 1;

			if (argCounts[p] > 1) {

				throw Blether.ParseError({
					"line": node.line,
					"column": node.column,
					"msg": "Argument name " + p + " repeats in method " +
						self.context.currentClass() + ">>" + node.selector
				});
			}
		});

		return [convertSelector(keywords.join("")), params];
	};

	this.visitSymbol = function(node) {
		return this.visitString(node);
	};

	this.visitStatement = function(node) {
		var output = node.expression.visit(this);

		output += output[output.length -1] !== ";" ? ";\n" : "\n";

		return output;
	};

	this.visitSequence = function(node) {
		var self = this;

		var output = "";

		this.context.pushTemps(node.temps);

		if (node.temps.length > 0) {
			output += "var " + node.temps.join(", ") + ";\n";
		}

		var needsReturn = true;

		node.statements.forEach(function(each, index, array) {
			if (index === array.length - 1) {
				if (self.context.returnContext() !== "method") {
					if (each._type !== "Return") {
						output += "return ";
					}
					else {
						needsReturn = false;
					}
				}
				else {
					if (each._type === "Return") {
						needsReturn = false;
					}
				}
			}
			output += each.visit(self) + ";\n";
		});

		if (needsReturn && self.context.returnContext() === "method") {
			output += "return self;\n";
		}

		this.context.pop();

		return output;
	};

	this.visitSend = function(node) {
		var self = this;

		var receiver = node.receiver.visit(this);

		if (node.receiver._type === "Block") {
			receiver = "(" + receiver + ")";
		}

		switch (receiver) {
			case "super":
				return this.convertSuper(convertSelector(node.selector), node);

			case "NodeJS":
				return this.convertNodeJS(node);
		}

		var output = "";

		if (node.selector === "value" || node.selector.match(/^(value:)+$/)) {
			output = receiver + ".__value$(";
			output += node.args.map(function(a) { return a.visit(self) }).join(", ");
			output += ")";
			return output;
		}

		switch (node.selector) {

			case "at:":
				output += this.convertAt(receiver, node.args[0]);
				break;

			case "at:put:":
				output += this.convertAtPut(receiver, node.args[0], node.args[1]);
				break;

			case "isNil":
				output += "(typeof (" + receiver + ") === \"undefined\" || " + receiver + " === null)";
				break;

			case "notNil":
				output += "(typeof (" + receiver + ") !== \"undefined\" && " + receiver + " !== null)";
				break;

			case "isEmpty":
				output += "(" + receiver + ".length === 0)";
				break;

			case "ifEmpty:":
				output += this.convertIfEmpty(receiver, node);
				break;

			case "ifNotEmpty:":
				output += this.convertIfNotEmpty(receiver, node);
				break;

			case "notEmpty":
				output += "(" + receiver + ".length > 0)";
				break;

			case "ifNil:":
				output = this.convertIfNil(receiver, node);
				break;

			case "ifNotNil:":
				output = this.convertIfNotNil(receiver, node);
				break;

			case "ifNil:ifNotNil:":
				output = this.convertIfNilIfNotNil(receiver, node);
				break;

			case "new":
				output = this.convertNew(receiver);
				break;

			case "whileTrue:":
			case "whileFalse:":
				output = this.convertWhile(node);
				break;

			case "ifTrue:":
			case "ifFalse:":
				output = this.convertSingleIf(node);
				break;

			case "ifTrue:ifFalse:":
			case "ifFalse:ifTrue:":
				output = this.convertIfTrueIfFalse(node);
				break;

			case "and:":
			case "or:":
				output = this.convertAndOr(receiver, node);
				break;

			case "==":
				output = receiver + " === " + node.args[0].visit(self);
				break;

			case "~~":
				output = receiver + " !== " + node.args[0].visit(self);
				break;

			case "~=":
				output = "!(" + receiver + ".equals$(" + node.args[0].visit(self) + "))";
				break;

			case "asJQuery":
				output = "jQuery(" + receiver + ")";
				break;

			default:
				output = receiver + "." + convertSelector(node.selector) + "(";
				output += node.args.map(function(each) { return each.visit(self) }).join(", ");
				output += ")";
				break;
		}

		return output;
	};

	this.visitAssignment = function(node) {
		return node.variable.visit(this) + " = " + node.expression.visit(this);
	};

	this.visitNumber = function(node) {
		// Wrap in parentheses in order to make method calls work on
		// literals.
		return "(" + node.value.toString() + ")";
	};

	this.visitVariable = function(node) {
		var i;

		if (node.value === "self") {
			return "self";
		}

		if (node.value === "STReturnValue") {
			throw Blether.ParseError({
				"line": node.line,
				"column": node.column,
				"msg": "Do not refer directly to STReturnValue - use the caret operator instead"
			});
		}

		var temps = this.context.getTemps();
		for (i = 0; i < temps.length; i++) {
			if (node.value === temps[i]) {
				return node.value;
			}
		}

		var instanceVars = this.context.getInstanceVars();
		for (i = 0; i < instanceVars.length; i++) {
			if (node.value === instanceVars[i]) {
				return "self." + node.value;
			}
		}

		if (node.value.match(/^[A-Z]/)) {
			//console.warn("Assuming " + node.value + " is a class name");
			return node.value;
		}
		else {
			throw Blether.ParseError({
				"line": node.line,
				"column": node.column,
				"msg": "Unknown variable name [" + node.value + "]"
			});
		}
	};

	this.visitUndefinedObject = function() {
		return "null";
	};

	this.visitBoolean = function(node) {
		return node.value.toString();
	};

	this.invokeBlock = function(node) {
		if (node.sequence.statements.length === 0) {
			return "null";
		}

		var blockArgs = [];

		if (arguments.length > 1) {
			for (var i = 1; i < arguments.length; i++) {
				blockArgs.push(arguments[i]);
			}
		}

		if (node.params.length !== blockArgs.length) {
			throw Blether.ParseError({
				"line": node.line,
				"column": node.column,
				"msg": "Not enough parameters specified to block"
			});
		}

		if (node.sequence.statements.length === 1 && node.sequence.temps.length === 0) {

			var boundParameters = {};
			blockArgs.forEach(function(arg, index) {
				boundParameters[node.params[index]] = arg;
			});

			node.find(function(n) {
				return n._type === "Variable";
			}).forEach(function(n) {
				if (boundParameters.hasOwnProperty(n.value)) {
					n.value = boundParameters[n];
				}
			});

			this.context.pushTemps(blockArgs);

			var output = node.sequence.statements[0].visit(this);

			this.context.pop();

			return output;
		}

		return "(" + node.visit(this) + ")(" + blockArgs.join(", ") + ")";
	};

	this.visitBlock = function(node) {

		this.context.pushBlock(node.params);

		var output = "function(" + node.params.join(", ") + ") {\n";
		output += node.sequence.visit(this);
		output += "}";

		this.context.pop();

		return output;
	};

	this.visitCascade = function(node) {
		var self = this;
		var output;

		// Translate this cascade into a block, then visit that block

		// TODO: Add a method on the node objects e.g. isLiteral()
		var deconstruct = false;
		switch (node.receiver._type) {
			case "Variable":
			case "String":
			case "Symbol":
			case "Number":
			case "Boolean":
			case "UndefinedObject":
				deconstruct = true;
				break;
		}

		var sequence, statements, cascadeBlock;

		if (deconstruct) {

			statements = node.messages.map(function(each) {
				each.receiver = node.receiver;
				return each;
			});

			sequence     = new Blether.Sequence([], statements);
			cascadeBlock = new Blether.Block([], sequence);

			output = "(" + cascadeBlock.visit(self) + ")()";
		}
		else {
			var receiverVar = this.context.pushReceiver();

			statements = node.messages.map(function(each) {
				each.receiver = new Blether.Variable(receiverVar);
				return each;
			});

			sequence     = new Blether.Sequence([], statements);
			cascadeBlock = new Blether.Block([receiverVar], sequence);

			output = "(" + cascadeBlock.visit(self) + ")(" + node.receiver.visit(self) + ")";

			this.context.popReceiver();
		}

		return output;
	};

	this.visitJsStatement = function(node) {
		return node.javascript.trim();
	};

	// Highest-level variables i.e. global
	this.visitVariableDeclaration = function(node) {
		this.context.pushTemps(node.variables);
		return "var " + node.variables.join(", ") + ";\n";
	};

	this.visitArray = function(node) {
		var self = this;
		return "[" + node.value.map(function(each) { return each.visit(self) }).join(", ") + "]";
	};

	this.visitDynamicArray = function(node) {
		var self = this;
		return "[" + node.values.map(function(each) { return each.visit(self) }).join(", ") + "]";
	};

	this.visitDynamicDictionary = function(node) {

		if (node.values.length === 0) {
			return "Object.create(null)";
		}

		var output = "(function() {\n";
		output += "var _dict = Object.create(null);\n";

		for (var i = 0, j = 1; j < node.values.length; i += 2, j += 2) {
			if (node.values[i]._type === "String" || node.values[i]._type === "Symbol") {
				output += "_dict." + node.values[i].value + " = " + node.values[j].visit(this) + ";\n";
			}
			else {
				output += "_dict[" + node.values[i].visit(this) + "] = " + node.values[j].visit(this) + ";\n";
			}
		}

		output += "return _dict;\n";
		output += "})()";

		return output;
	};

	this.visitReturn = function(node) {
		if (this.context.returnContext() === "method" ) {
			return "return " + node.value.visit(this);
		}
		else {
			return "stReturn(" + node.value.visit(this) + ")";
		}
	};

	this.convertIfEmpty = function(receiver, node) {
		var block = node.args[0];
		var receiverVar = this.context.pushReceiver();
		var output;
		output  = "(function(" + receiverVar + ") {\n";
		output += "return (" + receiverVar + ".length === 0) ? " + block.invoke(this) + " : null;\n";
		output += "})(" + receiver + ")";
		this.context.popReceiver();
		return output;
	};

	this.convertIfNotEmpty = function(receiver, node) {
		var block = node.args[0];

		checkBlockMaxParamCount(block, 1, "ifNotEmpty:");

		var receiverVar = this.context.pushReceiver();

		var output;
		output  = "(function(" + receiverVar + ") {\n";
		output += "return (" + receiverVar + ".length > 0) ? ";
		output += block.params.length === 0 ? block.invoke(this) : block.invoke(this, receiverVar);
		output += " : null;\n";
		output += "})(" + receiver + ")";

		this.context.popReceiver();

		return output;
	};

	this.convertIfNil = function(receiver, node) {
		var block = node.args[0];
		var receiverVar = this.context.pushReceiver();
		var output;
		output  = "(function(" + receiverVar + ") {\n";
		output += "return (typeof " + receiverVar + " === \"undefined\" || " + receiverVar + " === null) ";
		output += "? " + block.invoke(this) + " : " + receiverVar + ";\n";
		output += "})(" + receiver + ")";

		this.context.popReceiver();

		return output;
	};

	this.convertIfNotNil = function(receiver, node) {
		var block = node.args[0];

		checkBlockMaxParamCount(block, 1, "ifNotNil:");

        var hasParam = block.params.length === 1;
		var receiverVar = hasParam ? block.params[0] : this.context.pushReceiver();

		var output;
		output  = "(function(" + receiverVar + ") {\n";
		output += "return (typeof " + receiverVar + " !== \"undefined\" && " + receiverVar + " !== null) ? ";
		output += block.params.length === 0 ? block.invoke(this) : block.invoke(this, receiverVar);
		output += " : null;\n";
		output += "})(" + receiver + ")";

		if (!hasParam) {
			this.context.popReceiver();
		}

		return output;
	};

	this.convertIfNilIfNotNil = function(receiver, node) {

		var ifNilBlock    = node.args[0];
		var ifNotNilBlock = node.args[1];

		checkBlockMaxParamCount(ifNilBlock, 0, "ifNil:");
		checkBlockMaxParamCount(ifNotNilBlock, 1, "ifNotNil:");

        var hasParam = ifNotNilBlock.params.length === 1;
		var receiverVar = hasParam ? ifNotNilBlock.params[0] : this.context.pushReceiver();

		var output = "(function(" + receiverVar + ") {\n";
		output += "return (typeof " + receiverVar + " === \"undefined\" || " + receiverVar + " === null)";
		output += " ? " + ifNilBlock.invoke(this) + " : ";
		output += ifNotNilBlock.params.length === 0 ? ifNotNilBlock.invoke(this) : ifNotNilBlock.invoke(this, receiverVar);
		output += ";\n";
		output += "})(" + receiver + ")";

		if (!hasParam) {
			this.context.popReceiver();
		}

		return output;
	};

	this.convertNew = function(receiver) {
		return "new " + receiver + "()";
	};

	this.convertSuper = function(selector, node) {
		var self = this;
		if (typeof this.context.currentClass() !== "undefined") {
			var output = this.context.currentClass() + ".__super__." + selector + ".call(self";
			if (node.args.length > 0) {
				node.args.forEach(function(each) {
					output += ", " + each.visit(self);
				});
			}
			output += ")";
			return output;
		}
		else {
			throw Blether.ParseError({
				"line": node.line,
				"column": node.column,
				"msg": "Can't use [super] outside a method declaration"
			});
		}
	};

	this.convertWhile = function(node) {
		if (node.receiver._type !== "Block") {
			throw Blether.ParseError({
				"line": node.line,
				"column": node.column,
				"msg": node.selector + " can be sent to blocks only"
			});
		}

		var loopCondition;
		var invert = node.selector === "whileFalse:" ? "!" : "";
		loopCondition = invert + node.receiver.invoke(this);

		var output = "while (" + loopCondition + ") {\n";
		output += node.args[0].invoke(this) + ";\n";
		output += "}\n";

		return output;
	};

	this.convertSingleIf = function(node) {
		if (node.args[0]._type !== "Block") {
			throw Blether.ParseError({
				"line": node.line,
				"column": node.column,
				"msg": node.selector + " accepts literal blocks only"
			});
		}

		var invert = node.selector === "ifFalse:" ? "!" : "";

		if (invert) {
			return node.receiver.visit(this) + " ? null : " + node.args[0].invoke(this);
		}
		else {
			return node.receiver.visit(this) + " ? " + node.args[0].invoke(this) + " : null";
		}
	};

	this.convertIfTrueIfFalse = function(node) {
		if (node.args[0]._type !== "Block" || node.args[1]._type !== "Block") {
			throw Blether.ParseError({
				"line": node.line,
				"column": node.column,
				"msg": node.selector + " accepts literal blocks only"
			});
		}

		var truthyIndex = 0, falsyIndex = 1;
		if (node.selector === "ifFalse:ifTrue:") {
			truthyIndex = 1;
			falsyIndex = 0;
		}

		return node.receiver.visit(this) +
			" ? " + node.args[truthyIndex].invoke(this) +
			" : " + node.args[falsyIndex].invoke(this);
	};

	this.convertAt = function(receiver, argument) {
		if (argument._type === "String" || argument._type === "Symbol") {
			return receiver + "." + argument.value;
		}

		return receiver + "[" + argument.visit(this) + "]";
	};

	this.convertAtPut = function(receiver, key, value) {
		if (key._type === "String" || key._type === "Symbol") {
			return receiver + "." + key.value + " = " + value.visit(this);
		}

		return receiver + "[" + key.visit(this) + "] = " + value.visit(this);
	};

	this.convertAndOr = function(receiver, node) {
		if (node.args[0]._type !== "Block") {
			throw Blether.ParseError({
				"line": node.line,
				"column": node.column,
				"msg": node.selector + " accepts literal blocks only"
			});
		}

		var operator = node.selector === "and:" ? " && " : " || ";

		return receiver + operator + node.args[0].invoke(this);
	};

	this.convertNodeJS = function(node) {
		var self = this;

		switch (node.selector) {
			case "filename":
				return "__filename";

			case "dirname":
				return "__dirname";

			case "require:":
				return "require(" + node.args[0].visit(self) + ")";

			default:
				throw Blether.ParseError({
					"line": node.line,
					"column": node.column,
					"msg": "Unknown selector [" + node.selector + "]"
				});
		}
	};
};


module.exports = {
	"translate": function(text) {
		var ast = BletherParser.parse(text);

		var translation = new BletherTranslator().visit(ast);

		return translation;
	},

	"runtime": function() {
		var path = require("path");
		var fs   = require("fs");

		var runtimePath = path.join(path.dirname(fs.realpathSync(__filename)), "runtime.js");
		return fs.readFileSync(runtimePath).toString();
	}
};
