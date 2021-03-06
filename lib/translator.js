var BletherTranslator = function() {
	"use strict";

	var NAMESPACES = {};

	function checkBlockMaxParamCount(block, max, selector) {
		if (block.params.length > max) {
			throw Blether.ParseError({
				"line": block.line,
				"column": block.column,
				"msg": "Too many parameters specified in block argument to " + selector
			});
		}
	}

	this.ensureNamespace = function(className) {

		var parts = className.value.split(".");

		if (parts.length === 1) {
			return "";
		}

		// Already defined this namespace
		if (NAMESPACES.hasOwnProperty(className)) {
			return "";
		}

		var ns = parts[0];
		var output = "var " + ns + " = " + ns + " || {};\n";
		NAMESPACES[ns] = true;

		var limit = parts.length - 1;
		for (var i = 1; i < limit; i++) {
			ns = ns + "." + parts[i];
			output += ns + " = " + ns + " || {};\n";
			NAMESPACES[ns] = true;
		}

		return output;
	};

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

		if (node.isNamespaced()) {
			output += this.ensureNamespace(node.className);
			output += node.className + " = function(";
		}
		else {
			output += "function " + node.className + "(";
		}
		output += instanceNames.map(function(each) { return "_" + each.value }).join(", ");
		output += ") {\n";

		instanceNames.forEach(function(each) {
			var name = each.value;
			output += "this." + name + " = _" + name + ";\n";
		});

		output += "}" + (node.isNamespaced() ? ";" : "") + "\n\n";

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
		return [convertSelector(node.selector), []];
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

	this.visitClassName = function(node) {
		return node.value;
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
			case "ClassName":
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
	},

	"BletherTranslator": BletherTranslator,
};
