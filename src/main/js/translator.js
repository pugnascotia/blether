var BletherTranslator = function() {
	"use strict";

	this.printedClassPrerequisites = false;

	var classPrerequisites =
"var _extends = function(child, parent) {\n" +
"	for (var key in parent) {\n" +
"		if (_hasProp.call(parent, key)) child[key] = parent[key];\n" +
"	}\n" +
"\n" +
"	function ctor() {\n" +
"		this.constructor = child;\n" +
"	}\n" +
"	ctor.prototype = parent.prototype;\n" +
"	child.prototype = new ctor();\n" +
"	child.__super__ = parent.prototype;\n" +
"	return child;\n" +
"};\n\n" +
"var _hasProp = {}.hasOwnProperty;\n\n";

	this.visit = function(node) {
		return node.visit(this);
	};

	this.visitProgram = function(node) {
		var self = this;

		this.context = {
			"instanceVars": [],
			"temps": [],
			"returnContext": "root"
		};

		var output = "";

		node.elements.forEach(function(childNode) {
			// Skip methods since these are visited inside classes
			if (childNode._type !== "MethodDeclaration" || typeof childNode.getClass().superClass === "undefined") {
				output += childNode.visit(self);
			}
		});

		return output;
	};

	this.visitClassDeclaration = function(node) {
		var self = this;
		var output = "";

		var hasSuper = node.superClass !== "Object";

		if (!this.printedClassPrerequisites && hasSuper) {
			output += classPrerequisites;
			this.printedClassPrerequisites = true;
		}

		output += "var " + node.className.value + " = (function(";

		if (hasSuper) {
			output += "_super";
		}
		
		output += ") {\n";

		if (hasSuper) {
			output += "_extends(" + node.className + ", _super);\n\n";
		}

		var instanceNames = node.varNames.value;

		output += "function " + node.className + "(";
		output += instanceNames.map(function(each) { return "_" + each.value }).join(", ");
		output += ") {\n";

		if (hasSuper) {
			output += "    return " + node.className + ".__super__.constructor.apply(this, arguments);\n";
		}

		instanceNames.forEach(function(each) {
			var name = each.value;
			output += "this." + name + " = _" + name + ";\n";
		});

		output += "};\n\n";

		var oldContext = this.context;

		this.context = {
			"currentClass": node.className.value,
			"instanceVars": instanceNames.map(function(e) { return e.value }),
			"temps": [],
			"returnContext": "method"
		};

		var methods = node.getMethods();
		
		for (var prop in methods) {
			if (methods.hasOwnProperty(prop)) {
				output += methods[prop].visit(self);
			}
		}

		this.context = oldContext;

		output += "\nreturn " + node.className + ";\n\n";

		output += "})(";

		if (hasSuper) {
			output += node.superClass;
		}
		
		output += ");\n\n";

		return output;
	};

	this.visitMethodDeclaration = function(node) {

		var methodName = node.body.selector.visit(this)[0];

		var output = node.className + ".prototype." + methodName + " = ";
		
		output += node.body.visit(this);

		output += ";\n\n";

		return output;
	};

	this.visitString = function(node) {
		return "new STString(\"" + node.value.replace(/"/g, "\\\"") + "\")";
	};

	this.visitMethod = function(node) {

		var argumentNames = node.selector.visit(this)[1];

		var oldTemps = this.context.temps;
		this.context.temps = oldTemps.concat(argumentNames);

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

		var oldReturnContext = this.context.returnContext;
		this.context.returnContext = "method";

		output += node.sequence.visit(self);

		this.context.returnContext = oldReturnContext;

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

		this.context.temps = oldTemps;

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

		debugger;

		var argCounts = {};
		params.forEach(function(p) {
			argCounts[p] = (argCounts[p] || 0) + 1;

			if (argCounts[p] > 1) {

				throw Blether.ParseError({
					"line": node.line,
					"column": node.column,
					"msg": "Argument name " + p + " repeats in method " +
						self.context.currentClass + ">>" + node.selector
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

		var oldTemps = this.context.temps;
		this.context.temps = [].concat(oldTemps, node.temps);

		node.temps.forEach(function(each) {
			output += "var " + each + ";\n";
		});

		var needsReturn = true;

		node.statements.forEach(function(each, index, array) {
			if (index === array.length - 1) {
				if (self.context.returnContext !== "method") {
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

		if (needsReturn && self.context.returnContext === "method") {
			output += "return self;\n";
		}

		this.context.temps = oldTemps;

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
		}

		var output = "";

		switch (node.selector) {

			case "at:":
				output += receiver + "[" + node.args[0].visit(self) + "]";
				break;

			case "at:put:":
				output += receiver + "[" + node.args[0].visit(self) + "] = " + node.args[1].visit(self);
				break;

			case "isNil":
				output += "(typeof (" + receiver + ") === \"undefined\")";
				break;

			case "notNil":
				output += "(typeof (" + receiver + ") !== \"undefined\")";
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

			case "==":
				output = receiver + " === " + node.args[0].visit(self);
				break;

			case "~~":
				output = receiver + " !== " + node.args[0].visit(self);
				break;

			case "~=":
				output = "!(" + receiver + ".equals$(" + node.args[0].visit(self) + "))";
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
		return "new STNumber(" + node.value.toString() + ")";
	};

	this.visitVariable = function(node) {
		var i;

		if (node.value === "self") {
			return "self";
		}

		for (i = 0; i < this.context.temps.length; i++) {
			if (node.value === this.context.temps[i]) {
				return node.value;
			}
		}

		for (i = 0; i < this.context.instanceVars.length; i++) {
			if (node.value === this.context.instanceVars[i]) {
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

	// FIXME: Collapse simple blocks into a ternary expression instead of
	// using a function
	this.visitBlock = function(node) {
		var oldContext = this.context.returnContext;
		this.context.returnContext = "block";
		var output = "function(" + node.params.join(", ") + ") {\n";

		var oldTemps = this.context.temps;
		this.context.temps = [].concat(oldTemps, node.params);

		output += node.sequence.visit(this);

		this.context.temps = oldTemps;

		output += "}";

		this.context.returnContext = oldContext;
		return output;
	};

	this.visitCascade = function(node) {
		var self = this;

		// Translate this cascade into a block, then visit that block

		var statements = node.messages.map(function(each) {
			each.receiver = new Blether.Variable("_recv");
			return each;
		});

		var sequence     = new Blether.Sequence([], statements);
		var cascadeBlock = new Blether.Block(["_recv"], sequence);

		var output = "(" + cascadeBlock.visit(self) + ")(" + node.receiver.visit(self) + ")";

		return output;
	};

	this.visitJsStatement = function(node) {
		return node.javascript.trim();
	};

	// Highest-level variables i.e. global
	this.visitVariableDeclaration = function(node) {
		this.context.temps = this.context.temps.concat(node.variables);
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
		var output = "(function() {\n";
		output += "var _dict = Object.create(null);\n";

		for (var i = 0, j = 1; j < node.values.length; i += 2, j += 2) {
			output += "_dict[" + node.values[i].visit(this) + "] = " + node.values[j].visit(this) + ";\n";
		}

		output += "return _dict;\n";
		output += "})()";

		return output;
	};

	this.visitReturn = function(node) {
		if (this.context.returnContext === "method" ) {
			return "return " + node.value.visit(this);
		}
		else {
			return "throw new STReturnValue(" + node.value.visit(this) + ")";
		}
	};

	this.convertIfEmpty = function(receiver, node) {
		var block = node.args[0];
		var output;
		output  = "(function(_receiver$) {\n";
		output += "return (_receiver$.length === 0) ? (" + block.visit(this) + ")() : null;\n";
		output += "})(" + receiver + ")";
		return output;
	};

	this.convertIfNotEmpty = function(receiver, node) {
		var block = node.args[0];

		if (block.params.length > 1) {
			throw Blether.ParseError({
				"line": block.line,
				"column": block.column,
				"msg": "Cannot supply more than one argument to an ifNotEmpty: block"
			});
		}

		var output;
		output  = "(function(_receiver$) {\n";
		output += "return (_receiver$.length > 0) ? (" + block.visit(this);
		output += ")(" + (block.params.length === 0 ? "" : "_receiver$") + ") : null;\n";
		output += "})(" + receiver + ")";
		return output;
	};

	this.convertIfNil = function(receiver, node) {
		var block = node.args[0];
		var output;
		output  = "(function(_receiver$) {\n";
		output += "return (typeof _receiver$ === \"undefined\" || _receiver$ === null) ";
		output += "? (" + block.visit(this) + ")() : null;\n";
		output += "})(" + receiver + ")";
		return output;
	};

	this.convertIfNotNil = function(receiver, node) {
		var block = node.args[0];

		if (block.params.length > 1) {
			throw Blether.ParseError({
				"line": block.line,
				"column": block.column,
				"msg": "Cannot supply more than one argument to an ifNotNil: block"
			});
		}

		var output;
		output  = "(function(_receiver$) {\n";
		output += "return (typeof _receiver$ !== \"undefined\" && _receiver$ !== null) ";
		output += "? (" + block.visit(this) + ")(";
		output += block.params.length === 0 ? "" : "_receiver$";
		output += ") : null;\n";
		output += "})(" + receiver + ")";
		return output;
	};

	// FIXME: Collapse simple blocks into a ternary expression instead of
	// using a function
	this.convertIfNilIfNotNil = function(receiver, node) {

		var ifNilBlock    = node.args[0];
		var ifNotNilBlock = node.args[1];

		if (ifNilBlock.params.length !== 0) {
			throw Blether.ParseError({
				"line": ifNilBlock.line,
				"column": ifNilBlock.column,
				"msg": "ifNil: block does not take parameters"
			});
		}

		if (ifNotNilBlock.params.length > 1) {
			throw Blether.ParseError({
				"line": ifNotNilBlock.line,
				"column": ifNotNilBlock.column,
				"msg": "ifNotNil: block takes at most one parameters"
			});
		}

		var output = "(function(_receiver) {\n";
		output += "if (typeof _receiver === \"undefined\") {\n";
		output += "return (" + ifNilBlock.visit(this) + ")();\n";
		output += "}\n";
		output += "else {\n";
		output += "return (" + ifNotNilBlock.visit(this) + ")(";
		if (ifNotNilBlock.params.length === 1) {
			output += "_receiver";
		}
		output += ");\n";
		output += "}\n";
		output += "})(" + receiver + ")";

		return output;
	};

	this.convertNew = function(receiver) {
		return "new " + receiver + "()";
	};

	this.convertSuper = function(selector, node) {
		var self = this;
		if (typeof this.context.currentClass !== "undefined") {
			var output = this.context.currentClass + ".__super__." + selector + ".call(self";
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
		loopCondition = invert + "(" + node.receiver.visit(this) + ")()";

		var output = "while (" + loopCondition + ") {\n";
		output += "(" + node.args[0].visit(this) + ")();\n";
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
			return node.receiver.visit(this) + " ? null : (" + node.args[0].visit(this) + ")()";
		}
		else {
			return node.receiver.visit(this) + " ? (" + node.args[0].visit(this) + ")() : null";
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

		var invert = node.selector === "ifFalse:ifTrue:" ? "!" : "";
		
		if (invert) {
			return node.receiver.visit(this) + " ? (" + node.args[1].visit(this) + ")() : (" + node.args[0].visit(this) + ")()";
		}
		else {
			return node.receiver.visit(this) + " ? (" + node.args[0].visit(this) + ")() : (" + node.args[1].visit(this) + ")()";
		}
	};
};


module.exports = {
	"translate": function(text, _opts) {
		var opts = _opts || { include_runtime: true };

		var ast = BletherParser.parse(text);

		var runtime = "";

		if (opts.include_runtime) {
			var path = require("path");
			var fs   = require("fs");

			var runtimePath = path.join(path.dirname(fs.realpathSync(__filename)), "runtime.js");
			runtime = fs.readFileSync(runtimePath).toString();
		}

		var translation = new BletherTranslator().visit(ast);

		return runtime + translation;
	}
};
