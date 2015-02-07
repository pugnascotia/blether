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

		var methods = node.getMethods();
		
		for (var prop in methods) {
			if (methods.hasOwnProperty(prop)) {
				output += methods[prop].visit(self);
			}
		}

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
		
		this.currentClass = node.className;
		output += node.body.visit(this);
		this.currentClass = null;

		output += ";\n\n";

		return output;
	};

	this.visitString = function(node) {
		return "new STString(\"" + node.value.replace(/"/g, "\\\"") + "\")";
	};

	this.visitMethod = function(node) {

		var output = "function(" + node.selector.visit(this)[1].join(", ") + ") {\n";
		output += "var self = this;\n";

		var self = this;

		node.sequences.forEach(function(each) {
			output += each.visit(self);
		});

		output += "}";

		// node.sequences

		return output;
	};

	this.visitUnaryPattern = function(node) {
		return[convertSelector(node.selector), []];
	};

	this.visitBinaryPattern = function(node) {
		return [convertSelector(node.selector), [node.arg]];
	};

	this.visitKeywordPattern = function(node) {
		var keywords = [];
		var params = [];
		var i = 0;
		for(i = 0; i < node.pairs.length; i++){
		    keywords.push(node.pairs[i].key);
		}
		for(i = 0; i < node.pairs.length; i++){
		    params.push(node.pairs[i].arg);
		}
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

		node.temps.forEach(function(each) {
			output += "var " + each + ";\n";
		});

		var needsReturn = true;

		node.statements.forEach(function(each, index, array) {
			needsReturn = !(index === array.length - 1 && each._type === "Return");
			output += each.visit(self) + ";\n";
		});

		if (needsReturn) {
			output += "return self;\n";
		}

		return output;
	};

	this.visitSend = function(node) {
		var self = this;

		var receiver = node.receiver.visit(this);

		switch (receiver) {
			case "super":
				return this.convertSuper(convertSelector(node.selector), node);
		}

		var output = "";

		switch (node.selector) {

			case "isNil":
				output += "(typeof (" + receiver + ") === \"undefined\")";
				break;

			case "notNil":
				output += "(typeof (" + receiver + ") !== \"undefined\")";
				break;

			case "isEmpty":
				output += "(" + receiver + ".length === 0)";
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

			case "asJQuery":
				output = this.convertJQuery(node);
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
		return node.value;
	};

	this.visitUndefinedObject = function() {
		return "null";
	};

	this.visitBoolean = function(node) {
		return node.value.toString();
	};

	this.visitBlock = function(node) {
		return "function (" + node.params.join(", ") + ") {\n" +
			node.sequence.visit(this) +
		"}";
	};

	this.visitCascade = function(node) {
		var self = this;

		var output = "(function(_) {\n";

		node.messages.forEach(function(each, index, array) {
			each.receiver = new Blether.Variable("_");
			output += (index === array.length - 1 ? "return " : "") + each.visit(self) + ";\n";
		});

		output += "})(" + node.receiver.visit(this) + ")";

		return output;
	};

	this.visitJsStatement = function(node) {
		return node.javascript.trim();
	};

	this.visitVariableDeclaration = function(node) {
		return "var " + node.variables.join(", ") + ";\n";
	};

	this.visitArray = function(node) {
		var self = this;
		return "[" + node.value.map(function(each) { return each.visit(self) }).join(",") + "]";
	};

	this.visitDynamicArray = function(node) {
		var self = this;
		return "[" + node.values.map(function(each) { return each.visit(self) }).join(", ") + "]";
	};

	this.visitDynamicDictionary = function(node) {
		var output = "{\n";

		var values = [];

		for (var i = 0, j = 1; j < node.values.length; i += 2, j += 2) {
			values.push(node.values[i].visit(this) + ": " + node.values[j].visit(this));
		}

		output += values.join(",\n");

		output += "\n}";

		return output;
	};

	this.visitReturn = function(node) {
		return "return " + node.value.visit(this);
	};

	this.convertIfNil = function(receiver, node) {
		var block = node.args[0];
		var output;
		output  = "(function() {";
		output += "var _receiver = " + receiver + ";\n";
		output += "if (typeof _receiver === \"undefined\") {\n";
		output += "return (" + block.visit(this) + ")();\n";
		output += "})()\n";
		return output;
	};

	this.convertIfNil = function(receiver, node) {
		var block = node.args[0];

		// TODO check that block is indeed a Block

		if (block.params.length > 1) {
			throw Blether.ParseError({
				"line": block.line,
				"column": block.column,
				"msg": "Cannot supply more than one argument to an ifNotNil: block"
			});
		}

		var output;
		output  = "(function() {";
		output += "var _receiver = " + receiver + ";\n";
		output += "if (typeof _receiver !== \"undefined\") {\n";

		output += "return (" + block.visit(this) + ")(";
		if (block.params.length === 1) {
			output += "_receiver";
		}
		output += ");\n";
		output += "}})()\n";
		return output;
	};

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

		var output = "(function() {";
		output += "var _receiver = " + receiver + ";\n";
		output += "if (typeof _receiver === \"undefined\") {\n";
		output += "return (" + ifNilBlock.visit(this) + ")();";
		output += "}\n";
		output += "else {\n";
		output += "return (" + ifNotNilBlock.visit(this) + ")(";
		if (ifNotNilBlock.params.length === 1) {
			output += "_receiver";
		}
		output += ");\n";
		output += "}\n";
		output += "})()\n";

		return output;
	};

	this.convertNew = function(receiver) {
		return "new " + receiver + "()";
	};

	this.convertSuper = function(selector, node) {
		var self = this;
		if (typeof this.currentClass !== "undefined") {
			var output = this.currentClass + ".__super__." + selector + ".call(self";
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

	this.convertJQuery = function(node) {
		return "jQuery(" + node.receiver.visit(this) + ")";
	};
};


module.exports = {
	"translate": function(text) {
		var ast = BletherParser.parse(text);

		// var util = require("util");
		// console.log(util.inspect(ast, false, null));

		var modifiedAst = new BletherTreeModifier().visit(ast);

		// var util = require("util");
		// console.log(util.inspect(modifiedAst, false, null));

		var path = require("path");
		var fs   = require("fs");

		var runtimePath = path.join(path.dirname(fs.realpathSync(__filename)), "adaptors.js");
		var runtime = fs.readFileSync(runtimePath).toString();

		var translation = new BletherTranslator().visit(modifiedAst);

		return runtime + translation;
	}
};
