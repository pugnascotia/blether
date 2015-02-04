var Translator = function() {
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

	this.visit = function(something) {
		return something.visit(this);
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

		output += "var " + node.className.visit(this) + " = (function(";

		if (hasSuper) {
			output += "_super";
		}
		
		output += ") {\n";

		if (hasSuper) {
			output += "_extends(" + node.className + ", _super);\n\n";
		}

		output += "function " + node.className + "() {\n";
		if (hasSuper) {
			output += "    return " + node.className + ".__super__.constructor.apply(this, arguments);\n";
		}
		output += "};\n\n";

		var instanceNames = node.varNames.value;

		for (var i = 0; i < instanceNames.length; i++) {
			output += "this." + instanceNames[i].visit(this) + " = null;\n";
		}

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

		var output = node.className + ".prototype." + methodName + " = " + node.body.visit(this) + ";\n\n";

		return output;
	};

	this.visitString = function(node) {
		return "\"" + node.value.replace(/"/g, "\\\"") + "\"";
	};

	this.visitMethod = function(node) {

		var output = "function(" + node.selector.visit(this)[1].join(", ") + ") {\n";
		output += "var _self = this;\n";

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
		return [convertSelector(keywords.join("_").replace(/:/g, "$")), params];
	};

	this.visitSymbol = function(node) {
		return node.value;
	};

	this.visitStatement = function(node) {
		var output = node.expression.visit(this);

		output += (output[output.length -1] !== ";") ? ";\n" : "\n";

		return output;
	};

	this.visitSequence = function(node) {
		var self = this;

		var output = "";

		node.temps.forEach(function(each) {
			output += "var " + each + ";\n";
		});

		node.statements.forEach(function(each, index, array) {
			output += (index === array.length - 1 ? "return " : "") + each.visit(self) + ";\n";
		});

		return output;
	};

	this.visitSend = function(node) {
		var self = this;
		var output = "";

		var receiver = node.receiver.visit(this);
		var selector = convertSelector(node.selector);

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

			default:
				output = receiver + "." + selector + "(";

				//if (typeof node.args !== "undefined") {
					output += node.args.map(function(each) { return each.visit(self); }).join(", ");
				//}
				output += ")";

				break;
		}
		
		return output;
	};

	this.visitAssignment = function(node) {
		return node.variable.visit(this) + " = " + node.expression.visit(this);
	};

	this.visitNumber = function(node) {
		return node.value.toString();
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

		var output = "var _receiver = " + node.receiver.visit(this) + ";\n";

		output += node.messages.map(function(each) {
			each.receiver = new Blether.Variable("_receiver");
			return each.visit(self);
		}).join(";\n");

		return output;
	};

	this.visitJsStatement = function(node) {
		return node.javascript.trim();
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

		// TODO check type of block

		if (block.params.length > 1) {
			throw "Cannot supply more than one argument to an ifNotNil: block";
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
			throw "ifNil: block does not take parameters";
		}

		if (ifNotNilBlock.params.length > 1) {
			throw "ifNotNil: block takes at most one parameter";
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
};


module.exports = {
	"translate": function(text) {
		var ast = BletherParser.parse(text);

		// var util = require("util");
		// console.log(util.inspect(ast, false, null));

		return new Translator().visit(ast);
	}
};
