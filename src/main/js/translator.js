var Translator = function() {
	"use strict";

	var printedClassPrerequisites = false;

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
			if (childNode._type !== "MethodDeclaration") {
				output += childNode.visit(self);
			}
		});

		return output;
	};

	this.visitClassDeclaration = function(node) {
		var self = this;
		var output = "";

		if (!printedClassPrerequisites) {
			output += classPrerequisites;
		}

		output = "var " + node.className.visit(this) + " = (function(";

		var hasSuper = node.superClass !== "Object";

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
		return[node.selector, []];
	};

	this.visitBinaryPattern = function(node) {
		return [node.selector, [node.arg]];
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
		return [keywords.join("_").replace(/:/g, ""), params];
	};

	this.visitSymbol = function(node) {
		return node.value;
	};

	this.visitSequence = function(node) {
		var output = "";

		output += node.temps.map(function(each) { return "var " + each + ";\n"; }).join("");

		var self = this;
		output += node.statements.map(function(each) {
			return each.visit(self) + ";\n";
		}).join("");

		return output;
	};

	this.visitSend = function(node) {
		var self = this;
		var output = node.receiver.visit(this) + "." + node.selector + "(";

		if (typeof node.args !== "undefined") {
			output += node.args.map(function(each) { return each.visit(self); }).join(", ");
		}

		output += ")";
		
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

	this.visitBlock = function(node) {
		return "function (" + node.params.join(", ") + ") {\n" +
			node.sequence.visit(this) +
		"\n}";
	};

};


module.exports = {
	"translate": function(text) {
		var ast = BletherParser.parse(text);
		return new Translator().visit(ast);
	}
};
