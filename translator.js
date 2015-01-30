var Translator = function() {

	this.visit = function(something) {
		return something.visit(this);
	};

	this.visitProgram = function(node) {
		var self = this;
		return node.elements.reduce(function(output, childNode) {
			return output + childNode.visit(self);
		}, '');
	};

	this.visitClassDeclaration = function(node) {
		var output = "var " + node.className.visit(this) + " = function() {\n";

		for (var i = 0; i < node.varNames.length; i++) {
			output += "this." + node.varNames[i].visit(this) + " = null;\n";
		}

		output += "};\n\n";

		// TODO: Setup super access
		output += node.className.visit(this) + ".prototype = new " +
			(node.superClass ? node.superClass : "Object") + "();\n\n";

		return output;
	};

	this.visitMethodDeclaration = function(node) {
		var methodName = node.body.selector.visit(this)[0];

		var output = node.className + ".prototype." + methodName + " = " + node.body.visit(this) + ";\n\n";

		return output;
	};

	this.visitString = function(node) {
		return '"' + node.value.replace(/"/g, "\\\"") + '"';
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
		return [keywords.join("_").replace(/:/g, ''), params];
	};

	this.visitSymbol = function(node) {
		return node.value;
	};

	this.visitSequence = function(node) {
		var output = '';

		output += node.temps.map(function(each) { return "var " + each + ";\n" }).join("");

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
			output += node.args.map(function(each) { return each.visit(self) }).join(", ");
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

	this.visitUndefinedObject = function(node) {
		return "null";
	};

	this.visitBlock = function(node) {
		return "function (" + node.params.join(", ") + ") {\n" +
			node.sequence.visit(this) +
		"\n}";
	};

};

function translate(ast) {
	return new Translator().visit(ast);
}


module.exports = { "translate": translate };
