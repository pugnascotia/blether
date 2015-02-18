// Global context object and root of the language types used by the parser
var Blether = {
	"classes": {
		"Object": {
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


/* The language parts */

Blether.Node = function() {
	this.line = null;
	this.column = null;
	this.text = null;
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

Blether.DynamicDictionary = function(dict) {
	this._type = "DynamicDictionary";
	this.values = dict || {};
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

//------------------------------------------------------------------------------

Blether.Statement = function(expression) {
	this._type = "Statement";
	this.expression = expression;
};

Blether.Statement.prototype = new Blether.Node();
Blether.Statement.prototype.visit = function(visitor) { return visitor.visitStatement(this); };

//------------------------------------------------------------------------------

Blether.Block = function(paramList, sequence) {
	this._type = "Block";
	this.params = paramList || [];
	this.sequence = sequence;
};

Blether.Block.prototype = new Blether.Node();
Blether.Block.prototype.visit = function(visitor) { return visitor.visitBlock(this); };
Blether.Block.prototype.invoke = function(visitor) { return visitor.invokeBlock.apply(this, arguments); };

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

//------------------------------------------------------------------------------

Blether.ClassDeclaration = function(className, superClass, varNames) {
	this._type = "ClassDeclaration";
	this.className = className;
	this.superClass = superClass;
	this.varNames = varNames;
	this.methods = {};
	this.context = Blether;
};

Blether.ClassDeclaration.prototype = new Blether.Node();
Blether.ClassDeclaration.prototype.visit = function(visitor) { return visitor.visitClassDeclaration(this); };
Blether.ClassDeclaration.prototype.getMethods = function() {
	return this.context.classes[this.className].methods;
};

//------------------------------------------------------------------------------

Blether.Program = function(elements) {
	this._type = "Program";
	this.elements = elements;
};

Blether.Program.prototype = new Blether.Node();
Blether.Program.prototype.visit = function(visitor) { return visitor.visitProgram(this); };

//------------------------------------------------------------------------------

Blether.VariableDeclaration = function(vars) {
	this._type = "VariableDeclaration";
	this.variables = vars;
};

Blether.VariableDeclaration.prototype = new Blether.Node();
Blether.VariableDeclaration.prototype.visit = function(visitor) { return visitor.visitVariableDeclaration(this); };


