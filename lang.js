var Blether = {};

Blether.String = function(string) {
	this._type = 'String';
	this.value = string;
};

Blether.String.prototype.toString = function() { return this.value };
Blether.String.prototype.visit = function(visitor) { return visitor.visitString(this) };

//------------------------------------------------------------------------------

Blether.Symbol = function(string) {
	this._type = 'Symbol';
	this.value = string;
};

Blether.Symbol.prototype.toString = function() { return this.value };
Blether.Symbol.prototype.visit = function(visitor) { return visitor.visitSymbol(this) };

//------------------------------------------------------------------------------

Blether.Number = function(string) {
	this._type = 'Number';
	this.value = string;
};

Blether.Number.prototype.toString = function() { return this.value };
Blether.Number.prototype.visit = function(visitor) { return visitor.visitNumber(this) };

//------------------------------------------------------------------------------

Blether.Array = function(array) {
	this._type = 'Array';
	this.value = array;
};

Blether.Array.prototype = [];
Blether.Array.prototype.visit = function(visitor) { return visitor.visitArray(this) };

//------------------------------------------------------------------------------

Blether.DynamicArray = function(array) {
	this._type = 'DynamicArray';
	this.value = array || [];
};

Blether.DynamicArray.prototype = new Blether.Array();
Blether.DynamicArray.prototype.visit = function(visitor) { return visitor.visitDynamicArray(this) };

//------------------------------------------------------------------------------

Blether.DynamicDictionary = function(dict) {
	this._type = 'DynamicDictionary';
	this.value = dict || {};
};

Blether.DynamicDictionary.prototype = {};
Blether.DynamicDictionary.prototype.visit = function(visitor) { return visitor.visitDynamicDictionary(this) };

//------------------------------------------------------------------------------

Blether.UndefinedObject = function() {
	this._type = 'UndefinedObject';
};

Blether.UndefinedObject.prototype = {};
Blether.UndefinedObject.prototype.visit = function(visitor) { return visitor.visitUndefinedObject(this) };

//------------------------------------------------------------------------------

Blether.Boolean = function(bool) {
	this._type = 'Boolean';
	this.value = bool;
};

Blether.Boolean.prototype = new Boolean();
Blether.Boolean.prototype.visit = function(visitor) { return visitor.visitUndefinedObject(this) };

//------------------------------------------------------------------------------

Blether.Variable = function(id) {
	this._type = 'Variable';
	this.value = id;
};

Blether.Variable.prototype = {};
Blether.Variable.prototype.visit = function(visitor) { return visitor.visitVariable(this) };

//------------------------------------------------------------------------------

Blether.UnaryPattern = function(selector) {
	this._type = 'UnaryPattern';
	this.selector = selector;
};

Blether.UnaryPattern.prototype = {};
Blether.UnaryPattern.prototype.visit = function(visitor) { return visitor.visitUnaryPattern(this) };

//------------------------------------------------------------------------------

Blether.BinaryPattern = function(selector, arg) {
	this._type = 'BinaryPattern';
	this.selector = selector;
	this.arg = arg;
};

Blether.BinaryPattern.prototype = {};
Blether.BinaryPattern.prototype.visit = function(visitor) { return visitor.visitBinaryPattern(this) };

//------------------------------------------------------------------------------

Blether.KeywordPattern = function(keywordPairs) {
	this._type = 'KeywordPattern';
	this.pairs = keywordPairs;
};

Blether.KeywordPattern.prototype = {};
Blether.KeywordPattern.prototype.visit = function(visitor) { return visitor.visitKeywordPattern(this) };

//------------------------------------------------------------------------------

Blether.Assignment = function(variable, expression) {
	this._type = 'Assignment';
	this.variable = variable;
	this.expression = expression;
};

Blether.Assignment.prototype = {};
Blether.Assignment.prototype.visit = function(visitor) { return visitor.visitAssignment(this) };

//------------------------------------------------------------------------------

Blether.Return = function(expression) {
	this._type = 'Return';
	this.value = expression;
};

Blether.Return.prototype = {};
Blether.Return.prototype.visit = function(visitor) { return visitor.visitReturn(this) };

//------------------------------------------------------------------------------

Blether.Sequence = function(temps, statements) {
	this._type = 'Sequence';
	this.temps      = temps      || [];
	this.statements = statements || [];
};

Blether.Sequence.prototype = {};
Blether.Sequence.prototype.visit = function(visitor) { return visitor.visitSequence(this) };

//------------------------------------------------------------------------------

Blether.Block = function(paramList, sequence) {
	this._type = 'Block';
	this.params = paramList;
	this.sequence = sequence;
};

Blether.Block.prototype = {};
Blether.Block.prototype.visit = function(visitor) { return visitor.visitBlock(this) };

//------------------------------------------------------------------------------

Blether.Send = function(selector, args) {
	this._type = 'Send';
	this.selector = selector;
	this.args = args;
	this.receiver = null;
};


Blether.Send.prototype = {};
Blether.Send.prototype.visit = function(visitor) { return visitor.visitSend(this) };
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

Blether.Cascade = function(receiver, cascade) {
	this._type = 'Cascade';
	this.receiver = receiver;
	this.cascade = cascade;
};

Blether.Cascade.prototype = {};
Blether.Cascade.prototype.visit = function(visitor) { return visitor.visitCascade(this) };

//------------------------------------------------------------------------------

Blether.JsStatement = function(javascript) {
	this._type = 'JsStatement';
	this.javascript = javascript;
};

Blether.JsStatement.prototype = {};
Blether.JsStatement.prototype.visit = function(visitor) { return visitor.visitJsStatement(this) };

//------------------------------------------------------------------------------

Blether.Method = function(selector, sequences) {
	this._type = 'Method';
	this.selector = selector;
	this.sequences = sequences;
};

Blether.Method.prototype = {};
Blether.Method.prototype.visit = function(visitor) { return visitor.visitMethod(this) };

//------------------------------------------------------------------------------

Blether.Comment = function(comment) {
	this._type = 'Comment';
	this.comment = comment;
};

Blether.Comment.prototype = {};
Blether.Comment.prototype.visit = function(visitor) { return visitor.visitComment(this) };

//------------------------------------------------------------------------------

Blether.MethodDeclaration = function(className, body) {
	this._type = 'MethodDeclaration';
	this.className = className;
	this.body = body;
};

Blether.MethodDeclaration.prototype = {};
Blether.MethodDeclaration.prototype.visit = function(visitor) { return visitor.visitMethodDeclaration(this) };

//------------------------------------------------------------------------------

Blether.ClassDeclaration = function(className, varNames) {
	this._type = 'ClassDeclaration';
	this.className = className;
	this.varNames = varNames;
};

Blether.ClassDeclaration.prototype = {};
Blether.ClassDeclaration.prototype.visit = function(visitor) { return visitor.visitClassDeclaration(this) };

//------------------------------------------------------------------------------

Blether.Program = function(elements) {
	this._type = 'Program';
	this.elements = elements;
};

Blether.Program.prototype = {};
Blether.Program.prototype.visit = function(visitor) { return visitor.visitProgram(this) };


module.exports = Blether;
