var Blether = {};

Blether.String = function(string) {
	this.value = string;
};

Blether.String.prototype = new String();

//------------------------------------------------------------------------------

Blether.Number = function(string) {
	this.value = string;
};

Blether.Number.prototype = new Number();

//------------------------------------------------------------------------------

Blether.Array = function(array) {
	this.value = array;
};

Blether.Array.prototype = [];

//------------------------------------------------------------------------------

Blether.DynamicArray = function(array) {
	this.value = array || [];
};

Blether.DynamicArray.prototype = new Blether.Array();

//------------------------------------------------------------------------------

Blether.DynamicDictionary = function(dict) {
	this.value = dict || {};
};

Blether.DynamicDictionary.prototype = {};

//------------------------------------------------------------------------------

Blether.UndefinedObject = function() {
};

//------------------------------------------------------------------------------

Blether.Boolean = function(bool) {
	this.value = bool;
};

Blether.Boolean.prototype = new Boolean();

//------------------------------------------------------------------------------

Blether.Variable = function(id) {
	this.value = id;
};

//------------------------------------------------------------------------------

Blether.KeywordPattern = function(keywordPairs) {
	this.value = keywordPairs;
};

//------------------------------------------------------------------------------

Blether.Assignment = function(variable, expression) {
	this.variable = variable;
	this.expression = expression;
};

//------------------------------------------------------------------------------

Blether.Return = function(expression) {
	this.value = expression;
};

//------------------------------------------------------------------------------

Blether.Sequence = function(temps, sequence) {
	this.tempsValue = temps;
	this.seqValue = sequence;
};

//------------------------------------------------------------------------------

Blether.Block = function(paramList, sequence) {
	this.params = paramList;
	this.sequence = sequence;
};

//------------------------------------------------------------------------------

Blether.Send = function(selector, args) {
	this.selector = selector;
	this.args = args;
	this.receiver = null;
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

Blether.Cascade = function(receiver, cascade) {
	this.receiver = receiver;
	this.cascade = cascade;
};

//------------------------------------------------------------------------------

Blether.JsStatement = function(javascript) {
	this.javascript = javascript;
};

//------------------------------------------------------------------------------

Blether.Method = function(selector, args, sequences) {
	this._type = 'Method';
	this.selector = selector;
	this.args = args;
	this.sequences = sequences;
};

//------------------------------------------------------------------------------

Blether.Comment = function(comment) {
	this.comment = comment;
};

//------------------------------------------------------------------------------

Blether.MethodDeclaration = function(className, body) {
	this._type = 'MethodDeclaration';
	this.className = className;
	this.body = body;
};

//------------------------------------------------------------------------------

Blether.ClassDeclaration = function(className, varNames) {
	this._type = 'ClassDeclaration';
	this.className = className;
	this.varNames = varNames;
};


module.exports = Blether;
