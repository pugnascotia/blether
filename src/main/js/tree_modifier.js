/*exported BletherTreeModifier*/
var BletherTreeModifier = function() {

	function identity(node) { return node }

    this.visit = function(node) {
		return node.visit(this);
	};
	
    this.visitProgram = function(node) {
		for (var i = 0; i < node.elements.length; i++) {
			node.elements[i] = node.elements[i].visit(this);
		}
		return node;
	};
	
    this.visitClassDeclaration = function(node) {
		for (var method in node.methods) {
			if (node.methods.hasOwnProperty(method)) {
				node.methods[method] = node.methods[method].visit(this);
			}
		}
		return node;
	};
	
    this.visitMethodDeclaration = function(node) {
		node.body = node.body.visit(this);
		return node;
	};

    this.visitMethod = function(node) {
		node.sequence = node.sequence.visit(this);

		return node;
	};
	
    this.visitStatement = function(node) {
		node.expression = node.expression.visit(this);
		return node;
	};

    this.visitSequence = function(node) {
		for (var i = 0; i < node.statements.length; i++) {
			node.statements[i] = node.statements[i].visit(this);
		}

		return node;
	};
	
    this.visitSend = function(node) {
		// This happens with a Cascade
		if (node.receiver !== null) {
			node.receiver = node.receiver.visit(this);
		}

		for (var i = 0; i < node.args.length; i++) {
			node.args[i] = node.args[i].visit(this);
		}

		return node;
	};
	
    this.visitAssignment = function(node) {
		node.expression = node.expression.visit(this);
		return node;
	};
	
    this.visitVariable = function(node) {
		return node;
	};
	
    this.visitBlock = function(node) {
		node.sequence = node.sequence.visit(this);
		return node;
	};
	
    this.visitCascade = function(node) {
		node.receiver = node.receiver.visit(this);

		for (var i = 0; i < node.messages.length; i++) {
			node.messages[i] = node.messages[i].visit(this);
		}

		return node;
	};
	
    this.visitDynamicArray = function(node) {
		for (var i = 0; i < node.values.length; i++) {
			node.values[i] = node.values[i].visit(this);
		}

		return node;
	};
	
    this.visitDynamicDictionary = function(node) {
		for (var key in node.values) {
			if (node.values.hasOwnProperty(key)) {
				node.values[key] = node.values[key].visit(this);
			}
		}
		return node;
	};

	this.visitReturn = function(node) {
		node.value = node.value.visit(this);
		return node;
	};
	
    this.visitBinaryPattern = identity;
    this.visitKeywordPattern = identity;
    this.visitString = identity;
    this.visitSymbol = identity;
    this.visitUnaryPattern = identity;
    this.visitNumber = identity;
    this.visitUndefinedObject = identity;
    this.visitBoolean = identity;
    this.visitArray = identity;
    this.visitJsStatement = identity;
    this.visitVariableDeclaration = identity;
};
