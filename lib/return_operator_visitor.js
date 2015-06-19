/*exported BletherReturnOperatorVisitor*/
var BletherReturnOperatorVisitor = function() {

	function returnFalse() { return false }
	function returnTrue() { return true }

    this.visit = function(node) {
		return node.visit(this);
	};

    this.visitProgram = function(node) {
		for (var i = 0; i < node.elements.length; i++) {
			if (node.elements[i].visit(this)) {
				return true;
			}
		}
		return false;
	};

    this.visitClassDeclaration = function(node) {
		for (var method in node.methods) {
			if (node.methods.hasOwnProperty(method)) {
				if (node.methods[method].visit(this)) {
					return true;
				}
			}
		}
		return false;
	};

    this.visitMethodDeclaration = function(node) {
		return node.body.visit(this);
	};

    this.visitMethod = function(node) {
		for (var i = 0; i < node.sequences.length; i++) {
			if (node.sequences[i].visit(this)) {
				return true;
			}
		}

		return false;
	};

    this.visitStatement = function(node) {
		return node.expression.visit(this);
	};

    this.visitSequence = function(node) {
		for (var i = 0; i < node.statements.length; i++) {
			if (node.statements[i].visit(this)) {
				return true;
			}
		}

		return false;
	};

    this.visitSend = function(node) {
		// This happens with a Cascade
		if (node.receiver !== null) {
			if (node.receiver.visit(this)) {
				return true;
			}
		}

		for (var i = 0; i < node.args.length; i++) {
			if (node.args[i].visit(this)) {
				return true;
			}
		}

		return false;
	};

    this.visitAssignment = function(node) {
		return node.expression.visit(this);
	};

    this.visitBlock = function(node) {
		return node.sequence.visit(this);
	};

    this.visitCascade = function(node) {
		if (node.receiver.visit(this)) {
			return true;
		}

		for (var i = 0; i < node.messages.length; i++) {
			if (node.messages[i].visit(this)) {
				return true;
			}
		}

		return false;
	};

    this.visitDynamicArray = function(node) {
		for (var i = 0; i < node.values.length; i++) {
			if (node.values[i].visit(this)) {
				return true;
			}
		}

		return false;
	};

    this.visitDynamicDictionary = function(node) {
		for (var key in node.values) {
			if (node.values.hasOwnProperty(key)) {
				if (node.values[key].visit(this)) {
					return true;
				}
			}
		}
		return false;
	};

	this.visitReturn = returnTrue;

    this.visitBinaryPattern = returnFalse;
    this.visitKeywordPattern = returnFalse;
    this.visitString = returnFalse;
    this.visitSymbol = returnFalse;
    this.visitUnaryPattern = returnFalse;
    this.visitNumber = returnFalse;
    this.visitUndefinedObject = returnFalse;
    this.visitBoolean = returnFalse;
    this.visitArray = returnFalse;
    this.visitJsStatement = returnFalse;
    this.visitVariableDeclaration = returnFalse;
    this.visitVariable = returnFalse;
    this.visitClassName = returnFalse;
};
