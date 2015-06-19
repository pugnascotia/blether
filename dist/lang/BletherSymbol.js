/* lib/lang/BletherSymbol.st*/
function BletherSymbol(_value) {
this.value = _value;
}

BletherSymbol.prototype = Object.create(BletherNode.prototype);
BletherSymbol.prototype.constructor = BletherNode;

BletherSymbol.prototype.toString = function() {
var self = this;
return self.value;
};

BletherSymbol.prototype.visit = function(aVisitor) {
var self = this;
return aVisitor.visitSymbol(self);
};

