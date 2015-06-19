/* lib/lang/BletherNumber.st*/
function BletherNumber(_value) {
this.value = _value;
}

BletherNumber.prototype = Object.create(BletherNode.prototype);
BletherNumber.prototype.constructor = BletherNode;

BletherNumber.prototype.toString = function() {
var self = this;
return self.value;
};

BletherNumber.prototype.visit = function(aVisitor) {
var self = this;
return aVisitor.visitNumber(self);
};

