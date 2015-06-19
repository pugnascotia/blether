/* lib/lang/BletherString.st*/
function BletherString(_value) {
this.value = _value;
}

BletherString.prototype = Object.create(BletherNode.prototype);
BletherString.prototype.constructor = BletherNode;

BletherString.prototype.toString = function() {
var self = this;
return self.value;
};

BletherString.prototype.visit = function(aVisitor) {
var self = this;
return aVisitor.visitString(self);
};

