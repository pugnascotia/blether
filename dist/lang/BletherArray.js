/* lib/lang/BletherArray.st*/
function BletherArray(_values) {
this.values = _values;
};

BletherArray.prototype = Object.create(BletherNode.prototype);
BletherArray.prototype.constructor = BletherNode;

BletherArray.prototype.toString = function() {
var self = this;
return "[".concat(self.values.map(function(each) {
return "\"".concat(each).concat("\"");
}).join(", ")).concat("]");
};

BletherArray.prototype.visit = function(aVisitor) {
var self = this;
return aVisitor.visitArray(self);
};

