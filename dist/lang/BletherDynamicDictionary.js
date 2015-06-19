/* lib/lang/BletherDynamicDictionary.st*/
function BletherDynamicDictionary(_values) {
this.values = _values;
}

BletherDynamicDictionary.prototype = Object.create(BletherNode.prototype);
BletherDynamicDictionary.prototype.constructor = BletherNode;

BletherDynamicDictionary.prototype.visit = function(aVisitor) {
var self = this;
return aVisitor.visitDynamicDictionary(self);
};

