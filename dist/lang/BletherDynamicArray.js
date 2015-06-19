/* lib/lang/BletherDynamicArray.st*/
function BletherDynamicArray(_values) {
this.values = _values;
}

BletherDynamicArray.prototype = Object.create(BletherArray.prototype);
BletherDynamicArray.prototype.constructor = BletherArray;

BletherDynamicArray.prototype.visit = function(aVisitor) {
var self = this;
return aVisitor.visitDynamicArray(self);
};

