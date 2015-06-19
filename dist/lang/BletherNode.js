/* lib/lang/BletherNode.st*/
function BletherNode(_line, _column, _source) {
this.line = _line;
this.column = _column;
this.source = _source;
};

BletherNode.prototype.setLine_column_source$ = function(aLine, aColumn, aSource) {
var self = this;
self.line = aLine;
self.column = aColumn;
self.source = aSource;
return self;
};

BletherNode.prototype.invoke = function() {
return this.visit.apply(this, arguments);
};

BletherNode.prototype.find = function(aBlock) {
var self = this;
return aBlock.__value$(self) ? [self] : [];
};

BletherNode.prototype.isMethod = function() {
var self = this;
return false;
};

