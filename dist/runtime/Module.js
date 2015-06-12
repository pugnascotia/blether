/* lib/runtime/Module.st*/
var Module = (function() {
function Module() {
};

Module.export_as$ = function(key, value) {
exports[key] = value;
};


return Module;

})();

