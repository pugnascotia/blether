if (global.v8debug) {
	global.v8debug.Debug.setBreakOnException(); // speaks for itself
}

var fs = require("fs");
var parser = require("./parser.js");
var translator = require("./translator.js");
var util = require('util');

var text = fs.readFileSync("test.st").toString();

try {
	var ast = parser.parse(text);
}
catch (e) {
	console.log("Failed at line [" + e.line + "], column [" + e.column + "]: " + e);
	console.log(e);
	process.exit();
}

console.log(util.inspect(ast, false, null));

var js = translator.translate(ast);

console.log(js);
