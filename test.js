if (global.v8debug) {
	global.v8debug.Debug.setBreakOnException(); // speaks for itself
}

var fs = require("fs");
var blether = require("./target/blether.js");
var util = require('util');

var text = fs.readFileSync("test.st").toString();

try {
	var js = blether.translate(text);
	console.log(js);
}
catch (e) {
	console.log("Failed at line [" + e.line + "], column [" + e.column + "]: " + e);
	console.log(e);
	process.exit();
}
