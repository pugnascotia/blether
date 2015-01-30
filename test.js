var fs = require("fs");
var parser = require("./parser.js");
var translator = require("./translator.js");
var util = require('util');

var text = fs.readFileSync("test.st").toString();

var ast = parser.parse(text);

console.log(util.inspect(ast, false, null));

var js = translator.translate(ast);

console.log(js);
