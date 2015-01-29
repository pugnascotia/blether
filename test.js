var fs = require("fs");
var parser = require("./parser.js");
var util = require('util');

var text = fs.readFileSync("test.st").toString();

console.log(util.inspect(parser.parse(text), false, null));
