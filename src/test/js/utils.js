var assert = require("assert");
var util = require("util");

var blether = require("../../../target/blether.js");

var testUtils = {};

function appendNewlineIfNecessary(s) {
	if (typeof s === "string" && s.length !== 0 && s.slice(-1) !== "\n") {
		return s + "\n";
	}
	return s;
}

testUtils.getArtifacts = function(artifactPrefix) {
	var path = require("path");
	var fs = require("fs");

	var utilsPath = fs.realpathSync(__filename);
	var testDir = path.join(path.dirname(utilsPath), "..", "..", "..", "test");

	var source   = fs.readFileSync(path.join(testDir, artifactPrefix + ".st")).toString();
	var expected = fs.readFileSync(path.join(testDir, artifactPrefix + ".result")).toString();

	return { source: source, expected: expected};
};

testUtils.getSource = function(artifactPrefix) {
	var path = require("path");
	var fs = require("fs");

	var utilsPath = fs.realpathSync(__filename);
	var testDir = path.join(path.dirname(utilsPath), "..", "..", "..", "test");

	var source   = fs.readFileSync(path.join(testDir, artifactPrefix + ".st")).toString();

	return source;
};

testUtils.generateAndCompare = function(artifactPrefix) {
	var artifacts = testUtils.getArtifacts(artifactPrefix);

	var actual;

	// Do not prefix generated source with the required runtime
	try {
		actual = blether.translate(artifacts.source, { include_runtime: false });
	}
	catch (e) {
		actual = util.inspect(e, null, false);
	}

	assert.equal(appendNewlineIfNecessary(artifacts.expected), appendNewlineIfNecessary(actual));
};

testUtils.expectSyntaxError = function(artifactPrefix) {

	assert.throws(function() {
		blether.translate(testUtils.getSource(artifactPrefix));
	},
	/Expected/);
};

module.exports = testUtils;
