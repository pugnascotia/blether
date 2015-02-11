var assert = require("assert");
var util = require("util");

var blether = require("../../../target/blether.js");

var testUtils = {};

function normalise(s) {
	return s.trim() + "\n";
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

	assert.equal(normalise(artifacts.expected), normalise(actual));
};

testUtils.expectSyntaxError = function(artifactPrefix) {

	assert.throws(function() {
		blether.translate(testUtils.getSource(artifactPrefix));
	},
	/Expected/);
};

testUtils.expectParseError = function(artifactPrefix, expectedMessage) {

	try {
		blether.translate(testUtils.getSource(artifactPrefix));
		throw "Expected exception to be thrown";
	}
	catch (e) {
		assert.equal("Blether Parse Error", e.name);
		assert.equal(expectedMessage, e.message);
	}
};

module.exports = testUtils;
