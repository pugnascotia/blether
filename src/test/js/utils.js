var assert = require("assert");
var testUtils = {};

var blether = require("../../../target/blether.js");

testUtils.getArtifacts = function(testName) {
	var path = require("path");
	var fs = require("fs");

	var utilsPath = fs.realpathSync(__filename);
	var testDir = path.join(path.dirname(utilsPath), "..", "..", "..", "test");

	var source   = fs.readFileSync(path.join(testDir, testName + ".st")).toString();
	var expected = fs.readFileSync(path.join(testDir, testName + ".result")).toString();

	return { source: source, expected: expected};
};

testUtils.generateAndCompare = function(artifactPrefix) {
	var artifacts = testUtils.getArtifacts(artifactPrefix);
	// Do not prefix generated source with the required runtime
	var actual = blether.translate(artifacts.source, { include_runtime: false });

	assert.equal(artifacts.expected, actual);
};

module.exports = testUtils;
