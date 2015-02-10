var assert = require("assert");
var testUtils = {};

var blether = require("../../../target/blether.js");

testUtils.getArtifacts = function(__testPath) {
	var path = require("path");
	var fs = require("fs");

	var testPath = fs.realpathSync(__testPath);
	var testDir = path.dirname(testPath);
	var testName = path.basename(testPath, ".js");

	var source   = fs.readFileSync(path.join(testDir, testName + ".st")).toString();
	var expected = fs.readFileSync(path.join(testDir, testName + ".result")).toString();

	return { source: source, expected: expected};
};

testUtils.generateAndCompare = function(__testPath) {
	var artifacts = testUtils.getArtifacts(__testPath);
	// Do not prefix generated source with the required runtime
	var actual = blether.translate(artifacts.source, { include_runtime: false });

	assert.equal(artifacts.expected, actual);
};

module.exports = testUtils;
