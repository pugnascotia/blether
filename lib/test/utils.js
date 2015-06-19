var assert = require("assert");
var child_process = require("child_process");
var fs = require("fs");
var path = require("path");
var util = require("util");
var _eval = require("eval");

var blether = require("../../dist/blether.js");

var testUtils = {};

function normalise(s) {
	return s.trim() + "\n";
}

testUtils.executeBlether = function(testArtifactId) {
    var testDir = testUtils.resolveTestDir();
	var source   = path.join(testDir, testArtifactId + ".st");

    return child_process.execSync("bin/blether", ["-x", source], {
        env: {
            "NODE_PATH": ".:/usr/local/lib/node_modules/"
        }
    });
};

testUtils.resolveTestDir = function() {
	var utilsPath = fs.realpathSync(__filename);
	return path.join(path.dirname(utilsPath), "..", "..", "test");
};

testUtils.getArtifacts = function(artifactPrefix) {
    var testDir = testUtils.resolveTestDir();

	var source   = fs.readFileSync(path.join(testDir, artifactPrefix + ".st")).toString();
	var expected = normalise(fs.readFileSync(path.join(testDir, artifactPrefix + ".result")).toString());

	return { source: source, expected: expected};
};

testUtils.getSource = function(artifactPrefix) {
    var testDir = testUtils.resolveTestDir();
	var source   = fs.readFileSync(path.join(testDir, artifactPrefix + ".st")).toString();

	return source;
};

testUtils.generateAndCompare = function(artifactPrefix, opts) {

    if (opts == null) {
        opts = { execute: true };
    }

	var artifacts = testUtils.getArtifacts(artifactPrefix);

	var actual;

	// Do not prefix generated source with the required runtime
	try {
		actual = normalise(blether.translate(artifacts.source));
	}
	catch (e) {
		actual = e.stack == null ? normalise(util.inspect(e, null, false)) : e.stack;
	}

	try {
		assert.equal(actual, artifacts.expected);
	}
	catch (err) {
		err.expected = artifacts.expected;
		err.actual = actual;
		err.showDiff = true;
		throw err;
	}

    if (opts.execute === true) {
        try {
			var executableSource = blether.runtime() + blether.translate(artifacts.source);
            /* Re-gen code with runtime so that we can run it successfully */
            _eval(executableSource, artifactPrefix, null, true);
        }
        catch (err) {
            throw new Error(artifactPrefix + ": generated JavaScript failed to execute: [" + err + "]");
        }
    }
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
		//console.dir(e);
		assert.equal(e.name, "Blether Parse Error", util.inspect(e, null, false));
		assert.equal(e.message, expectedMessage);
	}
};

module.exports = testUtils;
