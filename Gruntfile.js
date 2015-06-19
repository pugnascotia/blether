module.exports = function(grunt) {
	"use strict";

	// Time our tasks in a spangly way
	require("time-grunt")(grunt);

	// Force use of Unix newlines
	grunt.util.linefeed = "\n";

    grunt.registerMultiTask("blether", "Run the blether compiler", function() {
        var args = [ "--runtime=false", "--verbose", "-o", "./dist/runtime", "-c"].concat(this.filesSrc);

        var result = require("child_process").spawnSync("./bin/blether", args);

        console.log(result.stdout.toString());
        console.log(result.stderr.toString());

        if (result.status !== 0) {
            throw new Error("blether invocation failed");
        }
    });

	// 1. All configuration goes here
	grunt.initConfig({
		pkg: grunt.file.readJSON("package.json"),

        clean: [ "dist" ],

		// Configure the grunt-peg plugin
		peg: {
			grammer: {
				src: "lib/parser.pegjs",
				dest: "dist/parser.js",
				options: { exportVar: "var BletherParser" }
			}
		},

		// Configure the grunt-contrib-jshint plugin
		jshint: {
			files: [ "lib/*.js", "lib/test/*.js", "test/**/*.js", "Gruntfile.js", "bin/blether" ],
			options: {
				jshintrc: ".jshintrc"
			}
		},

        blether: {
            files: "lib/runtime/*.st"
        },

		concat: {
			dist: {
				src: [
					"lib/lang.js",
					"dist/parser.js",
					"lib/selector.js",
					"lib/tree_modifier.js",
					"lib/return_operator_visitor.js",
					"lib/translator.js",
				],
				dest: "dist/blether.js"
			},
            runtime: {
                src: [
                    "dist/runtime/*.js",
                    "lib/runtime.js"
                ],
                dest: "dist/runtime.js"
            }
		},

		// Configure the grunt-contrib-uglify plugin
		uglify: {
			options: {
				banner: "/*! <%= pkg.name %> <%= grunt.template.today(\"dd-mm-yyyy\") %> */\n"
			},
			blether: {
				src: "dist/blether.js",
				dest: "dist/blether.min.js"
			},
			runtime: {
				src: "dist/runtime.js",
				dest: "dist/runtime.min.js"
			}
		},

		mochaTest: {
			test: {
				src: [ "test/**/*.js" ],
				options: {
					"reporter": "spec",
					"captureFile": "results.txt",
					"quiet": false,
					"clearRequireCache": false
				}
			}
		},

		watch: {
			sources: {
				files: [ "lib/**/*.js", "lib/**/*.pegjs" ],
				tasks: [ "build", "test" ]
			},
			tests: {
				files: [ "test/**/*" ],
				tasks: [ "jshint", "test" ]
			},
			project: {
				files: [ "Gruntfile.js", ".jshintrc" ],
				tasks: [ "default" ]
			}
		}
	});

	grunt.loadNpmTasks("grunt-contrib-concat");
	grunt.loadNpmTasks("grunt-contrib-jshint");
	grunt.loadNpmTasks("grunt-contrib-uglify");
	grunt.loadNpmTasks("grunt-peg");
	grunt.loadNpmTasks("grunt-contrib-watch");
	grunt.loadNpmTasks("grunt-mocha-test");
	grunt.loadNpmTasks("grunt-contrib-clean");

	grunt.registerTask("build", [ "jshint", "peg", "blether", "concat"]);

	grunt.registerTask("test", [ "mochaTest" ]);

	// Tell Grunt what to do when we type "grunt" into the terminal.
	grunt.registerTask("default", [ "build", "test", "uglify" ]);
};
