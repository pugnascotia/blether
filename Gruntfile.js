module.exports = function(grunt) {
	"use strict";

	// Time our tasks in a spangly way
	require("time-grunt")(grunt);

	// Force use of Unix newlines
	grunt.util.linefeed = "\n";

	// 1. All configuration goes here
	grunt.initConfig({
		pkg: grunt.file.readJSON("package.json"),

        clean: [ "lib" ],

		peg: {
			grammer: {
				src: "src/main/js/parser.pegjs",
				dest: "lib/parser.js",
				options: { exportVar: "var BletherParser" }
			}
		},

		// Configure the grunt-contrib-jshint plugin
		jshint: {
			files: [ "src/**/*.js", "test/**/*.js", "Gruntfile.js" ],
			options: {
				jshintrc: ".jshintrc"
			}
		},

		// Configure the grunt-peg plugin
		concat: {
			dist: {
				src: [
					"src/main/js/lang.js",
					"lib/parser.js",
					"src/main/js/selector.js",
					"src/main/js/tree_modifier.js",
					"src/main/js/return_operator_visitor.js",
					"src/main/js/translator.js",
				],
				dest: "lib/blether.js"
			}
		},

		copy: {
			runtime: {
				src: "src/main/js/runtime.js",
				dest: "lib/runtime.js"
			}
		},

		// Configure the grunt-contrib-uglify plugin
		uglify: {
			options: {
				banner: "/*! <%= pkg.name %> <%= grunt.template.today(\"dd-mm-yyyy\") %> */\n"
			},
			blether: {
				src: "lib/blether.js",
				dest: "lib/blether.min.js"
			},
			runtime: {
				src: "lib/runtime.js",
				dest: "lib/runtime.min.js"
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
				files: [ "src/**/*.js", "src/**/*.pegjs" ],
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
	grunt.loadNpmTasks("grunt-contrib-copy");
	grunt.loadNpmTasks("grunt-mocha-test");
	grunt.loadNpmTasks("grunt-contrib-clean");

	grunt.registerTask("build", [ "jshint", "peg", "concat", "copy" ]);

	grunt.registerTask("test", [ "build", "mochaTest" ]);

	// Tell Grunt what to do when we type "grunt" into the terminal.
	grunt.registerTask("default", [ "build", "test", "uglify" ]);
};
