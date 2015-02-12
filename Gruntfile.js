module.exports = function(grunt) {
	"use strict";

	// Time our tasks in a spangly way
	require("time-grunt")(grunt);

	// Auto-load only required tasks
	// require("jit-grunt")(grunt);

	// Force use of Unix newlines
	grunt.util.linefeed = "\n";

	// 1. All configuration goes here 
	grunt.initConfig({
		pkg: grunt.file.readJSON("package.json"),
		
		peg: {
			grammer: {
				src: "src/main/js/parser.pegjs",
				dest: "target/parser.js",
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
					"target/parser.js",
					"src/main/js/selector.js",
					"src/main/js/tree_modifier.js",
					"src/main/js/return_operator_visitor.js",
					"src/main/js/translator.js",
				],
				dest: "target/blether.js"
			}
		},

		copy: {
			runtime: {
				src: "src/main/js/runtime.js",
				dest: "target/runtime.js"
			}
		},

		// Configure the grunt-contrib-uglify plugin
		uglify: {
			options: {
				banner: "/*! <%= pkg.name %> <%= grunt.template.today(\"dd-mm-yyyy\") %> */\n"
			},
			blether: {
				src: "target/blether.js",
				dest: "target/blether.min.js"
			},
			runtime: {
				src: "target/runtime.js",
				dest: "target/runtime.min.js"
			}
		},

		// Configure the grunt-contrib-watch plugin
		watch: {
			scripts: {
				files: "test/**/*.js",
				tasks: ["jshint", "concat", "copy", "mochaTest" ]
			},
			tests: {
				files: [ "src/test/**/*.js", "test/**/*.js", "test/**/*.st", "test/**/*.result" ],
				tasks: [ "mochaTest" ]
			},
			grammar: {
				files: [ "src/main/js/parser.pegjs" ],
				tasks: [ "peg", "concat", "copy", "mochaTest" ]
			},
			build: {
				files: [ "Gruntfile.js", ".jshintrc" ],
				tasks: [ "default" ]
			}
		},

		mochaTest: {
			test: {
				options: {
					"reporter": "spec",
					"captureFile": "results.txt",
					"quiet": false,
					"clearRequireCache": false
				},
				src: [ "test/**/*.js" ]
			}
		}
	});

	// Usually we"d tell Grunt what plugins we plan to use as follows, but
	// jit-grunt (see above) does away with this.
	grunt.loadNpmTasks("grunt-contrib-concat");
	grunt.loadNpmTasks("grunt-contrib-jshint");
	grunt.loadNpmTasks("grunt-contrib-uglify");
	grunt.loadNpmTasks("grunt-peg");
	grunt.loadNpmTasks("grunt-contrib-watch");
	grunt.loadNpmTasks("grunt-contrib-copy");
	grunt.loadNpmTasks("grunt-mocha-test");

	grunt.registerTask("build", [ "jshint", "peg", "concat", "copy" ]);

	grunt.registerTask("test", [ "mochaTest" ]);

	// Tell Grunt what to do when we type "grunt" into the terminal.
	grunt.registerTask("default", [ "build", "test", "uglify" ]);
};
