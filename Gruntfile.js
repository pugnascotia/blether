module.exports = function(grunt) {
	'use strict';

	// Time our tasks in a spangly way
	require('time-grunt')(grunt);

	// Auto-load only required tasks
	// require('jit-grunt')(grunt);

	// Force use of Unix newlines
	grunt.util.linefeed = '\n';

	var javascriptFiles = [
		'src/*/js/**/*.js',
	];

	// 1. All configuration goes here 
	grunt.initConfig({
		pkg: grunt.file.readJSON('package.json'),
		
		peg: {
			grammer: {
				src: "src/main/js/parser.pegjs",
				dest: "target/parser.js",
				options: { exportVar: "var BletherParser" }
			}
		},

		// Configure the grunt-contrib-jshint plugin
		jshint: {
			files: javascriptFiles,
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
					"src/main/js/translator.js",
				],
				dest: "target/blether.js"
			}
		},

		// Configure the grunt-contrib-uglify plugin
		uglify: {
			options: {
				banner: '/*! <%= pkg.name %> <%= grunt.template.today("dd-mm-yyyy") %> */\n'
			},
			build: {
				src: 'target/blether.js',
				dest: 'target/blether.min.js'
			}
		},

		// Configure the grunt-contrib-watch plugin
		watch: {
			scripts: {
				files: javascriptFiles,
				tasks: ['jshint', 'concat' ]
			},
			grammar: {
				files: [ 'src/main/js/parser.pegjs' ],
				tasks: [ 'peg', 'concat' ]
			}
		},
	});

	// Usually we'd tell Grunt what plugins we plan to use as follows, but
	// jit-grunt (see above) does away with this.
	grunt.loadNpmTasks("grunt-contrib-concat");
	grunt.loadNpmTasks("grunt-contrib-jshint");
	grunt.loadNpmTasks("grunt-contrib-uglify");
	grunt.loadNpmTasks('grunt-peg');
	grunt.loadNpmTasks('grunt-contrib-watch');

	// Tell Grunt what to do when we type "grunt" into the terminal.
	grunt.registerTask("default", [
		"jshint",
		"peg",
		"concat",
		"uglify",
	]);

};