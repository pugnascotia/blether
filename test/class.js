var testUtils = require("../src/test/js/utils.js");

describe("Class creation", function(){

  describe("Extending Object", function() {
    it("should emit basic boilerplate when extending Object with no instance variables", function(){
		testUtils.generateAndCompare("class/extend_object");
    });

    it("should accept and ignore extended attributes", function(){
		testUtils.generateAndCompare("class/extended_syntax");
    });

  });

  describe("Extending a class", function() {
    it("should fail if the superclass is not defined", function(){
		testUtils.generateAndCompare("class/undefined_superclass");
    });

	it("should configure the super-class correctly", function() {
		testUtils.generateAndCompare("class/superclass");
	});

  });

});
