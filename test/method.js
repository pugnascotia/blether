var testUtils = require("../src/test/js/utils.js");

describe("Method declaration", function(){

  describe("Validation", function(){

    it("checks that class is defined", function() {
		testUtils.generateAndCompare("method/undefined_class");
    });

    it("checks that the method is not already is defined", function() {
		testUtils.generateAndCompare("method/already_defined");
    });

  });

  describe("Defining a method", function() {

    it("translates unary method names unchanged", function() {
		testUtils.generateAndCompare("method/unary_name");
    });

    it("translates binary method names", function() {
		testUtils.generateAndCompare("method/binary_name");
    });

  });
});
