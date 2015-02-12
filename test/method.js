var testUtils = require("../src/test/js/utils.js");

describe("Methods", function(){

  describe("Validation", function(){
    it("checks that class is defined", function() {
      testUtils.generateAndCompare("method/undefined_class");
    });

    it("checks that the method is not already is defined", function() {
      testUtils.generateAndCompare("method/already_defined");
    });

    it("checks that keyword methods do not list the same argument name more than once", function() {
      testUtils.generateAndCompare("method/unique_keyword_arg_names");
    });
  });

  describe("Defining", function() {
    it("translate unary method names unchanged", function() {
      testUtils.generateAndCompare("method/unary_name");
    });

    it("translate binary method names", function() {
      testUtils.generateAndCompare("method/binary_name");
    });

    it("translate keyword method names", function() {
      testUtils.generateAndCompare("method/keyword_name");
    });

    it("translate JavaScript methods unmolested", function() {
      testUtils.generateAndCompare("method/javascript_impl");
    });
  });


  describe("Return values", function() {
    it("should default to self if unspecified", function() {
      testUtils.generateAndCompare("method/default_return");
    });

    it("should return the correct value when specified", function() {
      testUtils.generateAndCompare("method/explicit_return");
    });

    it("should generate a try-catch for returns from blocks", function() {
      testUtils.generateAndCompare("method/block_return");
    });

    it("should generate a single try-catch for multiple returns from blocks", function() {
      testUtils.generateAndCompare("method/multi_block_return");
    });
  });

});
