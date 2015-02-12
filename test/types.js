var testUtils = require("../src/test/js/utils.js");

describe("Basic types", function(){

  describe("Characters", function() {
    it("should be translated as wrapped strings", function(){
		testUtils.generateAndCompare("types/character");
    });
  });

  describe("Strings", function() {
    it("should be translated as wrapped strings", function(){
		testUtils.generateAndCompare("types/string");
    });
  });

  describe("Symbols", function() {
    it("should be translated as wrapped strings", function(){
		testUtils.generateAndCompare("types/symbol");
    });

    it("can include spaces", function(){
		testUtils.generateAndCompare("types/symbol_with_spaces");
    });
  });

  describe("Integers", function() {
    it("should be translated as wrapped numbers", function(){
		testUtils.generateAndCompare("types/integer");
    });
  });

  describe("Floats", function() {
    it("should be translated as wrapped numbers", function(){
		testUtils.generateAndCompare("types/float");
    });
  });

  describe("Exponentials", function() {
    it("should be translated as wrapped numbers", function(){
		testUtils.generateAndCompare("types/exp");
    });
  });

});
