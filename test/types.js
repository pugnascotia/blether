var testUtils = require("../src/test/js/utils.js");

describe("Basic types", function(){

  describe("Primitives", function() {
    it("Characters should be translated as wrapped strings", function(){
      testUtils.generateAndCompare("types/character");
    });

    it("Strings should be translated as wrapped strings", function(){
      testUtils.generateAndCompare("types/string");
    });

    it("Symbols should be translated as wrapped strings", function(){
      testUtils.generateAndCompare("types/symbol");
    });

    it("Symbols can include spaces", function(){
      testUtils.generateAndCompare("types/symbol_with_spaces");
    });

    it("Integers should be translated as wrapped numbers", function(){
      testUtils.generateAndCompare("types/integer");
    });

    it("Floats should be translated as wrapped numbers", function(){
      testUtils.generateAndCompare("types/float");
    });

    it("Exponentials should be translated as wrapped numbers", function(){
      testUtils.generateAndCompare("types/exp");
    });

    it("Hexadecimal values should be translated as wrapped numbers", function(){
      testUtils.generateAndCompare("types/hex");
    });
  });

  describe("Structures", function() {
    it("Arrays should be translated to native arrays", function() {
      testUtils.generateAndCompare("types/array");
    });

    it("Dynamic arrays should be translated to native arrays", function() {
      testUtils.generateAndCompare("types/dynamic_array");
    });

    it("Dynamic dictionaries should be translated to native objects", function() {
      testUtils.generateAndCompare("types/dict");
    });
  });

});
