// vim: ts=2 sw=2 sts=2 et:
var test = require("../src/test/js/utils.js");

describe("Sending conversions", function(){

  describe("Native equivalent", function() {
    it("should be used for isNil", function(){
      test.generateAndCompare("conversions/isNil");
    });

    it("should be used for notNil", function(){
      test.generateAndCompare("conversions/notNil");
    });

    it("should be used for isEmpty", function(){
      test.generateAndCompare("conversions/isEmpty");
    });

    it("should be used for notEmpty", function(){
      test.generateAndCompare("conversions/notEmpty");
    });

    it("should be used for ifEmpty:", function(){
      test.generateAndCompare("conversions/ifEmpty");
    });

    it("should be used for ifNotEmpty:", function(){
      test.generateAndCompare("conversions/ifNotEmpty");
    });

    it("should be used for ifNil:", function(){
      test.generateAndCompare("conversions/ifNil");
    });

    it("should be used for ifNotNil:", function(){
      test.generateAndCompare("conversions/ifNotNil");
    });

    it("should be used for ifNil:ifNotNil:", function(){
      test.generateAndCompare("conversions/ifNilIfNotNil");
    });

    it("should be used for ifTrue:", function(){
      test.generateAndCompare("conversions/ifTrue");
    });

    it("should be used for ifFalse:", function(){
      test.generateAndCompare("conversions/ifFalse");
    });

    it("should be used for ifTrue:ifFalse:", function(){
      test.generateAndCompare("conversions/ifTrueIfFalse");
    });

    it("correctly nest ifNotNil: blocks and use unique receiver vars", function(){
      test.generateAndCompare("conversions/nestedIfNotNil");
    });
  });

});
