var testUtils = require("../src/test/js/utils.js");

describe("Sequences and Statements", function(){

  describe("Temporary values", function() {
    it("should be declared at the start of a sequence", function(){
		testUtils.generateAndCompare("statements/temps");
    });

    it("cannot include self", function(){
		testUtils.expectParseError( "statements/cannot_include_self", "Cannot name a variable [self]");
    });

    it("cannot include this", function(){
		testUtils.expectParseError( "statements/cannot_include_this", "Cannot name a variable [this]");
    });

    it("must be declared", function(){
		testUtils.expectParseError( "statements/must_be_declared", "Unknown variable name [foo]");
    });

  });

});
