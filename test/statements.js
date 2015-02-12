var testUtils = require("../src/test/js/utils.js");

describe("Sequences and Statements", function(){

  describe("Temporary values", function() {
    it("should be declared at the start of a sequence", function(){
		testUtils.generateAndCompare("statements/temps");
    });

    it("cannot be declared anywhere other than the start of a sequence", function(){
		testUtils.expectSyntaxError("statements/temps_after_start");
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

  describe("Instance variables", function() {
    it("are resolved when no temporary of the same name exists", function(){
		testUtils.generateAndCompare("statements/inst_vars_resolved");
    });

    it("can be hidden by a temporary variable ", function(){
		testUtils.generateAndCompare("statements/inst_vars_hidden");
    });

    it("must be declared", function(){
		testUtils.expectParseError( "statements/inst_vars_must_be_declared", "Unknown variable name [foo]");
    });

  });

});
