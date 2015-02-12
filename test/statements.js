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

  describe("Unary sends", function() {
    it("are translated to an argument-less method call", function(){
      testUtils.generateAndCompare("statements/unary_sends");
    });
  });

  describe("Binary sends", function() {
    it("are translated to an equivalent-method call with a single argument", function(){
      testUtils.generateAndCompare("statements/binary_sends");
    });

    it("for strict equality or non-equality are translated to native operations", function(){
      testUtils.generateAndCompare("statements/binary_sends_to_native");
    });
  });

  describe("Keyword sends", function() {
    it("should translate #at:put: to a property set", function(){
      testUtils.generateAndCompare("statements/keywords_at_put");
    });

    it("are translated to a method call with arguments", function(){
      testUtils.generateAndCompare("statements/keywords_sends");
    });
  });


});
