var testUtils = require("../lib/test/utils.js");

describe("Scripting", function(){
  describe("Declaring temps", function(){
    it("should emit JavaScript variables at the top of the output", function() {
		testUtils.generateAndCompare("script/declare_temps");
    });

	it("should fail if the temps are not first", function() {
		testUtils.expectSyntaxError("script/temps_not_first");
	});

  });
});
