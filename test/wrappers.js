var testUtils = require("../lib/test/utils.js");

describe("Wrappers", function(){

  describe("NodeJS", function() {
    it("filename is replaced by __filename", function(){
		testUtils.generateAndCompare("wrappers/nodejs_filename", { execute: false });
    });

    it("dirname is replaced by __dirname", function(){
		testUtils.generateAndCompare("wrappers/nodejs_dirname", { execute: false });
    });

    it("require is replaced by require()", function(){
		testUtils.generateAndCompare("wrappers/nodejs_require");
    });
  });

});
