var assert = require("assert");
var testUtils = require("../src/test/js/utils.js");

describe('Class creation', function(){
  describe('Extending Object', function(){
    it('should emit basic boilerplate when extending Object with no instance variables', function(){
		testUtils.generateAndCompare(__filename);
    })
  })
})
