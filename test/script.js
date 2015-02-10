var testUtils = require("../src/test/js/utils.js");

describe('Scripting', function(){
  describe('Declaring temps', function(){
    it('should emit JavaScript variables at the top of the output', function(){
		testUtils.generateAndCompare('script.declare_temps');
    })
  })
})
