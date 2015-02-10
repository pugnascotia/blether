var testUtils = require("../src/test/js/utils.js");

describe('Class creation', function(){

  describe('Extending Object', function() {
    it('should emit basic boilerplate when extending Object with no instance variables', function(){
		testUtils.generateAndCompare('class.extend_object');
    });

    it('should accept and ignore extended attributes', function(){
		debugger;
		testUtils.generateAndCompare('class.extended_syntax');
    });

  });

});
