/* exported Smalltalk, Transcript, STReturnValue */
/*jshint -W121 */

var Smalltalk = {
	"require": function(lib) {
		return require(lib);
	}
};

String.prototype.__equals$   = function(arg) { return this === arg };
String.prototype.__concat$ = function() {
	return Array.reduce(arguments, function(prev, curr) { prev + curr }, this);
};

String.prototype.__value$ = function() { return this };

Number.prototype.__equals$   = function(arg) { return this === arg };
Number.prototype.__plus$     = function(arg) { return this + arg };
Number.prototype.__multiply$ = function(arg) { return this * arg };
Number.prototype.__divide$   = function(arg) { return this / arg };
Number.prototype.__minus$    = function(arg) { return this - arg };
Number.prototype.__modulo$   = function(arg) { return this % arg };
/* jshint bitwise:false */
Number.prototype.__or$       = function(arg) { return this | arg };
Number.prototype.__and$      = function(arg) { return this & arg };
/* jshint bitwise:true */
Number.prototype.__value$ = function() { return this };

// Logical operators
Number.prototype.__greater_than$ = function(arg) { return this > arg };
Number.prototype.__less_than$    = function(arg) { return this < arg };

Function.prototype.__value$ = function() { return this.apply(this, arguments) };

var Transcript = (function() {
	var buffer = "";

	var T = {};

	T.show = function(s) { buffer += s; return this; };
	T.nl = T.cr = function() { console.log(buffer); buffer = ""; return this; };
	T.tab = function() { buffer += "\t"; return this; };

	return T;
})();

var STReturnValue = function(_value) {
	var value = _value;
	Object.defineProperty(this, "value", {
		value: value,
		writable: false,
		enumerable: true,
		configurable: true
	});
};
