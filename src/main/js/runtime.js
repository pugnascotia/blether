/* exported STString, STNumber, Transcript, STReturnValue */
var STString = (function() {
	function STString(s) {
		this.value = s || "";
		Object.defineProperty(this, "length", {
			get: function () {
				return this.value.length;
			}
		}); 
	}

	STString.prototype = Object.create(String.prototype);

	STString.prototype.toString = STString.prototype.valueOf = function() { return this.value };

	STString.prototype.equals$ = function(other) {
		return this.value === other;
	};

	STString.prototype.concat = function() {
		var ret = this.value;

		for (var i = 0; i < arguments.length; i++) {
			ret += arguments[i].toString();
		}

		return new STString(ret);
	};

	STString.prototype.asJQuery = function() {
		return jQuery(this.valueOf());
	};

	return STString;
})();

var STNumber = (function() {
	function STNumber(n) {
		this.value = n;
	}

	STNumber.prototype = Object.create(Number.prototype);

	STNumber.prototype.valueOf = function() { return this.value };

	STNumber.prototype.equals$ = function(other) {
		return this.value === other;
	};

	STNumber.prototype.toString = function() { return this.value.toString() };

	// Arithmetic operators
	STNumber.prototype.plus$ = function(arg) { return new STNumber(this.value + arg) };
	STNumber.prototype.multiply$ = function(arg) { return new STNumber(this.value * arg) };
	STNumber.prototype.divide$ = function(arg) { return new STNumber(this.value / arg) };
	STNumber.prototype.minus$ = function(arg) { return new STNumber(this.value - arg) };
	STNumber.prototype.modulo$ = function(arg) { return new STNumber(this.value % arg) };

	// Binary operators
	/* jshint bitwise:false */
	STNumber.prototype.or$ = function(arg) { return new STNumber(this.value | arg) };
	STNumber.prototype.and$ = function(arg) { return new STNumber(this.value & arg) };
	/* jshint bitwise:true */

	// Logical operators
	STNumber.prototype.greater_than$ = function(arg) { return this.value > arg };
	STNumber.prototype.less_than$ = function(arg) { return this.value < arg };

	return STNumber;
})();

var Transcript = (function() {

	var buffer = "";

	var T = {};

	T.show = function(s) { buffer += s; return this; },
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
