/* exported convertSelector */
function convertSelector(selector) {

	function convertBinaryChars(str) {
		var mapping = {
			"\\": "backslash",
			"+": "plus",
			"*": "multiply",
			"/": "divide",
			"=": "equals",
			">": "greater_than",
			"<": "less_than",
			",": "join",
			"@": "at",
			"%": "modulo",
			"~": "tilde",
			"|": "or",
			"&": "and",
			"-": "minus"
		};

		return str.split("").map(function(e) { return mapping[e]; }).join("_") + "$";
	}

	if (selector.match(/[\\+*/=><,@%~|&-]/)) {
		return convertBinaryChars(selector);
	}

	if (selector.match(/:/)) {
		var selector_parts = selector.split(":").filter(function(each) { return each.length > 0; });

		if (selector_parts.length > 1) {
			selector = selector_parts.join("_") + "$";
		}
		else {
			selector = selector_parts[0];
		}
	}

	var javascriptEquivalents = {
		"do": "forEach",
		"doWithIndex": "forEach",
		"select": "filter"
	};

	if (javascriptEquivalents.hasOwnProperty(selector)) {
		return javascriptEquivalents[selector];
	}

	return selector;
}
