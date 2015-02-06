/* exported convertSelector */
function convertSelector(selector) {

	var mapping = {
		"\\": "backslash",
		"+": "plus",
		"*": "multiply",
		"/": "divide",
		"=": "equals",
		">": "greater_than",
		"<": "less_than",
		"@": "at",
		"%": "modulo",
		"~": "tilde",
		"|": "or",
		"&": "and",
		"-": "minus"
	};

	var javascriptEquivalents = {
		"do:": "forEach",
		"doWithIndex:": "forEach",
		"select:": "filter",
		"," : "concat"
	};

	if (javascriptEquivalents.hasOwnProperty(selector)) {
		return javascriptEquivalents[selector];
	}

	if (selector.match(/[\\+*/=><,@%~|&-]/)) {
		return selector.split("").map(function(e) { return mapping[e]; }).join("_") + "$";
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

	return selector;
}
