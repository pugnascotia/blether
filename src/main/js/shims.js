Blether.Shims = {};

Blether.Shims.Transcript = {

	"visitVariable": function() {
		return "console";
	},

	"visitSend": function(translator, node) {
		// Disable the shim temporarily, or cascades don't work
		var recvShim = node.receiver.shim;
		delete node.receiver.shim;
		var receiver = node.receiver.visit(translator);
		node.receiver.shim = recvShim;

		var selector = convertSelector(node.selector);

		var output;

		switch (node.selector) {

			case "show:":
				output = receiver + ".log(";
				output += node.args.map(function(each) { return each.visit(translator) }).join(", ");
				output += ")";
				break;

			case "tab":
				output = receiver + ".log(\"\\t\")";
				break;

			case "nl":
				output = receiver + ".log(\"\\n\")";
				break;

			default:
				output = receiver + "." + selector + "(";
				output += node.args.map(function(each) { return each.visit(translator) }).join(", ");
				output += ")";
		}

		return output;
	}
};

Blether.Shims.Character = {

	"visitVariable": function() {
		return "console";
	},

	"visitSend": function(translator, node) {
		var receiver = node.receiver.visit(translator);
		var selector = convertSelector(node.selector);

		var output;

		switch (node.selector) {

			case "nl":
				output = "\"\\n\"";
				break;

			default:
				output = receiver + "." + selector + "(";
				output += node.args.map(function(each) { return each.visit(translator) }).join(", ");
				output += ")";
		}

		return output;
	}
};
