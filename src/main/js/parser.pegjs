{
/* Reinitiase global data */
Blether.classes = {
	"Object": {
		"methods": {}
	}
}
}

start = program

separator = [ \t\v\f\u00A0\uFEFF\n\r\u2028\u2029]+

comments = '"' ('""' {return '"'} / [^"])* '"'

ws = (separator / comments)*

identifier = first:[a-zA-Z] others:[a-zA-Z0-9]* {
	return first + others.join("");
}

keyword = first:identifier last:":" {return first + last;}

selector = first:[a-zA-Z] others:[a-zA-Z0-9\:]* {return first + others.join("");}

className = first:[A-Z] others:[a-zA-Z0-9]* {return first + others.join("");}

string = "'" val:(("''" {return "'";} / [^'])*) "'" {
	return new Blether.String(val.join("")).at(line(), column(), text());
}

character = "$" char:. {
	return new Blether.String(char).at(line(), column(), text());
}

symbol = "#" rest:bareSymbol {return rest;}

bareSymbol = val:(selector / binarySelector / node:string {return node.value;}) {
	return new Blether.Symbol(val).at(line(), column(), text());
}

number = val:(numberExp / hex / float / integer) {
	return new Blether.Number(val).at(line(), column(), text());
}

numberExp = n:((float / integer) "e" integer) {return parseFloat(n.join(""));}

hex = neg:"-"? "16r" num:[0-9a-fA-F]+ {return parseInt(((neg || '') + num.join("")), 16);}

float = neg:"-"? digits:[0-9]+ "." dec:[0-9]+ {return parseFloat(((neg || '') + digits.join("") + "." + dec.join("")), 10);}

integer = neg:"-"? digits:[0-9]+ {return (parseInt((neg || '') + digits.join(""), 10));}

literalArray = "#(" rest:literalArrayRest {return rest;}

bareLiteralArray = "(" rest:literalArrayRest {return rest;}

literalArrayRest = lits:(ws lit:(parseTimeLiteral / bareLiteralArray / bareSymbol) {return lit; })* ws ")" {
	return new Blether.Array(lits).at(line(), column(), text());
}

symbolArray = "#(" syms:( ws sym:(symbol / bareSymbol) { return sym; })* ws ")" {
	return new Blether.Array(syms).at(line(), column(), text());
}

dynamicArray = "{" ws expressions:expressions? ws "."? "}" {
	return new Blether.DynamicArray(expressions).at(line(), column(), text());
}

dynamicDictionary = "#{" ws expressions:associations? ws "}" {
	return new Blether.DynamicDictionary(expressions).at(line(), column(), text());
}

pseudoBooleanVariable = val:( 'true' {return true;} / 'false' {return false;}) {
	return new Blether.Boolean(val).at(line(), column(), text());
}

pseudoNilVariable = val:'nil' {
	return new Blether.UndefinedObject().at(line(), column(), text());
}

pseudoVariable = pseudoBooleanVariable / pseudoNilVariable

parseTimeLiteral = pseudoVariable / number / literalArray / string / symbol / character

runtimeLiteral = dynamicDictionary / dynamicArray / block

literal = runtimeLiteral / parseTimeLiteral

variable = identifier:identifier {
	if (identifier === "this") {
		throw Blether.ParseError({
			"line": line(),
			"column": column(),
			"msg": "Use [self] instead of [this]"
		});
	}

	return new Blether.Variable(identifier).at(line(), column(), text());
}

reference = variable

keywordPair = ws key:keyword ws arg:binarySend {return {key:key, arg:arg};}

binarySelector = bin:[\\+*/=><,@%~|&-]+ { return bin.join(""); }

unarySelector = identifier

keywordPattern = pairs:(ws key:keyword ws arg:identifier {return {key:key, arg:arg};})+ {
	return new Blether.KeywordPattern(pairs).at(line(), column(), text());
}

binaryPattern = ws selector:binarySelector ws arg:identifier {
	return new Blether.BinaryPattern(selector, arg).at(line(), column(), text());
}

unaryPattern = ws selector:unarySelector {
	return new Blether.UnaryPattern(selector).at(line(), column(), text());
}

expressionChain = message:message (ws chain:expressionChain { return chain })? {
	if (chain) {
		return chain.setReceiver(message);
	}
	else {
		return new Blether.send(message).at(line(), column(), text());
	}
}

expression2 = send:(keywordSend / binarySend) message:(ws mess:expressionChain { return mess })? {
	if (message) {
		return message.setReceiver(send);
	}
	else {
		return send;
	}
}

expression = assignment / cascade / expression2

expressionList = ws "." ws expression:expression {return expression;}

expressions = first:expression others:expressionList* { return [first].concat(others); }

assignment = variable:variable ws ':=' ws expression:expression {
	return new Blether.Assignment(variable, expression).at(line(), column(), text());
}

ret = '^' ws expression:expression ws '.'? {
	return new Blether.Return(expression).at(line(), column(), text());
}

temps = "|" vars:(ws variable:identifier {return variable;})* ws "|" {
	
	vars.forEach(function(each) {
		if (each === "self" || each === "this") {
			throw Blether.ParseError({
				"line": line(),
				"column": column(),
				"msg": "Cannot name a variable [" + each + "]"
			});
		}
	});

	return vars;
}

blockParamList = params:((ws ":" ws param:identifier {return param;})+) ws "|" {return params;}

subexpression = '(' ws expression:expression ws ')' {return expression;}

statements = ret:ret "."* {return [ret];}
/ exps:expressions ws "."+ ws ret:ret "."* {
	var expressions = exps;
	expressions.push(ret);
	return expressions;
}
/ expressions:expressions? "."* {
	return expressions || [];
}

statement = expr:(e:expression "." { return e } / jsStatement) {
	return new Blether.Statement(expr).at(line(), column(), text());
}

sequence = jsSequence / stSequence

stSequence = temps:temps? ws statements:statements? ws {
	return new Blether.Sequence(temps, statements).at(line(), column(), text());
}

jsSequence = jsStatement

block = '[' params:blockParamList? ws sequence:sequence? ws ']' {
	return new Blether.Block(params, sequence).at(line(), column(), text());
}

operand = literal / reference / subexpression


unaryMessage = ws selector:unarySelector !":" {
	return new Blether.Send(selector).at(line(), column(), text());
}

unaryTail = message:unaryMessage ws tail:unaryTail? ws {
	if (tail) {
		return tail.setReceiver(message);
	}
	else {
		return message;
	}
}

unarySend = receiver:operand ws tail:unaryTail? {
	if (tail) {
		return tail.setReceiver(receiver);
	}
	else {
		return receiver;
	}
}


binaryMessage = ws selector:binarySelector ws arg:(unarySend / operand) {
	return new Blether.Send(selector, [arg]).at(line(), column(), text());
}


binaryTail = message:binaryMessage tail:binaryTail? {
	if (tail) {
		return tail.setReceiver(message);
	}
	else {
		return message;
	}
}


binarySend = receiver:unarySend tail:binaryTail? {
	if (tail) {
		return tail.setReceiver(receiver);
	}
	else {
		return receiver;
	}
}


keywordMessage = pairs:keywordPair+ {
	var selector = [];
	var args = [];
	for(var i = 0; i < pairs.length; i++) {
		selector.push(pairs[i].key);
		args.push(pairs[i].arg);
	}

	return new Blether.Send(selector.join(""), args).at(line(), column(), text());
}

keywordSend = receiver:binarySend tail:keywordMessage {
	return tail.setReceiver(receiver);
}

message = binaryMessage / unaryMessage / keywordMessage

cascade = ws send:(keywordSend / binarySend) cascade:(ws ";" ws mess:message {return mess;})+ {
	return new Blether.Cascade(send.receiver, [send].concat(cascade)).at(line(), column(), text());
}

jsStatement = "<" val:((">>" {return ">";} / [^>])*) ">" {
	return new Blether.JsStatement(val.join("")).at(line(), column(), text());
}


method = pattern:(keywordPattern / binaryPattern / unaryPattern) ws sequence:sequence? ws {
	return new Blether.Method(pattern, sequence).at(line(), column(), text());
}


associationSend = send:binarySend & { return send.selector === "->" } {
	return [send.receiver, send.args[0]];
}

associationList = ws "." ws expression:associationSend {return expression;}

associations = first:associationSend others:associationList* {
	return first.concat.apply(first, others);
}

classDeclaration = superClass:selector ws "subclass:" ws className:symbol ws ("variables:" / "instanceVariableNames:") ws varNames:symbolArray (ws "classVariableNames:" ws symbolArray ws "poolDictionaries:" ws symbolArray)? ws "." {
	if (!className.value.match(/^[A-Z]/)) {
		throw Blether.ParseError({
			"line": line(),
			"column": column(),
			"msg": "Class name " + className + " must start with a capital"
		});
	}

	if (!Blether.classes[superClass]) {
		throw Blether.ParseError({
			"line": line(),
			"column": column(),
			"msg": "Super-class " + superClass + " not defined"
		});
	}

	if (Blether.classes[className]) {
		throw Blether.ParseError({
			"line": line(),
			"column": column(),
			"msg": "Class " + className + " already defined"
		});
	}

	return Blether.classes[className] = new Blether.ClassDeclaration(
		className,
		superClass,
		varNames).at(line(), column(), text());
}

classAndMethod = "!" className:className ws body:method "!" ws? "." {
	if (className !== "Object" && !Blether.classes[className]) {
		throw Blether.ParseError({
			"line": line(),
			"column": column(),
			"msg": "Unknown class " + className
		});
	}

	if (Blether.classes[className].methods[body.selector]) {
		throw Blether.ParseError({
			"line": line(),
			"column": column(),
			"msg": "Method " + body.selector + " already defined on class " + className
		});
	}

	return Blether.classes[className].methods[body.selector] = 
		new Blether.MethodDeclaration(className, body).at(line(), column(), text());
}

programElement = ws? element:(classAndMethod / classDeclaration / statement) ws?  {
	return element
}

program = first:(ws? decl:temps ws? { return new Blether.VariableDeclaration(decl); } / programElement) others:programElement* {
	return new Blether.Program([first].concat(others)).at(line(), column(), text());
}
