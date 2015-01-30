{
// Import AST types
var Blether = require("./lang.js");
}

start = program

separator      = [ \t\v\f\u00A0\uFEFF\n\r\u2028\u2029]+

comments       = comment:('"' [^"]* '"')+ {
                    return Blether.Comment(comment);
                  }

ws             = (separator / comments)*

identifier     = first:[a-zA-Z] others:[a-zA-Z0-9]* {return first + others.join("");}

keyword        = first:identifier last:":" {return first + last;}

selector      = first:[a-zA-Z] others:[a-zA-Z0-9\:]* {return first + others.join("");}

className      = first:[A-Z] others:[a-zA-Z0-9]* {return first + others.join("");}

string         = "'" val:(("''" {return "'";} / [^'])*) "'" {
                      return new Blether.String(val.join(""));
                 }

character      = "$" char:. {
                      return new Blether.String(char);
                  }

symbol         = "#" rest:bareSymbol {return rest;}

bareSymbol         = val:(selector / binarySelector / node:string {return node._value();}) {
                      return new Blether.Symbol(val);
                  }

number         = val:(numberExp / hex / float / integer) {
                      return new Blether.Number(val);
                 }

numberExp      = n:((float / integer) "e" integer) {return parseFloat(n.join(""));}

hex            = neg:"-"? "16r" num:[0-9a-fA-F]+ {return parseInt(((neg || '') + num.join("")), 16);}

float          = neg:"-"? digits:[0-9]+ "." dec:[0-9]+ {return parseFloat(((neg || '') + digits.join("") + "." + dec.join("")), 10);}

integer        = neg:"-"? digits:[0-9]+ {return (parseInt((neg || '') + digits.join(""), 10));}

literalArray   = "#(" rest:literalArrayRest {return rest;}

bareLiteralArray   = "(" rest:literalArrayRest {return rest;}

literalArrayRest   = lits:(ws lit:(parseTimeLiteral / bareLiteralArray / bareSymbol) {return lit; })* ws ")" {
                      return new Blether.Array(lits);
                 }

dynamicArray   = "{" ws expressions:expressions? ws "."? "}" {
                      return new Blether.DynamicArray(expressions);
                 }

dynamicDictionary = "#{" ws expressions:associations? ws "}" {
                      return new Blether.DynamicDictionary(expressions);
                    }

pseudoBooleanVariable = val:( 'true' {return true;} 'false' {return false;}) {
						   return new Blether.Boolean(val);
					   }

pseudoNilVariable = val:'nil' {
                      return new Blether.UndefinedObject();
                   }

pseudoVariable = pseudoBooleanVariable / pseudoNilVariable

parseTimeLiteral = pseudoVariable / number / literalArray / string / symbol / character

runtimeLiteral = dynamicDictionary / dynamicArray / block

literal        = runtimeLiteral / parseTimeLiteral

variable       = identifier:identifier {
                      return new Blether.Variable(identifier);
                 }

reference      = variable

keywordPair    = ws key:keyword ws arg:binarySend {return {key:key, arg:arg};}

binarySelector = bin:[\\+*/=><,@%~|&-]+ {
					return bin.join("");
				}

unarySelector  = identifier

keywordPattern = pairs:(ws key:keyword ws arg:identifier {return {key:key, arg:arg};})+ {
                     return new Blether.KeywordPattern(pairs);
                 }
binaryPattern  = ws selector:binarySelector ws arg:identifier {
					return new Blether.BinaryPattern(selector, arg);
				}
unaryPattern   = ws selector:unarySelector {
					return new Blether.UnaryPattern(selector);
				}

expression     = assignment / cascade / keywordSend / binarySend

expressionList = ws "." ws expression:expression {return expression;}
expressions    = first:expression others:expressionList* { return [first].concat(others); }

assignment     = variable:variable ws ':=' ws expression:expression {
					 console.log("assignment");
                    return new Blether.Assignment(variable, expression);
                 }

ret            = '^' ws expression:expression ws '.'? {
                      // FIXME why does Amber use an array of ndoes?
                      return new Blether.Return(expression);
                 }
  
temps          = "|" vars:(ws variable:identifier {return variable;})* ws "|" {return vars;}

blockParamList = params:((ws ":" ws param:identifier {return param;})+) ws "|" {return params;}

subexpression  = '(' ws expression:expression ws ')' {return expression;}

statements     = ret:ret "."* {return [ret];}
                 / exps:expressions ws "."+ ws ret:ret "."* {
                       var expressions = exps;
                       expressions.push(ret);
                       return expressions;
                   }
                 / expressions:expressions? "."* {
                       return expressions || [];
                   }

sequence       = jsSequence / stSequence

stSequence     = temps:temps? ws statements:statements? ws {
                      return new Blether.Sequence(temps, statements);
                 }

jsSequence     = jsStatement

block          = '[' params:blockParamList? ws sequence:sequence? ws ']' {
                    return new Blether.Block(params, sequence);
                 }

operand        = literal / reference / subexpression


unaryMessage   = ws selector:unarySelector !":" {
                      return new Blether.Send(selector);
                 }

unaryTail      = message:unaryMessage ws tail:unaryTail? ws {
					 return tail ? tail.setReceiver(message) : message;
                 }

unarySend      = receiver:operand ws tail:unaryTail? {
                     if (tail) {
                         return tail.setReceiver(receiver);
                     }
                     else {
                         return receiver;
                     }
                 }

binaryMessage  = ws selector:binarySelector ws arg:(unarySend / operand) {
					 console.log("binaryMessage [" + selector + "] [" + arg + "]");
                      return Blether.Send(selector, [arg]);
                 }

binaryTail     = message:binaryMessage tail:binaryTail? {
                     if (tail) {
                         return tail.setReceiver(message);
                      }
                     else {
                         return message;
                     }
                 }

binarySend     = receiver:unarySend tail:binaryTail? {
                     if (tail) {
						console.warn("binarySend [" + receiver + "] [" + tail + "]");
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
                      return new Blether.Send(selector.join("_").replace(/:/g, ''), args);
                 }

keywordSend    = receiver:binarySend tail:keywordMessage {
                     //return tail._valueForReceiver_(receiver);
					 console.log("keywordSend [" + receiver + "] [" + tail + "]");
                     return tail.setReceiver(receiver);
                 }

message        = binaryMessage / unaryMessage / keywordMessage

cascade        = ws send:(keywordSend / binarySend) messages:(ws ";" ws mess:message {return mess;})+ {
                     var cascade = [];
                     cascade.push(send);
                     for(var i = 0; i < messages.length; i++) {
                         cascade.push(messages[i]);
                     }
                    return new Blether.Cascade(send.receiver, cascade);
                 }

jsStatement    = "<" val:((">>" {return ">";} / [^>])*) ">" {
                     //return $globals.JSStatementNode._new()
                     //       ._position_((line()).__at(column()))
                     //       ._source_(val.join(""))
                      return new Blether.JsStatement(val.join(""));
                 }


method         = pattern:(keywordPattern / binaryPattern / unaryPattern) ws sequence:sequence? ws {
                      return new Blether.Method(pattern, [sequence]);
                 }


associationSend     = send:binarySend & { return send.selector === "->" } {
                          return [send.receiver, send.args[0]];
                      }

associationList = ws "." ws expression:associationSend {return expression;}

associations    = first:associationSend others:associationList* {
                      return first.concat.apply(first, others);
                      
                  }

classDeclaration = "Object" ws "subclass:" ws className:symbol ws "variables:" ws varNames:literalArray ws "." {
                      return new Blether.ClassDeclaration(className, varNames);
                  }

classAndMethod = "!" className:className ws body:method "!" ws? "." {
                    return new Blether.MethodDeclaration(className, body);
                  }

programElement = ws? element:(comments / classAndMethod / classDeclaration) ws?  { return element }

program = first:programElement others:programElement* {
              return new Blether.Program([first].concat(others));
          }
