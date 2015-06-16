Object subclass: #BletherTranslator variables: #( printedClassPrerequisites classPrerequisites context ).

" Yuck. But, sometimes necessary. "
!BletherTranslator truthy: aValue
    <return (typeof value === "boolean" && value) ||
        (typeof value === "string" && value === "true");>

!.

!BletherTranslator classPrerequisites
	<return
"var _extends = function(child, parent) {\n" +
"	for (var key in parent) {\n" +
"		if (_hasProp.call(parent, key)) child[key] = parent[key];\n" +
"	}\n" +
"\n" +
"	function ctor() {\n" +
"		this.constructor = child;\n" +
"	}\n" +
"	ctor.prototype = parent.prototype;\n" +
"	child.prototype = new ctor();\n" +
"	child.__super__ = parent.prototype;\n" +
"	return child;\n" +
"};\n\n" +
"var _hasProp = {}.hasOwnProperty;\n\n";>
!.

!BletherTranslator checkBlock: block maxParamCount: max selector: selector
    ((block at: #params) at: #length) > max ifTrue: [
        BletherParseError raise: 'Too many parameters specified in block arguments to ' , selector
            line: (block at: #line)
            column: (block at: #column) ].
!.

!BletherTranslator visit: node
    ^node visit: self.
!.

!BletherTranslator visitProgram: node
    | output |
    context := BletherContextMgr new.

    output := ''.

    (node at: #elements) forEach: [ :childNode |
        ((childNode at: #'_type') == 'ClassMethodDeclaration' or: [ (childNode at: #'_type') == 'MethodDeclaration' ]) ifTrue: [
            (childNode getClass at: #superClass) ifNotNil: [ ^self ] ].

        output := output , (childNode visit: self).
    ].

    ^output.
!.

!BletherTranslator visitClassDeclaration: node
   | output hasSuper |
   output := ''.

   hasSuper := (node at: #superClass) != 'Object'.
!.

Module export: #translate as: [ :text :argOpts |
    | opts ast runtime translation |
    opts := argOpts ifNil: [ #{ #runtime -> true } ].

    ast := BletherParser parse: text.

    runtime := ''.

    (self truthy: (opts at: #runtime)) ifTrue: [
        | path fs runtimePath |
        path := Module require: 'path'.
        fs := Module require: 'fs'.

        runtimePath := (path dirname: (fs readFileSync: NodeJS filename)) , '/' , 'runtime.js'.
        runtime := (fs readFileSync: runtimePath) toString.
    ].

    translation := BletherTranslator new
        " FIXME: should be initialsed via an #init method "
        at: #printedClassPrerequisites put: false;
        visit: ast.

    runtime , translation.
].
