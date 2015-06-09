| foo |
foo := 'bobbins'.

" No block parameter "
foo ifNotEmpty: [ 'fred' ].

" With block parameter "
foo ifNotEmpty: [ :aBar | aBar , ' baz' ].
