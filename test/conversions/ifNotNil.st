| foo |
" No block parameter "
foo ifNotNil: [ 'hello' ].

" With block parameter "
foo ifNotNil: [ :fred | fred hello ].
