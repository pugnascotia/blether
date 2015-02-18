| foo |
" No block parameter "
foo bar ifNotNil: [ 'hello' ].

" With block parameter "
foo bar ifNotNil: [ :fred | fred hello ].
