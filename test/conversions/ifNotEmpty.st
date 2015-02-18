| foo |
" No block parameter "
foo bar ifNotEmpty: [ 'fred' ].

" With block parameter "
foo bar ifNotEmpty: [ :aBar | aBar barney ].
