| foo |
foo := #{ #bar -> [] }.

" No argument to ifNotNil: block "
foo bar ifNil: [ 'fred' ] ifNotNil: [ 'barney' ].

" With argument to ifNotNil: block "
foo bar ifNil: [ 1 + 2 ] ifNotNil: [ :aBar | aBar barney ].
