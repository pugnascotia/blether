| foo bar |
bar := #{ #baz -> [1] }.

" Reduced to simple value "
foo or: [ bar ].

" Block is executed "
foo or: [ | tmp | tmp := bar baz. tmp > 2 ].
