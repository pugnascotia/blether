| foo |
foo := #{ #bar -> [nil] }.
foo bar ifNotNil: [ :aBar |
	aBar baz ifNotNil: [ :aBaz |
		Transcript show: aBar, ' -> ', aBaz.
	]
].
