Object subclass: #Foobar variables: #().

!Foobar test
	| fred |
	self foo: #bar.
	self foo: #bar baz: 1 + 1.
	self foo: #bar baz: 1 + 1 quux: fred run for it!.
