Object subclass: #Foobar variables: #().

!Foobar test
	| fred |
	self at: #foo put: #bar.
	self at: #baz put: fred run.
	self at: #quux put: 1 + 2.
	self at: #schmee put: (fred run: #home).
	self at: #bob put: (fred run: #home with: #Julia).
	self at: fred key put: nil.
!.
