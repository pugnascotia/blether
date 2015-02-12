Object subclass: #Foobar variables: #().

!Foobar search: aList
	aList do: [ :each |
		each == 3 ifTrue: [ ^true ] ].

	^false
!.
