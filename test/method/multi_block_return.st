Object subclass: #Foobar variables: #().

!Foobar compare: aList with: anotherList
	aList do: [ :each |
		each == 3 ifTrue: [ ^true ].
		each == 4 ifTrue: [ ^true ].
		each == 5 ifTrue: [
			anotherList do: [ :other |
				other == #fred ifTrue: [ ^false ] ] ] ].

	^false
!.
