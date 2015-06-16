Object subclass: #BletherNode variables: #( line column source ).

!BletherNode setLine: aLine column: aColumn source: aSource
	line := aLine.
	column := aColumn.
	source := aSource.
!.

!BletherNode invoke
	<return this.visit.apply(this, arguments);>
!.

!BletherNode find: aBlock
	^(aBlock value: self) ifTrue: [ {self} ] ifFalse: [ {} ].
!.

!BletherNode isMethod
	^false.
!.
