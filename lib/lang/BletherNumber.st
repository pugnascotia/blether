BletherNode subclass: #BletherNumber variables: #( value ).

!BletherNumber toString
	^value.
!.

!BletherNumber visit: aVisitor
	^aVisitor visitNumber: self
!.
