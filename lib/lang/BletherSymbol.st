BletherNode subclass: #BletherSymbol variables: #( value ).

!BletherSymbol toString
	^value.
!.

!BletherSymbol visit: aVisitor
	^aVisitor visitSymbol: self.
!.
