BletherNode subclass: #BletherString variables: #( value ).

!BletherString toString
	^value.
!.

!BletherString visit: aVisitor
	^aVisitor visitString: self
!.
