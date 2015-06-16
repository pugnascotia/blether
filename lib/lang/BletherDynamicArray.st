BletherArray subclass: #BletherDynamicArray variables: #(values).

!BletherDynamicArray visit: aVisitor
	^aVisitor visitDynamicArray: self
!.
