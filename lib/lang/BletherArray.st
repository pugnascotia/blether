BletherNode subclass: #BletherArray variables: #(values).

!BletherArray toString
	^'[' , ((values map: [ :each | '"' , each , '"' ]) join: ', ') , ']'.
!.

!BletherArray visit: aVisitor
	^aVisitor visitArray: self
!.
