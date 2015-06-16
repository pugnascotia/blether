BletherNode subclass: #BletherDynamicDictionary variables: #( values ).

!BletherDynamicDictionary visit: aVisitor
	^aVisitor visitDynamicDictionary: self.
!.
