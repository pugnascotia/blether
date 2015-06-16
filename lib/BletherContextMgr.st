Object subclass: #BletherContextMgr variables: #( contextStack receiverDepth ).

!BletherContextMgr pushClass: className withVars: instVars
    | newContext |
    newContext := BletherContext new.
    newContext className: className.
    newContext instanceVars: instVars.
    self push: newContext.
!.

!BletherContextMgr pushMethod: temps
    | newContext |
    newContext := BletherContext new.
    newContext returnContext: 'method'.
    newContext temps: temps.
    self push: newContext.
!.

!BletherContextMgr pushTemps: temps
    | newContext |
    newContext := BletherContext new.
    newContext temps: temps.
    self push: newContext.
!.

!BletherContextMgr pushBlock: temps
    | newContext |
    newContext := BletherContext new.
    newContext returnContext: 'block'.
    newContext temps: temps.
    self push: newContext.
!.

!BletherContextMgr push: context
    contextStack ifNil: [
        contextStack := { BletherContext new } ].

    contextStack push: context.
!.

!BletherContextMgr pop
    contextStack length == 1 ifTrue: [
        self error: 'Cannot pop the context beyond the initial state!' ].

    contextStack pop.
!.

!BletherContextMgr pushReceiver
    receiverDepth := (receiverDepth ifNil: [ 0 ]) + 1.
!.

!BletherContextMgr popReceiver
    (receiverDepth ifNil: [0]) == 0 ifTrue: [
        self error: 'receiverDepth is already zero and cannot be popped further' ].

    receiverDepth := receiverDepth - 1.
!.

!BletherContextMgr getTemps
    ^contextStack reduceRight: [ :prev :curr |
            prev concat: (curr temps) ]
        startingWith: #().
!.

!BletherContextMgr getInstanceVars
    ^contextStack reduceRight: [ :prev :curr |
            prev concat: (curr instanceVars) ]
        startingWith: #().
!.

!BletherContextMgr currentClass
    contextStack do: [ :each |
        each className ifNotNil: [ :cn | ^cn ] ].

    ^nil.
!.

!BletherContextMgr returnContext
    contextStack do: [ :each |
        each returnContext ifNotNil: [ :rc | ^rc ] ].

    ^nil.
!.
