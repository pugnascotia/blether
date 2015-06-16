Object subclass: #BletherContext variables: #( currentClass instanceVars temps returnContext ).

!BletherContext currentClass
    ^currentClass.
!.

!BletherContext currentClass: aClass
    currentClass := aClass.
!.

!BletherContext instanceVars
    ^instanceVars.
!.

!BletherContext instanceVars: aClass
    instanceVars := aClass.
!.

!BletherContext temps
    ^temps.
!.

!BletherContext temps: aClass
    temps := aClass.
!.

!BletherContext returnContext
    ^returnContext.
!.

!BletherContext returnContext: aClass
    returnContext := aClass.
!.
