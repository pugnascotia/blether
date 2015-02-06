" A comment! "

| sum |

Object subclass: #Foo variables: #( #input #index #interval ).

Foo subclass: #Twit variables: #( #input #index #interval ).

!Foo main
	| fs |
    console log: 'Writing a file called "message.txt".'.

    1 to: 2 do: [ :each | | foo | console log: each ].

    fs := require value: 'fs'.

    fs foo bar writeFileSync: 'message.txt' content: 'This file was written by Amber.' 
       errFn:  nil.!.

!Foo bar: arse with: twit
    | badger |
    Transcript show: 'Badger!'.
    Transcript nl.
	^badger := 1 + 2!.

sum := Foo new bar: 'foo' with: 'arse'.

Transcript show: sum; cr.
