" A comment! "

Object subclass: #Foo variables: #( #input #index #interval #console ).

!Foo main
| fs |
    console log: 'Writing a file called "message.txt".'.

    1 to: 2 do: [ :each | | foo | console log: each ].

    fs := require value: 'fs'.

    fs writeFileSync: 'message.txt' content: 'This file was written by Amber.' 
       errFn:  nil.!.

!Foo bar: arse with: twit
    | badger |
    console log: 'Badger!'!.
