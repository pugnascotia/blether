| foo |
foo := #{}.
foo at: 'bar' put: [ 1 ].
foo at: 'baz' put: [ 2 ].
{ foo bar. foo baz }.
