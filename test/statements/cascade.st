| foo |
foo := #{}.
foo at: #bar put: [ #{ #operationOne -> []. #operationTwo -> []. #operationThree -> [] } ].
foo bar
	operationOne;
	operationTwo;
	operationThree.
