Object subclass: #Foobar variables: #().

!Foobar singleBinaries
	| fred jim |
	fred \ jim.
	fred + jim.
	fred * jim.
	fred / jim.
	fred = jim.
	fred > jim.
	fred < jim.
	fred @ jim.
	fred % jim.
	fred ~ jim.
	fred | jim.
	fred & jim.
	fred - jim.
!.

!Foobar doubleBinaries
	| fred jim |
	fred \\ jim.
	fred ++ jim.
	fred ** jim.
	fred // jim.
	" Double-equals is translated specially and tested seperately "
	"fred == jim."
	fred >> jim.
	fred << jim.
	fred @@ jim.
	fred %% jim.
	" Double-tilde is translated specially and tested seperately "
	"fred ~~ jim."
	fred || jim.
	fred && jim.
	fred -- jim.
!.

!Foobar tripleBinaries
	| fred jim |
	fred \\\ jim.
	fred +++ jim.
	fred *** jim.
	fred /// jim.
	fred === jim.
	fred >>> jim.
	fred <<< jim.
	fred @@@ jim.
	fred %%% jim.
	fred ~~~ jim.
	fred ||| jim.
	fred &&& jim.
	fred --- jim.
!.
