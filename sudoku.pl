/* Sudoku Solver 2013 */
/* Richard Lum
====================================================

ALL TESTS WORK.   test3 running at well under  3 seconds on a small virtual machine.
On  linux machines all tests combined  run in about a second.

% 12,808,842 inferences, 1.038 CPU in 1.041 seconds (100% CPU, 12339055 Lips)


test. has been modified to include test1,test2,test3 and put the fail test case 0c at
the end.  entire test takes under 5 seconds on a low end virtual machine.

initial results:

test0	recognizes it is a solution
test0a	finds solution
test0b	solution in % 27,199 inferences, 0.000 CPU in 0.004 seconds (0% CPU, Infinite Lips)
test0c	% 3,578 inferences, 0.000 CPU in 0.001 seconds (0% CPU, Infinite Lips)
test0d	% 178,186 inferences, 0.020 CPU in 0.049 seconds (41% CPU, 8909300 Lips)

above results were before speed up attempts, this version now provides the following

test0d	% 18,457 inferences, 0.000 CPU in 0.003 seconds (0% CPU, Infinite Lips)
test1 	% 1,413,956 inferences, 0.000 CPU in 0.436 seconds (0% CPU, Infinite Lips)
test2	% 3,838,320 inferences, 0.000 CPU in 1.144 seconds (0% CPU, Infinite Lips)
test3	% 7,502,382 inferences, 0.000 CPU in 2.227 seconds (0% CPU, Infinite Lips)

test1 solution
sudoku
[9, 6, 3, 1, 7, 4, 2, 5, 8]
[1, 7, 8, 3, 2, 5, 6, 4, 9]
[2, 5, 4, 6, 8, 9, 7, 3, 1]
[8, 2, 1, 4, 3, 7, 5, 9, 6]
[4, 9, 6, 8, 5, 2, 3, 1, 7]
[7, 3, 5, 9, 6, 1, 8, 2, 4]
[5, 8, 9, 7, 1, 3, 4, 6, 2]
[3, 1, 7, 2, 4, 6, 9, 8, 5]
[6, 4, 2, 5, 9, 8, 1, 7, 3]
% 1,413,956 inferences, 0.150 CPU in 0.451 seconds (33% CPU, 9426373 Lips)
true.


test2 solution
?- time(test2).
sudoku
[2, 5, 4, 1, 9, 3, 8, 7, 6]
[6, 8, 9, 5, 7, 2, 1, 4, 3]
[3, 7, 1, 6, 4, 8, 2, 9, 5]
[4, 6, 2, 8, 5, 9, 3, 1, 7]
[9, 3, 7, 2, 1, 4, 6, 5, 8]
[8, 1, 5, 7, 3, 6, 9, 2, 4]
[1, 4, 6, 9, 8, 5, 7, 3, 2]
[5, 9, 8, 3, 2, 7, 4, 6, 1]
[7, 2, 3, 4, 6, 1, 5, 8, 9]
% 3,838,320 inferences, 0.400 CPU in 1.174 seconds (34% CPU, 9595800 Lips)
true.



test3 solution
sudoku
[1, 4, 3, 9, 8, 6, 2, 5, 7]
[6, 7, 9, 4, 2, 5, 3, 8, 1]
[2, 8, 5, 7, 3, 1, 6, 9, 4]
[9, 6, 2, 3, 5, 4, 1, 7, 8]
[3, 5, 7, 6, 1, 8, 9, 4, 2]
[4, 1, 8, 2, 7, 9, 5, 6, 3]
[8, 2, 1, 5, 6, 7, 4, 3, 9]
[7, 9, 6, 1, 4, 3, 8, 2, 5]
[5, 3, 4, 8, 9, 2, 7, 1, 6]
% 7,502,382 inferences, 0.750 CPU in 2.292 seconds (33% CPU, 10003176 Lips)
true.

It turns out that the way the sudoku solution is written, 
it is multifunction, so test will loop over all solutions in 
each of the test cases because of the failing test case in test0c
that causes back tracking to find other possible solutions in earlier
test cases. Effectively, this means
that test0b will be in an apparent infinite loop.  I added 
a cut at the end of sudoku to prevent this so only a single
solution is shown for each invocation of sudoku.


Core
Search Strategy: find all locations where blanks exist, create a matrix in each of those
	positions. The matrix will have 3 lists, rowlist,collist,cubelist. Each
	list is to be populated by numbers that they need to complete the 
	respective row, col, cube.   These numbers are needed for solution but
	they also represent what numbers are allowed. For each variable, 
	intersect the three lists and use that intersection as input to the 
	solution generator. If we find any zero intersections, there is no solution.
	These lists of choices are inserted into a mirror copy of the input matrix
	and hold the lists in the same position as the variables in L
	
Attempted speedups by 
- changing member to = for single choice lists 
- adding cuts after assignment of single choice list
- adding cut after generation of constraint matrix to ensure we arent 
	backtracking to look for alternate constraints once we have built the
	constraint matrix
- move the check for diff elems in a row to within the same method where
	we constrain (unify) a anonymous variable to member of list in constraint
	matrix. FAIL UNACCEPTABLE LINES AS EARLY AS POSSIBLE.  debugging shows that
	test1 was alwas failing row check, now reaches col checks indicating
	significant speed up from fast elimination of nonconforming rows. 
- speedup improvements to this point from test0d, factor of 10
 17,111 inferences, 0.000 CPU in 0.009 seconds (0% CPU, Infinite Lips)
 178,186 inferences, 0.020 CPU in 0.049 seconds (41% CPU, 8909300 Lips)
- still not enough to get reasonalbe test 1 result.
- removed cuts and continued following up on idea of eliminating false choices
	earlier. Idea is to replicate what was done with early detection
	elimination of bad row choices to something for columns.
	Specifically, MODIFIED LATER ROW SELECTION OF CHOICE BASED ON 
	EARLIER ROW BINDINGS.  Carried the constraint matrix into the the 
	binding of variables, once a variable was bound, updated constraint
	matrix for all future lines such that any list of choices that had the 
	current line bindings removed from future line choices.  This
	in addition to the early faulty row choice detection provided 
	significant speed up so that all individual tests complete in under 3 seconds
- could probably do something similar for cube violations and probably
	improve line performance by modifying selections on the same
	row before binding stage instead of building the row fully 
	before rejecting it. Similar to how row selections eliminate
	future choices for other selections in other rows in same column
	we can modify future choices in same row/same cube.  This would have
	significant impacts on matrices that contained a lot of choices 
	per row/cube.  But law of diminishing returns suggests that I should go do 
	my other homework now.....

*/




/* This runs all  tests and shows
completed sudoku tables, and finally the 
word false (as test0c will fail.) */
test :-
	test0, nl,
	test0a, nl,
	test0b, nl,
	test1, nl,
	test2, nl,
	test3, nl,
	test0c, nl.

/* This is a completly solved solution. */
test0 :-
	L = [
             [9,6,3,1,7,4,2,5,8],
             [1,7,8,3,2,5,6,4,9],
             [2,5,4,6,8,9,7,3,1],
             [8,2,1,4,3,7,5,9,6],
             [4,9,6,8,5,2,3,1,7],
             [7,3,5,9,6,1,8,2,4],
             [5,8,9,7,1,3,4,6,2],
             [3,1,7,2,4,6,9,8,5],
             [6,4,2,5,9,8,1,7,3]],
        sudoku(L),
        writeln('-----test0-------------'),
        printsudoku(L).

/* This has a solution (the one in test0) which 
should be found very quickly. */
test0a :-
	L = [
             [9,_,3,1,7,4,2,5,8],
             [_,7,_,3,2,5,6,4,9],
             [2,5,4,6,8,9,7,3,1],
             [8,2,1,4,3,7,5,_,6],
	     [4,9,6,8,5,2,3,1,7],
             [7,3,_,9,6,1,8,2,4],
             [5,8,9,7,1,3,4,6,2],
             [3,1,7,2,4,6,9,8,5],
             [6,4,2,5,9,8,1,7,3]],
        sudoku(L),
         writeln('-----test0a-------------'),
        printsudoku(L).

/* This has a solution (the one in test0) and 
may take a few seconds to find. */
test0b :-
	L = [
             [9,_,3,1,7,4,2,5,_],
             [_,7,_,3,2,5,6,4,9],
             [2,5,4,6,_,9,_,3,1],
             [_,2,1,4,3,_,5,_,6],
             [4,9,_,8,_,2,3,1,_],
             [_,3,_,9,6,_,8,2,_],
             [5,8,9,7,1,3,4,6,2],
             [_,1,7,2,_,6,_,8,5],
             [6,4,2,5,9,8,1,7,3]],
        sudoku(L),
		 writeln('-----test0b-------------'),
        printsudoku(L).

/* This one obviously has no solution (column 2 has 
two nines in it.) and it may take a few seconds 
to deduce this. */
test0c :-
	L = [
             [_,9,3,1,7,4,2,5,8],
             [_,7,_,3,2,5,6,4,9],
             [2,5,4,6,8,9,7,3,1],
             [8,2,1,4,3,7,5,_,6],
	     [4,9,6,8,5,2,3,1,7],
             [7,3,_,9,6,1,8,2,4],
             [5,8,9,7,1,3,4,6,2],
             [3,1,7,2,4,6,9,8,5],
             [6,4,2,5,9,8,1,7,3]],
        
		 writeln('-----test0c-------------'),
        sudoku(L),
		printsudoku(L).

/* Here is an extra test for you to try. It would be
nice if your program can solve this puzzle, but it's
not a requirement. */

test0d :-
	L = [
             [9,_,3,1,_,4,2,5,_],
             [_,7,_,3,2,5,6,4,9],
             [2,5,4,6,_,9,_,3,1],
             [_,2,1,4,3,_,5,_,6],
             [4,9,_,8,_,2,3,1,_],
             [_,3,_,9,6,_,8,2,_],
             [5,8,9,7,1,3,4,6,2],
             [_,1,7,2,_,6,_,8,5],
             [6,4,2,5,_,8,1,7,3]],
        sudoku(L),
		 writeln('-----test0d-------------'),
        printsudoku(L).


/* The next 3 tests are supposed to be progressively 
harder to solve. Our first attempt at a solver did not 
find a solution in a reasonable length of time for 
any of these, so if you manage to write a solver 
that does them in a reasonable length of time, 
expect to recieve top or possibly bonus marks. (BUT 
YOU MUST TELL US THIS IN YOUR SUBMISSION OR WE WON'T 
RUN THESE TESTS.) */
test1 :-
	L = [
             [_,6,_,1,_,4,_,5,_],
             [_,_,8,3,_,5,6,_,_],
             [2,_,_,_,_,_,_,_,1],
             [8,_,_,4,_,7,_,_,6],
	     [_,_,6,_,_,_,3,_,_],
             [7,_,_,9,_,1,_,_,4],
             [5,_,_,_,_,_,_,_,2],
             [_,_,7,2,_,6,9,_,_],
             [_,4,_,5,_,8,_,7,_]],
        sudoku(L),
		 writeln('-----test1-------------'),
        printsudoku(L).

test2 :-
	L = [
             [_,_,4,_,_,3,_,7,_],
             [_,8,_,_,7,_,_,_,_],
             [_,7,_,_,_,8,2,_,5],
             [4,_,_,_,_,_,3,1,_],
	     [9,_,_,_,_,_,_,_,8],
             [_,1,5,_,_,_,_,_,4],
             [1,_,6,9,_,_,_,3,_],
             [_,_,_,_,2,_,_,6,_],
             [_,2,_,4,_,_,5,_,_]],
        sudoku(L),
		 writeln('-----test2-------------'),
        printsudoku(L).

test3 :-
	L = [
             [_,4,3,_,8,_,2,5,_],
	     [6,_,_,_,_,_,_,_,_],
             [_,_,_,_,_,1,_,9,4],
             [9,_,_,_,_,4,_,7,_],
             [_,_,_,6,_,8,_,_,_],
             [_,1,_,2,_,_,_,_,3],
             [8,2,_,5,_,_,_,_,_],
             [_,_,_,_,_,_,_,_,5],
             [_,3,4,_,9,_,7,1,_]],
        sudoku(L),
		 writeln('-----test3-------------'),
        printsudoku(L).


% print suduko table
printsudoku([]).
printsudoku([H|T]) :- 
	write(H),nl,
	printsudoku(T).


% Expects a list of lists 9 by 9 grid.
sudoku(L):-
	writeln('sudoku'),
	conforms(L),
	makeConstraints(L,CMatrix), % writeln('constrained'), printsudoku(L), % create constraints while analyzing matrix
	intersectContraintsMatrix(CMatrix,IMatrix), % ! , % speed up attempt : prevent any backtrackin on generation of constraing matrix, once done, its done and the only answer  .. writeln('intersection Matrix'), printsudoku(IMatrix),  % intersect row, col, cube constraints
	solve(IMatrix,L),!. 	% cut added here to prevent looping results for test

%%
%%


solve(C,L):- 
		identical(C,L).  % will be the case if presented with solved puzzle
solve(ConstraintsMatrix,L):- 
	constrain(L,ConstraintsMatrix), % must constrain before doing anything that causes backtracking
	 % write('solve'),printsudoku(ConstraintsMatrix),writeln('L'),printsudoku(L),
	valid(L). 

identical([],[]).
identical([C|Cs],[L|Ls]):-
	identicalLines(C,L),
	identical(Cs,Ls).

identicalLines([],[]).
identicalLines([X|Xs],[X|Ys]):- % writeln('identicalLines'),
	integer(X),  % prevent binding to anonymous varibles
	identicalLines(Xs,Ys).
	
	
% check permutation row,col,cube ea have no duplicates
% and contain all digits.
valid(L):-
	digits(Digits),
	%uniqueElemInLines(L), 		% Rows no duplicates
	colsToRows(L,RowTranspose),
	%uniqueElemInLines(RowTranspose),
	(		checkCorrectLines(RowTranspose, Digits)
		->	true
		;	%writeln('failed col check'),
			false
	),
	cubeToRows(L,CubeLines),	% cubes have no duplicates
	%uniqueElemInLines(CubeLines),
	(		checkCorrectLines(CubeLines,Digits)
		->	true
		;	%writeln('failed cube check'),
			false
	), % move to end since adding diff in constrain
	(		checkCorrectLines(L,Digits)  % Rows have all digits
		->	true
		;	%writeln('failed row check'),
			false
	).

%%%% note something about flattening a list of empty lists causes problems
	
% given parallel matrices, L the source matrix
% and C the derived constraints matrix 
constrain([L|Ls],[C|Cs]):-  % write('constrain1: '),writeln(L),
	constrainLine(L,C),
	diff(L),  % speed up by trying to elim bad choices as early as possible... elim bad rows here
	updAvailChoices(L,Cs,NewCs), 
	constrain(Ls,NewCs) .
constrain([],[]). % :- writeln('constrain4: ').

% based on selected line elements, change range of
% future choices available.
updAvailChoices(_,[],[]).
updAvailChoices(L,[C|Cs],[NewC|NewCs]):-
	updatAvailLCh(L,C,NewC),
	updAvailChoices(L,Cs,NewCs).

% if you find a list in a line remove any appearances of the 
% selected value in line L in same column
updatAvailLCh([],[],[]).
updatAvailLCh([L|Ls],[E|Es],[Z|NewEs]):-
	not(integer(E)),
	(	
		select(L,E,Z)	% remove the selected number from Other choices in same col
	->	true
	;	Z=E	% L was not in set E, just return E
	),
	updatAvailLCh(Ls,Es,NewEs).
updatAvailLCh([L|Ls],[E|Es],[E|NewEs]):-
	integer(E),
	updatAvailLCh(Ls,Es,NewEs).



diff([_]).
diff([H|T]):-not(member(H,T)),diff(T).


constrainLine([],[]).
constrainLine([X|Xs],[_|Ys]):-
	integer(X),
	constrainLine(Xs,Ys).
constrainLine([X|Xs],[Y|Ys]):-
	not(integer(X)),  % write('constraining : '), write(X),write(' to '),writeln(Y),
	(		[A] = Y % singleton, assign it rather than member, speed up attempt
		->	X=A	%,!	% ,write('assigning: '),write(X),write('='),writeln(A)% was hoping direct assignment instesd of member might speed things up...
		;	member(X,Y)  %, write('binding : '),write(X),write(' '),writeln(Y)
	),    % member(X,Y),  % this is where we unify to the anonymous variables
	constrainLine(Xs,Ys).
	
% need logic to link allowed selection choices such that a two selection choices
% on the same row have mutual exclustion...a dynamic constraint matrix so that
% future choice is restricted by current choice. within the building of a matrix.
	
	
	
	
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% methods for permuting matrix, unfinished in favor
%% of following class method of generation of possible solutions

permuteMatrix(SMatrix,PMatrix):-
	pMatrix(SMatrix,PMatrix,_).  % // stop when unselected has no more choices

pMatrix(SMatrix,PMatrix,CMatrix):-
	hasChoicesM(SMatrix),
	iterM(SMatrix,PMatrix,OtherChoices),CMatrix=OtherChoices.

hasChoicesM([Line|_]):-
	hasChoicesL(Line).
hasChoicesM([Line|Lines]):-
	not(hasChoicesL(Line)),
	hasChoicesM(Lines).

hasChoicesL([E|Elems]):-
	integer(E),
	hasChoicesL(Elems).
hasChoicesL([E|_]):-
	not(integer(E)),
	E\=[].
hasChoicesL([]) :- false.
	
iterM([],[],[]).
iterM([Line|Lines],[NLine|NLines],[RemainingChoicesL|RemainChoicesM]):- % 2nd parm is selection, 3rd is unpicked
	iterL(Line,NLine,RemainingChoicesL), % NLine all first selections from choices, RemainingChoices are the selections unpicked
	iterM(Lines,NLines,RemainChoicesM).
	
iterL([],[],[]).
iterL([E|Elems],[E|NEs],[E|Remaining]):-
	integer(E),
	iterL(Elems,NEs,Remaining).
iterL([E|Elems],[Selected|NEs],[Remaining|OtherRemaining]):-
	not(integer(E)),
	pickFirst(E,Selected,Remaining), % found something to pick, pickit and carry on picking all first choices
	iterL(Elems,NEs,OtherRemaining).

pickFirst([E],E,E).  % if its the last elem put it into remaining as an elem, not a choice (no list). dont allow backtrack if this is selected
pickFirst([E|Elems],E,Elems).

	
%% strategy, pick first elem of all lists
%% return that as first permutation,
%% carry along matrix with lists and when you return, modify
%% the matrix to remove the first elem of the first list
%% recursively go to the first line( eg pick 1st elem of all lists.


collapseSingletonsM([],[]).
collapseSingletonsM([Line|Lines],[NLine|NLines]):-
	collapseLine(Line,NLine),
	collapseSingletonsM(Lines,NLines).

collapseLine([],[]).
collapseLine([E|Elems],[E|NEs]):-
	integer(E),
	collapseLine(Elems,NEs).
collapseLine([[E]|Elems],[E|NEs]):-
	collapseLine(Elems,NEs).
collapseLine([[E|Es]|Elems],[[E|Es]|NEs]):-
	Es\=[],
	collapseLine(Elems,NEs).
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% methods for finding constraints

intersectContraintsMatrix([],[]).
intersectContraintsMatrix([Line|Lines],[CLine|IMatrix]):- % writeln('interM'),
	intersectContraintsLine(Line,CLine),
	intersectContraintsMatrix(Lines,IMatrix).

intersectContraintsLine([],[]).
intersectContraintsLine([E|Elems],[E|CLine]):- % writeln('inter1'),
	integer(E),
	intersectContraintsLine(Elems,CLine).
intersectContraintsLine([[Row,Col,Cub]|Elems],[Inter|CLine]):- % writeln('inter2'),
	intersect(Row,Col,Res), % should be a list of 3 lists
	intersect(Res,Cub,Inter),
	intersectContraintsLine(Elems,CLine).
intersectContraintsLine([Constraint|Elems],[Inter|CLine]):- % writeln('inter3'),
	not(integer(Constraint)),
	[Row,Col,Cub] = Constraint,
	intersect(Row,Col,Res), % should be a list of 3 lists
	intersect(Res,Cub,Inter),
	intersectContraintsLine(Elems,CLine).
	
	

getblankpositions([],[],_).
getblankpositions([E|Elems],[N1|BlankPos],N):-
	not(integer(E)), 	% since underscore is a anon unbound var,  id integer vs '_' 
	N1 is N+1,
	getblankpositions(Elems,BlankPos,N1).
getblankpositions([E|Elems],BlankPos,N):-
	integer(E),
	N1 is N+1,
	getblankpositions(Elems,BlankPos,N1).


seeindexing(L):-
	flatten(L,Out),
	printall(Out).

printall([],_).
printall([L|List],N):-
	N1 is N+1,
	write(N1),tab(2),
	writeln(L),
	printall(List,N1).

% is the matrix shape 9x9
% do the numbers satisfy uniqueness in rows, cols and cubes.
conforms(L) :- % writeln('conforms'),
	len(L,N),N = 9, % number of lines
	eachlinesize(L),   % writeln('eachlinesize in conforms'),
	uniqueElemInLines(L), % no duplicates in lines
	colsToRows(L,RowTranspose), % writeln('RowTranspose'), printsudoku(RowTranspose),
	uniqueElemInLines(RowTranspose), % no duplicates i columns (transposed to lines)
	cubeToRows(L,CubeLines), %   writeln('Cubelines'), printsudoku(CubeLines),
	uniqueElemInLines(CubeLines).
	

eachlinesize([]).
eachlinesize([L|Lines]) :-
	len(L,N),
	N = 9,
	eachlinesize(Lines).

uniqueElemInLines([]).
uniqueElemInLines([L|Lines]):-
	uniqueElemInLine(L),
	uniqueElemInLines(Lines).
uniqueElemInLine([]).
uniqueElemInLine([E|Elems]):-
	not(integer(E)),		% eat the _ anonymous variables
	uniqueElemInLine(Elems).
uniqueElemInLine([E|Elems]):-
	integer(E),
	not(intmember(E,Elems)),
	uniqueElemInLine(Elems).

% arggg, underscore will match everything, I probably should substitue all underscores
% right up front but, noooo, instead, Im going to try to handle it everywhere....
intmember(E,_):-
	not(integer(E)).  % prevent underscore matching anything, treat as unique, _ not a member of anything
intmember(X,[Z|Zs]):-
	(		not(integer(Z))
		->	intmember(X,Zs)  % ignore Z=underscore
		;	X\=Z,            % is an integer and not match
			intmember(X,Zs)
	).  % prevent anything from matching underscore
intmember(X,[Y|_]):-
	integer(X),
	integer(Y),
	X=Y.		% true integer match, no underscores involved.

	
% ea missing elem is replaced by a list of 3 list with the 
% allowed row, col and cube values.
makeConstraints(L,CMatrix) :-  % writeln('constrained'),printsudoku(L),
	digits(Digits), % so we dont have to recreate on each seperate call
	replMatrix_(L,L2),
	correctLines(L2,Digits,RowMiss), %  writeln('lines missing'),printsudoku(RowMiss),
	correctCols(RowMiss,Digits,ColMiss), % writeln('cols missing '),printsudoku(ColMiss),
	correctCubes(ColMiss,Digits,CubeMiss), % writeln('cube missing '),printsudoku(CubeMiss),
	CMatrix=CubeMiss.  % all constraints added in.

	
replMatrix_([],[]).
replMatrix_([L|Lines],[NewLine|NewMatrix]):-
	replLine_(L,NewLine),
	replMatrix_(Lines,NewMatrix).

replLine_([],[]).	
replLine_([E|Elems],[E|NewElems]):-
	integer(E),
	replLine_(Elems,NewElems).
replLine_([E|Elems],[[]|NewElems]):-
	not(integer(E)),
	replLine_(Elems,NewElems).
	
/*
correctLines([],_,[]).
correctLines([L|Lines],Digits,Missing):- write('correctlines'),writeln(L),
	setdiff(Digits,L,LineMissing),	write('linemising '),writeln(LineMissing),
	correctLines(Lines,Digits,OtherLineMissing),
	Missing=[LineMissing|OtherLineMissing].
*/
% just check if all digits present and accounted for in line
checkCorrectLines([],_). %  :- writeln('end checkCorrectLines').
checkCorrectLines([L|Lines],Digits):- 
	setdiff(Digits,L,LineMissing),
	(		LineMissing=[]
		->	true
		;   %write('missing elems in line = '),
			%write(LineMissing),
			%write('source line = '),
			%writeln(L),
			false
	),
	checkCorrectLines(Lines,Digits).

% check and insert list of missing elements into every available variable
correctLines([],_,[]). %  :- writeln('end correctLines').
correctLines([L|Lines],Digits,[Lnew|NewMatrix]):- % write('correctlines321'),writeln(L), write('newmatrix'),writeln(NewMatrix),
	setdiff(Digits,L,LineMissing),	% write('linemising '),writeln(LineMissing),write('322L'),writeln(L),
	insertMissing(L,LineMissing,Lnew), % write('323L, '),writeln(Lnew),
	correctLines(Lines,Digits,NewMatrix).

insertMissing([],_,[]). % :-   writeln('insertmissing0').
insertMissing([E|Elems],LineMissing,[E|Lnew]):- 
	integer(E),		% write('insertmissing1: '),writeln(E),
	insertMissing(Elems,LineMissing,Lnew).
% insertMissing([E|Elems],LineMissing,[LineMissing|Lnew]):- % writeln('insertmissing2'),
%	not(integer(E)),  % finds _ or lists so we can append to _ or a list we already created.
%	insertMissing(Elems,LineMissing,Lnew).
insertMissing([[]|Elems],LineMissing,[[LineMissing]|Lnew]):- % writeln('insertmissing2'),
	insertMissing(Elems,LineMissing,Lnew).
insertMissing([[A]|Elems],LineMissing,[[A,LineMissing]|Lnew]):- % write('insertmissing3,LineMisssing='),writeln(LineMissing),
	insertMissing(Elems,LineMissing,Lnew).
insertMissing([[A,B]|Elems],LineMissing,[[A,B,LineMissing]|Lnew]):- % write('insertmissing4,LineMisssing='),writeln(LineMissing),
	insertMissing(Elems,LineMissing,Lnew). % kind of ugly but works. really should be able to collapse row and cube test and append in to one case
	
	
correctCols(L,Digits,Missing):-
	colsToRows(L,Transpose),     % transpose to turn columns into rows
	correctLines(Transpose,Digits,Result), % utilize same functor for inputing missing elems
	colsToRows(Result,Missing).  % transpose back to original matrix

correctCubes(L,Digits,Missing):- 
	cubeMToRows(L,CubeLines),  %  writeln('cubeMToRows'), printsudoku(CubeLines),
	correctLines(CubeLines,Digits,Result), % writeln('correctLines'),
	cuberowBackToM(Result,Missing).
	
% my transpose function for list of row lists...
%colsToRows([[],[],[]],[]).
colsToRows([],[]).
colsToRows([[]|Ls],[]):-  
	colsToRows(Ls,[]).
colsToRows(L,[NewLine|Result]):- 
	firstElemOfAllLines(L,NewLine,Headless),
	colsToRows(Headless,Result).

% remove first elem of each list within aggregate list and make into newline
% also return the original list of lists with each sublist having first elem removed (Headless)
firstElemOfAllLines([],[],[]).
firstElemOfAllLines([[E|Elems]|Lines],[E|NewLine],[Elems|Headless]):-
	firstElemOfAllLines(Lines,NewLine,Headless).

% works for _ in matrix.  Should consolidate all to one format but it was
% a struggle to get here so I leave it for posterity and use it for 
% the upfront conformance tests 
cubeToRows(L,CubeLines):- % writeln('cubeToRows'),  %direct access to all members of matrix, rows A-I, col 1 to 9
	flatten(L,Flat),
[	A1,A2,A3,A4,A5,A6,A7,A8,A9,
	B1,B2,B3,B4,B5,B6,B7,B8,B9,
	C1,C2,C3,C4,C5,C6,C7,C8,C9,
	D1,D2,D3,D4,D5,D6,D7,D8,D9,
	E1,E2,E3,E4,E5,E6,E7,E8,E9,
	F1,F2,F3,F4,F5,F6,F7,F8,F9,
	G1,G2,G3,G4,G5,G6,G7,G8,G9,
	H1,H2,H3,H4,H5,H6,H7,H8,H9,
	I1,I2,I3,I4,I5,I6,I7,I8,I9] = Flat,   % writeln('Assigned from L'),
	Cube1=[A1,A2,A3,B1,B2,B3,C1,C2,C3],
	Cube2=[A4,A5,A6,B4,B5,B6,C4,C5,C6],
	Cube3=[A7,A8,A9,B7,B8,B9,C7,C8,C9],
	Cube4=[D1,D2,D3,E1,E2,E3,F1,F2,F3],
	Cube5=[D4,D5,D6,E4,E5,E6,F4,F5,F6],
	Cube6=[D7,D8,D9,E7,E8,E9,F7,F8,F9],
	Cube7=[G1,G2,G3,H1,H2,H3,I1,I2,I3],
	Cube8=[G4,G5,G6,H4,H5,H6,I4,I5,I6],
	Cube9=[G7,G8,G9,H7,H8,H9,I7,I8,I9],
    CubeLines=[Cube1,Cube2,Cube3,Cube4,Cube5,Cube6,Cube7,Cube8,Cube9].

% expects matrix L to have no _, substitution with [] or filled in lists for
% missing elements with potential row, col replacements.
cubeMToRows(L,CubeLines):- % writeln('cubeToRows'),  %direct access to all members of matrix, rows A-I, col 1 to 9
	%flatten(L,Flat),
[	[A1,A2,A3,A4,A5,A6,A7,A8,A9],
	[B1,B2,B3,B4,B5,B6,B7,B8,B9],
	[C1,C2,C3,C4,C5,C6,C7,C8,C9],
	[D1,D2,D3,D4,D5,D6,D7,D8,D9],
	[E1,E2,E3,E4,E5,E6,E7,E8,E9],
	[F1,F2,F3,F4,F5,F6,F7,F8,F9],
	[G1,G2,G3,G4,G5,G6,G7,G8,G9],
	[H1,H2,H3,H4,H5,H6,H7,H8,H9],
	[I1,I2,I3,I4,I5,I6,I7,I8,I9]] = L,   % writeln('Assigned from L'),
	Cube1=[A1,A2,A3,B1,B2,B3,C1,C2,C3],
	Cube2=[A4,A5,A6,B4,B5,B6,C4,C5,C6],
	Cube3=[A7,A8,A9,B7,B8,B9,C7,C8,C9],
	Cube4=[D1,D2,D3,E1,E2,E3,F1,F2,F3],
	Cube5=[D4,D5,D6,E4,E5,E6,F4,F5,F6],
	Cube6=[D7,D8,D9,E7,E8,E9,F7,F8,F9],
	Cube7=[G1,G2,G3,H1,H2,H3,I1,I2,I3],
	Cube8=[G4,G5,G6,H4,H5,H6,I4,I5,I6],
	Cube9=[G7,G8,G9,H7,H8,H9,I7,I8,I9],
    CubeLines=[Cube1,Cube2,Cube3,Cube4,Cube5,Cube6,Cube7,Cube8,Cube9].

%	assign(
%	/*	A1,A2,A3,A4,A5,A6,A7,A8,A9,
%		B1,B2,B3,B4,B5,B6,B7,B8,B9,
%		C1,C2,C3,C4,C5,C6,C7,C8,C9,
%		D1,D2,D3,D4,D5,D6,D7,D8,D9,
%		E1,E2,E3,E4,E5,E6,E7,E8,E9,
%		F1,F2,F3,F4,F5,F6,F7,F8,F9,
%		G1,G2,G3,G4,G5,G6,G7,G8,G9,
%		H1,H2,H3,H4,H5,H6,H7,H8,H9,
%		I1,I2,I3,I4,I5,I6,I7,I8,I9, */
%		Flat):-
%	[	[A1,A2,A3,A4,A5,A6,A7,A8,A9],
%		[B1,B2,B3,B4,B5,B6,B7,B8,B9],
%		[C1,C2,C3,C4,C5,C6,C7,C8,C9],
%		[D1,D2,D3,D4,D5,D6,D7,D8,D9],
%		[E1,E2,E3,E4,E5,E6,E7,E8,E9],
%		[F1,F2,F3,F4,F5,F6,F7,F8,F9],
%		[G1,G2,G3,G4,G5,G6,G7,G8,G9],
%		[H1,H2,H3,H4,H5,H6,H7,H8,H9],
%		[I1,I2,I3,I4,I5,I6,I7,I8,I9]] = L,
%		flatten(L,Flat).
	
	
	
cuberowBackToM(CubeRows,M):-
	[[A1,A2,A3,B1,B2,B3,C1,C2,C3],
	 [A4,A5,A6,B4,B5,B6,C4,C5,C6],
	 [A7,A8,A9,B7,B8,B9,C7,C8,C9],
	 [D1,D2,D3,E1,E2,E3,F1,F2,F3],
	 [D4,D5,D6,E4,E5,E6,F4,F5,F6],
	 [D7,D8,D9,E7,E8,E9,F7,F8,F9],
	 [G1,G2,G3,H1,H2,H3,I1,I2,I3],
	 [G4,G5,G6,H4,H5,H6,I4,I5,I6],
	 [G7,G8,G9,H7,H8,H9,I7,I8,I9]] = CubeRows,
 M=[[A1,A2,A3,A4,A5,A6,A7,A8,A9],
	[B1,B2,B3,B4,B5,B6,B7,B8,B9],
	[C1,C2,C3,C4,C5,C6,C7,C8,C9],
	[D1,D2,D3,D4,D5,D6,D7,D8,D9],
	[E1,E2,E3,E4,E5,E6,E7,E8,E9],
	[F1,F2,F3,F4,F5,F6,F7,F8,F9],
	[G1,G2,G3,G4,G5,G6,G7,G8,G9],
	[H1,H2,H3,H4,H5,H6,H7,H8,H9],
	[I1,I2,I3,I4,I5,I6,I7,I8,I9]].
	 
    
    
digit(1).
digit(2).
digit(3).
digit(4).
digit(5).
digit(6).
digit(7).
digit(8).
digit(9).
blank('_').


digits(Digits) :- findall(Digit,digit(Digit),Digits).

member(X,[X|_]).
member(X,[_|Ys]) :-
	member(X,Ys).



	

intersect([],_,[]).
intersect([A|As],B,[A|Result]):-
	member(A,B),
	intersect(As,B,Result).
intersect([A|As],B,Result):-
	not(member(A,B)),
	intersect(As,B,Result).


setdiff([],_,[]).
setdiff([A|As],B,[A|Result]) :-
	not(member(A,B)),
	setdiff(As,B,Result).
setdiff([A|As],B,Result) :-
	member(A,B),
	setdiff(As,B,Result).

append([X|Y],Z,[X|W]) :- append(Y,Z,W).
append([],X,X).

len([],0).
len([_|Xs],N):-
	len(Xs,N1),
	N is N1 + 1.





  % given a list, recursively take out an element and permute the remainder
  % eg ?- permute([a,b,c],Z).
permute(Xs,[Z|Zs]) :- select(Z,Xs,Ys),permute(Ys,Zs).
permute([],[]). 


