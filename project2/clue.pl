%CLUE
%Julian Pradinuk     +      Tucker Buchy
%44962090                   34091090
%o6o7                       g9c7
%jampradinuk@gmail.com      tuckerbuchy@gmail.com

:- dynamic suspect/1, weapon/1, room/1, nPlayers/1, firstPlayer/1.
%every suspect could be the murderer
suspect(colonelMustard).
suspect(missScarlet).
suspect(professorPlum).
suspect(mrGreen).
suspect(mrsWhite).
suspect(mrsPeacock).

suspectno(1,colonelMustard).
suspectno(2,missScarlet).
suspectno(3,professorPlum).
suspectno(4,mrGreen).
suspectno(5,mrsWhite).
suspectno(6,mrsPeacock).

%every room could be the kill room
room(kitchen).
room(lounge).
room(study).
room(library).
room(diningRoom).
room(hall).
room(billiardRoom).
room(conservatory).
room(ballroom).

%every weapon could be the murder weapon
weapon(rope).
weapon(leadPipe).
weapon(knife).
weapon(wrench).
weapon(candlestick).
weapon(pistol).

readDetectiveJournal :-
	writeln('suspects:'),
	forall(suspect(X),writeln(X)),
	writeln('weapons:'),
	forall(weapon(X),writeln(X)),
	writeln('rooms:'),
	forall(room(X),writeln(X)).

clue :- clear,
		setup, 
		firstPlayer(P),
		nPlayers(N),
		play(P,N).

yayOrNay('yes') :- suggest.
yayOrNay('no') :- true.


play(1, N) :-	write('It\'s your turn. Make a suggestion?\n'),
				read(YayOrNay),
				yayOrNay(YayOrNay), 
				play(2, N).
play(P, N) :-	write('It is player '),
				write(P),
				write('\'s turn.\n'),
				P1 is (P mod N) + 1, 
				play(P1, N).

clear :-
	retractall(firstPlayer(X)),
	retractall(nPlayers(X)).

setup :-
	write('How many players?\n'),
	read(NPlayers),
	member(NPlayers,[3,4,5,6]),
	assert(nPlayers(NPlayers)),
	write('You are player 1. The player to your left is player 2. The player to your right is player '),
	write(NPlayers),
	write('. Who starts?\n'),
	read(FirstPlayer),
	assert(firstPlayer(FirstPlayer)).


suggest :-
	write('Suspect?\nEnter:\n\t1 for Colonel Mustard,\n\t2 for Miss Scarlet,\n\t
		3 for Professor Plum,\n\t4 for Mr. Green,\n\t5 for Mrs. White\n\t
		or 6 for Mrs. Peacock,\n\tfollowed by a full stop and hit enter!\n'),
	read(Suspect),
	member(Suspect,[1,2,3,4,5,6]),
	write('Room?\nEnter:\n\t1 for Kitchen Lounge Study Library Dining Room Hall Billiard Room Conservatory Ballroom'),
	read(Room),
	member(Room,[1,2,3,4,5,6,7,8,9]),
	write('Weapon?\nEnter:\n\t1 for Rope\n\t2 for Lead Pipe\n\t3 for Knife\n\t4 for Wrench\n\t5 for Candlestick\n\t6 for Pistol\n:'),
	read(Weapon),
	member(Weapon,[1,2,3,4,5,6]),
	write('I suggested '),
	write(Suspect),
	write(' in the '),
	write(Room),
	write(' with the '),
	write(Weapon),
	write('.').
