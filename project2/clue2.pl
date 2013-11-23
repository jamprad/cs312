%CLUE
%Julian Pradinuk     +      Tucker Buchy
%44962090                   34091090
%o6o7                       g9c7
%jampradinuk@gmail.com      tuckerbuchy@gmail.com

:- dynamic nPlayers/1, firstPlayer/1, hasCard/2.

suspect(colonelMustard).
suspect(missScarlet).
suspect(professorPlum).
suspect(mrGreen).
suspect(mrsWhite).
suspect(mrsPeacock).

room(kitchen).
room(lounge).
room(study).
room(library).
room(diningRoom).
room(hall).
room(billiardRoom).
room(conservatory).
room(ballroom).

weapon(rope).
weapon(leadPipe).
weapon(knife).
weapon(wrench).
weapon(candlestick).
weapon(pistol).

suspectno(1,colonelMustard).
suspectno(2,missScarlet).
suspectno(3,professorPlum).
suspectno(4,mrGreen).
suspectno(5,mrsWhite).
suspectno(6,mrsPeacock).

roomno(1,kitchen).
roomno(2,lounge).
roomno(3,study).
roomno(4,library).
roomno(5,diningRoom).
roomno(6,hall).
roomno(7,billiardRoom).
roomno(8,conservatory).
roomno(9,ballroom).

weaponno(1,rope).
weaponno(2,leadPipe).
weaponno(3,knife).
weaponno(4,wrench).
weaponno(5,candlestick).
weaponno(6,pistol).


% play clue!
%
clue :- clear,
		setup, 
		firstPlayer(P),
		play(P).

% clear the database
%
clear :-
	retractall(hasCard(_,_)),
	retractall(firstPlayer(_)),
	retractall(nPlayers(_)).

% game setup:
%	1. How many players?
%	2. Who starts?
%	3. How many cards are you holding and what are they?
%
setup :-
	readNPlayers,
	readFirstPlayer,
	readMyCards.

readNPlayers :-
	write('>>How many players?\n>>>Enter a number within the range 3-6 followed by a full stop before hitting return.\n'),
	read(NPlayers),
	between(3,6,NPlayers) ->
		(writef("You are player 1.\nThe player to your left is player 2.\nThe player to your right is player %d.\n", [NPlayers]),
			assert(nPlayers(NPlayers)));
		(write('Bad input, try again.\n'),readNPlayers).

readFirstPlayer :-
	nPlayers(NPlayers),
	writef(">>Who starts?\n>>>Enter a number within the range 1-%d followed by a full stop before hitting return.\n", [NPlayers]),
	read(FirstPlayer),
	between(1,NPlayers,FirstPlayer) -> 
		(writef("Player %d starts.\n", [FirstPlayer]),
			assert(firstPlayer(FirstPlayer)));
		(write('Bad input, try again.\n'),readFirstPlayer).

readMyCards :-
	write('>>How many cards were you dealt?\n>>>Enter a number greater than 0 followed by a full stop before hitting return.\n'),
	read(NCards),
	NCards > 0 ->
		myCards(NCards, 0);
		(write('Bad input, try again.\n'),readMyCards).

myCards(N, N).
myCards(N, I) :- 
	I1 is I + 1,
	writef(">>>>Card %d\n", [I1]),
	readCard(1),
	myCards(N, I1).

readCard(Player) :-
	write('>>>>>Type?\n>>>>>>Enter:\n>>>>>>>1 for suspect\n>>>>>>>2 for room\n>>>>>>>3 for weapon\nfollowed by a full stop before hitting return.\n'),
	read(CardType),
	between(1,3,CardType) ->
		readCard(Player, CardType);
		(write('Bad input, try again.\n'),readCard(Player)).

%suspect
readCard(Player, 1) :-
	writeSuspects,
	read(SuspectNo),
	suspectno(SuspectNo, Suspect) ->
		assert(hasCard(Player,Suspect));
		(write('Bad input, try again.\n'),readCard(Player, 1)).
%room
readCard(Player, 2) :-
	writeRooms,
	read(RoomNo),
	roomno(RoomNo, Room) ->
		assert(hasCard(Player,Room));
		(write('Bad input, try again.\n'),readCard(Player, 2)).
%weapon
readCard(Player, 3) :-
	writeWeapons,
	read(WeaponNo),
	weaponno(WeaponNo, Weapon) ->
		assert(hasCard(Player,Weapon));
		(write('Bad input, try again.\n'),readCard(Player, 3)).

writeSuspects :-
	write('>>>>>Suspect? Enter:\n'),
	forall(suspectno(No,Suspect), writef(">>>>>>%d for %d\n", [No, Suspect])),
	write('>>>>>>>followed by a full stop before hitting return.\n').

writeRooms :-
	write('>>>>>Room? Enter:\n'),
	forall(roomno(No,Room), writef(">>>>>>%d for %d\n", [No, Room])),
	write('>>>>>>>followed by a full stop before hitting return.\n').

writeWeapons :-
	write('>>>>>Weapon? Enter:\n'),
	forall(weaponno(No,Weapon), writef(">>>>>>%d for %d\n", [No, Weapon])),
	write('>>>>>>>followed by a full stop before hitting return.\n').

writeDetectiveJournal :-
	writeln('------------DETECTIVE JOURNAL------------\n'),
	writeln('\tSuspects:'),
	forall(suspect(X), writeCard(X)),
	writeln('\tRooms:'),
	forall(room(X), writeCard(X)),
	writeln('\tWeapons:'),
	forall(weapon(X), writeCard(X)),
	writeln('\n-----------------------------------------\n').

% writes
%		X P 	if hasCard(P, X)
%		X 		otherwise
writeCard(X) :-
	hasCard(P,X) -> 
		writef("\t\t%d %d\n", [X,P]);
		writef("\t\t%d\n", [X]).

play(Player) :-
	(Player =:= 1 -> 
		write('>>It\'s your turn.\n');
		writef(">>It\'s player %d\'s turn.\n", [Player])),
	writeTurnOptions,
	read(TurnOption),
	between(1,4,TurnOption) -> 
		turn(Player, TurnOption);
		(write('Bad input, try again.\n'),play(Player)).

turnOption(1,"read your detective journal").
turnOption(2,"make a suggestion by this turn\'s player").
turnOption(3,"report card shown").
turnOption(4,"end this turn").
writeTurnOptions :-
	write('>>>Enter:\n'),
	forall(turnOption(No,Option), writef(">>>>%d to %s\n",[No,Option])),
	write('>>>>>followed by a full stop before hitting return.\n').

turn(Player, 1) :-
	writeDetectiveJournal,
	play(Player).

turn(Player, 2) :-
	makeSuggestion(Player),
	play(Player).

turn(Player, 3) :-
	reportCard,
	play(Player).

turn(Player, 4) :-
	nPlayers(N),
	Player1 is (Player mod N) + 1,
	play(Player1).

makeSuggestion(Player) :-
	suggestion(Suspect,Room,Weapon),
	Player =:= 1 ->
		true;
		(nPlayers(NPlayers),
		writef(">>>Who showed a card?\n>>>>Enter a number within the range 1-%d followed by a full stop before hitting return.\n", [NPlayers]),
		read(ShowPlayer),
		(ShowPlayer =:= 1 ->
			true;
			inferCardShown(ShowPlayer,[Suspect,Room,Weapon]))).

suggestion(Suspect,Room,Weapon) :-
	writeSuspects,
	read(SuspectNo),
	writeRooms,
	read(RoomNo),
	writeWeapons,
	read(WeaponNo),
	(suspectno(SuspectNo,Suspect),roomno(RoomNo,Room),weaponno(WeaponNo,Weapon)) ->
		true;
		(write('Bad input, try again.\n'),suggestion(_,_,_)).

reportCard :-
	nPlayers(NPlayers),
	writef(">>>Whose card were you shown?\n>>>>Enter a number within the range 2-%d followed by a full stop before hitting return.\n", [NPlayers]),
	read(ShowPlayer),
	between(2,NPlayers,ShowPlayer) ->
		readCard(ShowPlayer);
		(write('Bad input, try again.\n'),reportCard).

inferCardShown.

% returns true and updates the database 
%	if the card shown can be (and has not already been) inferred
%
inferCardShown(ShowPlayer, SuggestionCards) :-
	select(Card, SuggestionCards, TwoCards),
	[Card1, Card2] = TwoCards,
	not(hasCard(_,Card)),
	hasCard(Y, Card1),
	Y \= ShowPlayer,
	hasCard(Z, Card2),
	Z \= ShowPlayer,
	writef("Aha! Player %d has the card %d. I updated your detective journal for you.\n", [ShowPlayer, Card]),
	assert(hasCard(ShowPlayer, Card)).

% returns true if we are ready to make the accusation:
%	M in the R with the W!
%
accusationReady(M,R,W) :-
	findall(X,(suspect(X),not(hasCard(_,X))),Suspects),
	findall(Y,(room(Y),not(hasCard(_,Y))),Rooms),
	findall(Z,(weapon(Z),not(hasCard(_,Z))),Weapons),
	[M] = Suspects,
	[R] = Rooms,
	[W] = Weapons.

