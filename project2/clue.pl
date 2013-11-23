%CLUE
%Julian Pradinuk     +      Tucker Buchy
%44962090                   34091090
%o6o7                       g9c7
%jampradinuk@gmail.com      tuckerbuchy@gmail.com

:- dynamic suspect/1, weapon/1, room/1, nPlayers/1, firstPlayer/1, hasCard/2.
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

roomno(1,kitchen).
roomno(2,lounge).
roomno(3,study).
roomno(4,library).
roomno(5,diningRoom).
roomno(6,hall).
roomno(7,billiardRoom).
roomno(8,conservatory).
roomno(9,ballroom).

%every weapon could be the murder weapon
weapon(rope).
weapon(leadPipe).
weapon(knife).
weapon(wrench).
weapon(candlestick).
weapon(pistol).

weaponno(1,rope).
weaponno(2,leadPipe).
weaponno(3,knife).
weaponno(4,wrench).
weaponno(5,candlestick).
weaponno(6,pistol).

card(X) :- 
	suspect(X);
	room(X);
	weapon(X).

clue :- clear,
		setup, 
		firstPlayer(P),
		play(P).

hasCards(Player, Cards) :-
	findall(Card, hasCard(Player, Card), Cards).

inferCardShown(ShowPlayer, SuggestionCards) :-
	select(Card, SuggestionCards, TwoCards),
	[Card1, Card2] = TwoCards,
	not(hasCard(_,Card)),
	hasCard(Y, Card1),
	Y \= ShowPlayer,
	hasCard(Z, Card2),
	Z \= ShowPlayer,
	writef("Aha! Player %d has the card %d.\n", [ShowPlayer, Card]),
	assert(hasCard(ShowPlayer, Card)).

readDetectiveJournal :-
	writeln('\tsuspects:'),
	forall(suspect(X), printCard(X)),
	writeln('\tweapons:'),
	forall(weapon(X), printCard(X)),
	writeln('\trooms:'),
	forall(room(X), printCard(X)).

%prints
%		card player 	if hasCard(player, card)
%		card 			otherwise
printCard(X) :-
	hasCard(P,X) -> writef("\t\t%d %d\n", [X,P]); writef("\t\t%d\n", [X]).

turn(Player, 1) :- suggest(Player),
					play(Player).
turn(Player, 2) :- readDetectiveJournal,
					play(Player).
turn(Player, 3) :- reportCard,
					play(Player).
turn(Player, 4) :-  nPlayers(N),
					Player1 is (Player mod N) + 1,
					play(Player1).

%suspects
reportCard(1, Suspect) :- printSuspects,
					read(SuspectNo),
					suspectno(SuspectNo,Suspect).

%rooms
reportCard(2, Room) :- printRooms,
					read(RoomNo),
					roomno(RoomNo, Room).
%weapons
reportCard(3, Weapon) :- printWeapons,
					read(WeaponNo),
					weaponno(WeaponNo,Weapon).


proposeSuggestion :- 
	suspect(S),not(hasCard(_,S)),
	room(R),not(hasCard(_,R)),
	weapon(W),not(hasCard(_,W)),
	writef("You could suggest %d in the %d with the %d.\n", [S,R,W]).

reportCard :- write('Whose card were you shown?\n'),
			read(Player),
			reportCard(Player).

reportCard(HasPlayer) :- 
			  write('Was the card a suspect (1.), room (2.) or weapon (3.)?\n'),
			  	read(CardOption),
			  	reportCard(CardOption, Card),
			  	assert(hasCard(HasPlayer, Card)).


play(1) :-	write('It\'s your turn.\n'), % Make a suggestion? Enter y or n \c
					%followed by a full stop before hitting return.\n'),
				read(TurnOption),
				turn(1, TurnOption).
play(P) :-	write('It\'s player '),
				write(P),
				write('\'s turn.\n'), % Did they make a suggestion? Enter y or n \c 
				%	followed by a full stop before hitting return.\n'),
				%read(YayOrNay),
				%yayOrNay(YayOrNay),
				%P1 is (P mod N) + 1, 
				%play(P1, N).
				read(TurnOption),
				turn(P, TurnOption).

clear :-
	retractall(hasCard(_,_)),
	retractall(firstPlayer(_)),
	retractall(nPlayers(_)).

setup :-
	write('How many players?\n Enter a number within the range \c
		3-6 followed by a full stop before hitting return.\n'),
	read(NPlayers),
	between(3,6,NPlayers),
	assert(nPlayers(NPlayers)),
	writef("You are player 1.\n\c
			The player to your left is player 2.\n\c
			The player to your right is player %d.\n\c
			Who starts?\n\c
			Enter a number within the range 1-%d followed by a full stop before hitting return.\n", [NPlayers, NPlayers]),
	read(FirstPlayer),
	between(1,NPlayers,FirstPlayer),
	assert(firstPlayer(FirstPlayer)),
	write('How many cards are you holding?\n'),
	read(NoCards),
	mycards(NoCards).

mycards(0).
mycards(N) :- 	reportCard(1),
				N1 is N - 1,
				mycards(N1).

printSuspects :-
	write('\t1 for Colonel Mustard,\n\t2 for Miss Scarlet,\n\t3 for Professor Plum,\n\t4 for Mr. Green,\n\t5 for Mrs. White\n\t6 for Mrs. Peacock\n').
printRooms :-
	write('\t1 for Kitchen,\n\t2 for Lounge,\n\t3 for Study,\n\t4 for Library,\n\t5 for Dining Room,\n\t6 for Hall,\n\t7 for Billiard Room,\n\t8 for Conservatory,\n\t9 for Ballroom\n').
printWeapons :-
	write('\t1 for Rope,\n\t2 for Lead Pipe,\n\t3 for Knife,\n\t4 for Wrench,\n\t5 for Candlestick,\n\t6 for Pistol\n').

suggest(1) :-
	suggestion(_, _, _).
suggest(_) :-
	suggestion(Suspect, Room, Weapon),
	write('Who showed a card?\n'),
	read(ShowPlayer),
	inferCardShown(ShowPlayer, [Suspect,Room,Weapon]).

suggestion(Suspect, Room, Weapon) :-
	write('Suspect?\nEnter:\n'),
	printSuspects,
	read(SuspectNo),
	between(1,6,SuspectNo),
	write('Room?\nEnter:\n'),
	printRooms,
	read(RoomNo),
	between(1,9,RoomNo),
	write('Weapon?\nEnter:\n'),
	printWeapons,
	read(WeaponNo),
	between(1,6,WeaponNo),
	suspectno(SuspectNo, Suspect),
	roomno(RoomNo, Room),
	weaponno(WeaponNo, Weapon).
