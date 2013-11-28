%CLUE
%Julian Pradinuk            Tucker Buchy
%44962090                   34091090
%o6o7                       g9c7
%jampradinuk@gmail.com      tuckerbuchy@gmail.com

:- dynamic nPlayers/1, firstPlayer/1, hasCard/2, totalSuggestions/1, cardSuggestionCount/2.

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

%updates the total number of suggestions,
% this is just a tally keeping track of the number 
updateTotalSuggestions :- 
	totalSuggestions(N),
	N1 is N + 1,
	retractall(totalSuggestions(_)),
	assert(totalSuggestions(N1)).

%updates the number of times a card has appeared in a suggestion
updateCardSuggestions(Card) :-
	cardSuggestionCount(Card, N) ->
		%update existing count
		(N1 is N + 1,
		retractall(cardSuggestionCount(Card, _)),
		assert(cardSuggestionCount(Card, N1)));
		%start one at 1
		assert(cardSuggestionCount(Card, 1)).

% play clue!
%
clue :- clear,
		write('Welcome to Tudor Mansion. Your host, Mr. John Boddy has met an untimely end...\n'),
		write('Please follow the game instructions *carefully*.\n'),
		setup, 
		firstPlayer(P),
		play(P).

% clear the database
%
clear :-
	retractall(hasCard(_,_)),
	retractall(firstPlayer(_)),
	retractall(nPlayers(_)),
	retractall(totalSuggestions(_)),
	retractall(cardSuggestionCount(_,_)).

% game setup:
%	1. How many players?
%	2. Who starts?
%	3. How many cards are you holding and what are they?
%
setup :-
	assert(totalSuggestions(0)),
	readNPlayers,
	readFirstPlayer,
	readMyCards.

readNPlayers :-
	write('>>How many players?\n>>>Enter a number within the range 3-6 followed by a full stop before hitting return.\n'),
	read(NPlayers),
	(integer(NPlayers),between(3,6,NPlayers)) ->
		(writef("You are player 1.\nThe player to your left is player 2.\nThe player to your right is player %d.\n", [NPlayers]),
			assert(nPlayers(NPlayers)));
		(write('Bad input, try again.\n'),readNPlayers).

readFirstPlayer :-
	nPlayers(NPlayers),
	writef(">>Who starts?\n>>>Enter a number within the range 1-%d followed by a full stop before hitting return.\n", [NPlayers]),
	read(FirstPlayer),
	(integer(FirstPlayer),between(1,NPlayers,FirstPlayer)) -> 
		(writef("Player %d starts.\n", [FirstPlayer]),
			assert(firstPlayer(FirstPlayer)));
		(write('Bad input, try again.\n'),readFirstPlayer).

readMyCards :-
	write('>>How many cards were you dealt?\n>>>Enter a number greater than or equal to 0 followed by a full stop before hitting return.\n'),
	read(NCards),
	(integer(NCards),NCards >= 0) ->
		myCards(NCards, 0);
		(write('Bad input, try again.\n'),readMyCards).

%reads in the cards player 1 card.
myCards(N, I) :-
	I < N ->
		(writef(">>>>Card %d\n", [I]),readCard(1),I1 is I + 1,myCards(N, I1)); true.

readCard(Player) :-
	write('>>>>>Type?\n>>>>>>Enter:\n>>>>>>>1 for suspect\n>>>>>>>2 for room\n>>>>>>>3 for weapon\nfollowed by a full stop before hitting return.\n'),
	read(CardType),
	(integer(CardType),between(1,3,CardType)) ->
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

%writes the detective journal to the console
% 	info in the journal is:
%		card, owner, times it has been suggested
writeDetectiveJournal :-
	writeln('------------------------DETECTIVE JOURNAL------------------------\n'),
	writeln('\tSuspects:'),
	forall(suspect(X), writeCard(X)),
	writeln('\tRooms:'),
	forall(room(X), writeCard(X)),
	writeln('\tWeapons:'),
	forall(weapon(X), writeCard(X)),
	writeln('\n-----------------------------------------------------------------\n').

% writes
%		X P 	if hasCard(P, X)
%		X 		otherwise
writeCard(Card) :-
	(cardSuggestionCount(Card, Count) ->
		true;
		Count is 0),
	totalSuggestions(Total),
	(hasCard(Owner,Card) -> 
		writef("\t\t%d -> Card Owner is %d", [Card,Owner]);
		writef("\t\t%d", [Card])),
	writef("\n\t\t\tseen in %d of %d suggestions.\n", [Count, Total]).

play(Player) :-
	(Player =:= 1 -> 
		(write('>>It\'s your turn.\n'), isAccusationReady);
		writef(">>It\'s player %d\'s turn.\n", [Player])),
	writeTurnOptions,
	read(TurnOption),
	(integer(TurnOption),between(1,4,TurnOption)) -> 
		turn(Player, TurnOption);
		(write('Bad input, try again.\n'),play(Player)).

turnOption(1,"read your detective journal").
turnOption(2,"make a suggestion by this turn\'s player").
turnOption(3,"report card shown").
turnOption(4,"have a suggestion recommended for you").
turnOption(5,"end this turn").
writeTurnOptions :-
	write('>>>Enter:\n'),
	forall(turnOption(No,Option), writef(">>>>%d to %s\n",[No,Option])),
	write('>>>>>followed by a full stop before hitting return.\n').

%%%TURN OPTIONS
%1 is for writing the detective journal out
turn(Player, 1) :-
	writeDetectiveJournal,
	play(Player).

%2 is for making a suggestion
% note this is both for player 1 (us), and for recording other players suggestions down.
turn(Player, 2) :-
	makeSuggestion(Player),
	play(Player).

%3 is for reporting a card shown. This is mostly useful when we are shown a card after our own suggestion,
% but could be useful for some other mistakes the player makes (droping a card on the ground, us cheating ;-))
turn(Player, 3) :-
	reportCard,
	play(Player).

%4 is for having a suggestion recommended to you
turn(Player, 4) :-
	recommendSuggestion,
	play(Player).

%5 is for ending the turn of the current player.
turn(Player, 5) :-
	nPlayers(N),
	Player1 is (Player mod N) + 1,
	play(Player1).

%reads in a suggestion
makeSuggestion(1) :- 
	suggestion(_,_,_).
makeSuggestion(_) :-
	suggestion(Suspect,Room,Weapon),
	nPlayers(NPlayers),
	writef(">>>Who showed a card?\n>>>>Enter a number within the range 1-%d followed by a full stop before hitting return.\n", [NPlayers]),
	read(ShowPlayer),
	checkIfCanInfer(ShowPlayer,[Suspect,Room,Weapon]).
	
suggestion(Suspect,Room,Weapon) :-
	writeSuspects,
	read(SuspectNo),
	writeRooms,
	read(RoomNo),
	writeWeapons,
	read(WeaponNo),
	(suspectno(SuspectNo,Suspect),roomno(RoomNo,Room),weaponno(WeaponNo,Weapon)) ->
		true, updateTotalSuggestions, updateCardSuggestions(Suspect), updateCardSuggestions(Room), updateCardSuggestions(Weapon);
		(write('Bad input, try again.\n'),suggestion(_,_,_)).

reportCard :-
	nPlayers(NPlayers),
	writef(">>>Whose card were you shown?\n>>>>Enter a number within the range 2-%d followed by a full stop before hitting return.\n", [NPlayers]),
	read(ShowPlayer),
	(integer(ShowPlayer),between(2,NPlayers,ShowPlayer)) ->
		readCard(ShowPlayer);
		(write('Bad input, try again.\n'),reportCard).

%tells the user via message if we were able to successfully infer a card
checkIfCanInfer(ShowPlayer, SuggestionCards) :-
	inferCardShown(ShowPlayer, SuggestionCards) ->
		write('################################################\nWe were able to infer a card! Check your journal.\n################################################\n');
		write('Learned nothing... :(\n').

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
	assert(hasCard(ShowPlayer, Card)).

%prints a message if we are able to make an accusation with certainty.
isAccusationReady :-
	accusationReady(M,R,W) -> 
	writef('########################################################\nDude! Make an accusation! It was %d in the %d with the %d!\n########################################################\n', [M, R, W]);
	true.

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

% recommend suggestion
%	prints the most frequently suggested cards
%	for which the card
%
recommendSuggestion :-
	recommendSuspect(S),
	recommendRoom(R),
	recommendWeapon(W),
	writef('########################################################\nYou should suggest: %d in the %d with %d.\n########################################################\n',[S,R,W]).

recommendSuspect(S) :-
	findall(Count, (not(hasCard(_,Card),suspect(Card),cardSuggestionCount(Card,Count))), CardCounts),
	maxL(CardCounts, MaxCount),
	not(hasCard(_,MaxCard)),
	suspect(MaxCard),
	cardSuggestionCount(MaxCard, MaxCount).

recommendRoom(R) :-
	findall(cardSuggestionCount(Card,Count),(room(Card),not(hasCard(_,Card))), CardSuggestionCounts),
	maxL(CardCounts, MaxCount),
	not(hasCard(_,MaxCard)),
	room(MaxCard),
	cardSuggestionCount(MaxCard, MaxCount).

recommendWeapon(W) :-
	findall(cardSuggestionCount(Card,Count),(weapon(Card),not(hasCard(_,Card))), CardSuggestionCounts),
	maxL(CardCounts, MaxCount),
	not(hasCard(_,MaxCard)),
	weapon(MaxCard),
	cardSuggestionCount(MaxCard, MaxCount).


%max2(X1, X2, max(X1,X2))
max2(X, Y, X) :-
    X >= Y.
max2(X, Y, Y) :-
    X < Y.

%max of list 
maxL([X], X).
maxL([H | T], X) :-
    maxL(T, I),
    maxOfTwo(H, I, X).

