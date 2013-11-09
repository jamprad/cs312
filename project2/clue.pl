%CLUE
%Julian Pradinuk     +      Tucker Buchy
%44962090                   34091090
%o6o7                       g9c7
%jampradinuk@gmail.com      tuckerbuchy@gmail.com

%every suspect could be the murderer
suspect(colonelMustard).
suspect(missScarlet).
suspect(professorPlum).
suspect(mrGreen).
suspect(mrsWhite).
suspect(mrsPeacock).

%every weapon could be the murder weapon
weapon(rope).
weapon(leadPipe).
weapon(knife).
weapon(wrench).
weapon(candlestick).
weapon(pistol).

%every room could be the kill room
room(kitchen).
room(lounge).
room(study).
room(library).
room(diningRoom).
room(hall).
room(billiardsRoom).
room(conservatory).
room(ballroom).

suggest :-
	write('Player?'),
	read_term(Player, [colonelMustard,missScarlet,professorPlum,mrGreen,mrsWhite,mrsPeacock]),
	write('Suspect?'),
	read(Suspect),
	write('Room?'),
	read(Room),
	write('Weapon?'),
	read(Weapon),
	assert(suggestion(Player,Suspect,Room,Weapon)),
	write(Player),
	write(' suggested '),
	write(Suspect),
	write(' in the '),
	write(Room),
	write(' with the '),
	write(Weapon),
	write('.').