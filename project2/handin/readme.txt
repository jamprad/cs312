
========================================================================================================================
Julian Pradinuk           				  ????						    Tucker Buchy
44962090                   			  	 PROLOG							34091090
o6o7                       			  	  CLUE							    g9c7
jampradinuk@gmail.com      				  ????				           tuckerbuchy@gmail.com
========================================================================================================================

					           What the program does

1- Set up the game: how many players, the order of play, which cards you are holding
2- Make suggestions
3- Report cards shown
4- Read your detective journal (see the contents of the database)
5- Know when to make an accusation
6- Take advantage of what can be inferred from the suggestions of other players
7- Keeps track of the number of times a card has appeared in suggestions.
8- Recommends suggestions

========================================================================================================================

						  How to play/how it works

To play clue, simply load the program clue.pl into the Swipl prolog interpreter and start a game:
-Start a new swipl session.
-Type "[clue]."
-Type "clue."
-FOLLOW THE INSTRUCTIONS!


Players:
-There must be 3-6 players
-You are always player '1'. The player sitting left of you is player '2'. Player numbers increase around the board. This
is how we address different players throughout the user interface.


At each turn (whether its your turn or someone else's), your options are:
>>>>1 to read your detective journal
>>>>2 to make a suggestion by this turn's player
>>>>3 to report card shown
>>>>4 to have a suggestion recommended for you
>>>>5 to end this turn


1. Your detective journal looks like:

################DETECTIVE JOURNAL###############

	Suspects:
		colonelMustard (owner is player 1, 0/0 suggestions).
		missScarlet (0/0 suggestions).
		professorPlum (0/0 suggestions).
		mrGreen (0/0 suggestions).
		mrsWhite (0/0 suggestions).
		mrsPeacock (0/0 suggestions).
	Rooms:
		kitchen (owner is player 1, 0/0 suggestions).
		lounge (0/0 suggestions).
		study (0/0 suggestions).
		library (0/0 suggestions).
		diningRoom (0/0 suggestions).
		hall (0/0 suggestions).
		billiardRoom (0/0 suggestions).
		conservatory (0/0 suggestions).
		ballroom (0/0 suggestions).
	Weapons:
		rope (owner is player 1, 0/0 suggestions).
		leadPipe (0/0 suggestions).
		knife (0/0 suggestions).
		wrench (0/0 suggestions).
		candlestick (0/0 suggestions).
		pistol (0/0 suggestions).

##############################################
All of the cards are presented. If the card is one of your cards or you have been shown (and have reported) the owner is
reported. The history of the card in terms of how many times it has been suggested out of the total number of suggestions
is also shown. For example, "0/3 suggestions". This feature helps recommend suggestions for you (option 4).

When all but one suspect, room and weapon have been, you will be notified to make the accusation at the beginning of your
turn (or in the middle of your turn after having reported a card shown). This message looks like:
########################################################
Dude! Make an accusation! It was missScarlet in the lounge with the pistol.
########################################################


2. Make a suggestion by this turn's player:
If it's your turn, you just make the suggestion. 

If it was someone else's turn, you will also be asked to report the player who showed them a card. This takes advantage
of what can be inferred from the suggestions of others. For example: Suppose player 2 makes the suggestion "professorPlum
in the kitchen with the rope" and player 3 shows player 2 a card. Further suppose, as in the detective journal above, we 
know the owner of the cards kitchen and rope. We can infer that player 3 showed professorPlum to player 2! Your detective
 journal will be updated and you will see the message:
################################################
We were able to infer a card! Check your journal.
################################################


3. Report card shown:
Use this option whenever you see another player's card. The most common case for this is when player 1 makes a suggestion
and is shown a card that proves that suggestion is invalid. Your detective journal will be updated.


4. Have a suggestion recommended for you:
This option uses the number of times a card has been in suggestions. If many players tend to suggest the same card in
different instances, we can make a decent assumption about there being little knowledge of that card, hence giving it a
higher chance of being the solution to the game.


5. End this player:
Options 1-4 do not end the turn. You must explicitly end the turn by using this option.


*Note: when a game of clue ends, the Swipl environment will still have all of the predicates generated from that round
in the database. However, when a new game of clue is run, the predicates are cleared. Therefore, you do not need to 
restart the interpreter to play several games of clue.


**If for some unexpected reason game play is interrupted, you can probably type "play(Player).", where Player is
replaced with the number of the player whose turn it was, to return to the game.

========================================================================================================================
