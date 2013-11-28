
=============================CLUE=============================
Julian Pradinuk           			Tucker Buchy
44962090                   			  34091090
o6o7                       			  g9c7
jampradinuk@gmail.com      			tuckerbuchy@gmail.com
==============================================================
What the program does:
1- Set up the game: how many players, the order of
play, which cards you are holding
2- Make suggestions
3- Report cards shown
4- Read your detective journal (see the contents of the 
database)
5- Know when to make an accusation
6- Take advantage of what can be inferred from the suggestions
of other players
7- Keeps track of the percentage of times a card has appeared 
in suggestions.

How to use:

To play clue, simply load the program clue.pl into the Swipl prolog interpreter with [clue].

To begin a game, call the clue. predicate. Instructions will be shown in the game. 

One thing to note is that the user of our program is always known as player '1'. The left of him is player '2'. This is how we address different players throughout the user interface.

Note that when a game of clue ends, the Swipl environment will still have all of the predicates generated from that round in the database. However, when a new game of clue is run, the predicates are cleared.
Therefore, you do not need to restart the interpreted to play several games of clue. 

Feature Descriptions:

Reading detective journal:
	Reading the detective journal is the same thing as you would have in a real game of clue, except the card owner ship is kept track of for you. When it is known that a player has a card, there will be a message next to that card's name showing who owns the card.
	Also, there is a feature added for displaying the history of a card in terms of how many times it has been suggested out of the total number of suggestions. For example, a card may say "seen in 3 of 6 suggestions.". This feature was implemented with the intent
	of showing information as to how likely the card is to be in the envelope. If many players tend to suggest the same card in different instances, we can make a decent assumption about there being little knowledge of that card, hence giving it a higher probabilty
	of being the solution to the game.

Make suggestion:
	One can make a suggestion by providing a Suspect, Weapon, and Room card. Suggestions can and should be made regardless if it is player '1's turn. This is because there is information we can capture from other player's suggestions. For example, there is a interencing
	feature that will infer from a suggestion who has a card. This will happen in the case that 2 of the 3 suggested cards are known to be owned by some players. When a given players reveals a card to the suggesting player, we can infer that the revealed card is the remaining
	card that is unknown. This will be automatically updated in the database and we give user notification to tell them.

Report a card:
	One can also report cards upon discovering who holds them. The most common case for this is when player 1 makes a suggestion and is revealed a card that proves that suggestion is invalid. This will also update the database.

Accusation Prompt:
	When there is only one possibility of what 3 cards could be a solution to the clue game, the user will get a notification telling them to make a accusation. 

