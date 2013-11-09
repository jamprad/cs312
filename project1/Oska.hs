--Julian Pradinuk		+	Tucker Buchy
--44962090					34091090
--o6o7 						g9c7
--jampradinuk@gmail.com 	tuckerbuchy@gmail.com

import Data.List
import Data.Ord
import Debug.Trace

--ASSUMPTION ABOUT (start) BOARD DIRECTIONALITY:
-- White's (w's) "home" row is the first element in the list
-- Black's (b's) "home" row is the last element in the list
oska_o6o7 :: [String] -> Char -> Int -> [String]
oska_o6o7 start turn depth = case turn of
								'w' -> oska_o6o7' start turn depth
								'b' -> reverse (oska_o6o7' (reverse start) turn depth)

oska_o6o7' :: [String] -> Char -> Int -> [String]
oska_o6o7' start turn depth = move
	where (move, score) = minimax_o6o7 start turn depth 0

--minimax algorithm:
-- 1. generate game tree to depth levels
-- 2. top level (level 0) is MAX, next is MIN...
-- 3. apply evaluation function to all the terminal (leaf) states/boards to get "goodness" values
-- 4. if the parent is at a MIN level, take the minimum value of its children
--		if the parent is at a MAX level, then the value is the maximum of the values of its children
-- 5. propagate values as in step 4 until the MAX at the top chooses its move
--
--	eg. for depth 4
--		level 	0	MAX
--						(if isEndOfGame then endOfGameScore else MAX player's moves...)
--		level 	1 	MIN
--						(if isEndOfGame then endOfGameScore else MIN otherPlayer's moves...)
--		level 	2   MAX
--						(if isEndOfGame then endOfGameScore else MAX player's moves...)
--      level 	3	MIN
--						(if isEndOfGame then endOfGameScore else MIN otherPlayer's moves...)
--		level 	4
--						evaluateBoard
--
--
--	minimax_o6o7 (board,boardScore) player depth level
--		returns (nextMove,score)
--			where 	nextMove = player's next best move,
--					score = minimax score
--				by applying the minimax algorithm to depth levels
--
minimax_o6o7 :: [String] -> Char -> Int -> Int -> ([String], Int)
minimax_o6o7 board player depth level
	| trace ("level " ++ show level ++ ": board = " ++ show board) False = undefined --see http://www.haskell.org/haskellwiki/Debugging
	| isEndOfGame = do trace ("\tendOfGameScore = " ++ show endOfGameScore) $
						(board, endOfGameScore) --leaf because this board is an end game board (assigns correct endOfGameScore if we are at depth, too)
	| depth == level = do trace ("\tevaluateBoardScore = " ++ show evaluateBoard) $ 
						(board, evaluateBoard) --leaf because we are at depth
	| otherwise = do trace ("resultScore = " ++ show (resultScore) ++ " (level " ++ show level ++ ") resultBoard = " ++ show (resultBoard)) $
						(resultBoard, resultScore)
						--case level of
						--	0 -> (resultBoard, resultScore)
						--	_ -> (board, resultScore)
	where 
		(isEndOfGame, endOfGameScore) = endOfGame_o6o7 board player;
		evaluateBoard = evaluateBoard_o6o7 board player;
		(resultBoard, resultScore) = case (level `rem` 2) of
						0 -> maximumBy (comparing score_o6o7)
								[minimax_o6o7 move player depth (level+1) | move <- playerMoves_o6o7 board player] --player's turn
						1 -> minimumBy (comparing score_o6o7)
								[minimax_o6o7 move player depth (level+1) | move <- map reverse (playerMoves_o6o7 (reverse board) (otherPlayer_o6o7 player))] --otherPlayer's turn

--
-- endOfGame_o6o7 board player
--		returns (isEndOfGame, endOfGameScore)
--			isEndOfGame == True:
--				-player has pieces left and otherPlayer doesn't: endOfGameScore == maxScore
--				-otherPlayer has pieces left and player doesn't: endOfGameScore == -maxScore
--				-standstill (neither player can move): 
--											-player has more pieces: endOfGameScore == maxScore
--											-players have same number of pieces: endOfGameScore == 0
--											-otherPlayer has more pieces: endOfGameScore == -maxScore
--				-both players pieces are at end:
--											-player has more pieces: endOfGameScore == maxScore
--											-players have same number of pieces: endOfGameScore == 0
--											-otherPlayer has more pieces: endOfGameScore == -maxScore
--				-player has all pieces at end: endOfGameScore == maxScore
--				-otherPlayer has all pieces at end: endOfGameScore == -maxScore
--			isEndOfGame == False: endOfGameScore == 0
--
endOfGame_o6o7 :: [String] -> Char -> (Bool, Int)
endOfGame_o6o7 board player
	| otherPlayerNPieces == 0 = (True, maxScore) --player wins, test: ["---","bb","---"] 'b'
	| playerNPieces == 0 = (True, (-maxScore)) --otherPlayer wins, test: ["---","bb","---"] 'w'
	| head (playerMoves_o6o7 board player) == board
		&& head (playerMoves_o6o7 (reverse board) (otherPlayer_o6o7 player)) == reverse board = --do trace ("standstill") $
														case (playerNPieces - otherPlayerNPieces) of
															x | x > 0 -> (True, maxScore) --player wins, test: ["www","ww","bbb"] 'w'
															0 -> (True, 0) --tie game, test: ["www","wb","bbb"] 'w'
															y | y < 0 -> (True, (-maxScore)) --otherPlayer wins, test: ["www","ww","bbb"] 'b'
	| playerAllPiecesAtEnd && otherPlayerAllPiecesAtEnd = case (playerNPiecesAtEnd - otherPlayerNPiecesAtEnd) of
															x | x > 0 -> (True, maxScore) --player wins, test: ["b--","--","-ww"] 'w'
															0 -> (True, 0) --tie game, test: ["b--","--","w--"] 'w'
															y | y < 0 -> (True, (-maxScore)) --otherPlayer wins, test: ["bb-","--","w--"] 'w'
	| playerAllPiecesAtEnd = (True, maxScore) --player wins, test: ["-b-","-b","-w-"] 'w'
	| otherPlayerAllPiecesAtEnd = (True, (-maxScore)) --otherPlayer wins, test: ["-b-","-w","-w-"] 'w'
	| otherwise = (False, 0) --game isn't over yet, test: ["bbb","-w","-w-"] 'b'
							 --						test: ["bbb","ww","---"] 'b'
							 --						test: ["---","ww","bbb"] 'w'
	where
		maxScore = maxScore_o6o7 (length board) 
		playerNPieces = playerNPieces_o6o7 board player;
		otherPlayerNPieces = playerNPieces_o6o7 board (otherPlayer_o6o7 player);
		playerAllPiecesAtEnd = playerAllPiecesAtEnd_o6o7 board player;
		otherPlayerAllPiecesAtEnd = playerAllPiecesAtEnd_o6o7 (reverse board) (otherPlayer_o6o7 player);
		playerNPiecesAtEnd = playerNPiecesRow_o6o7 (last board) player;
		otherPlayerNPiecesAtEnd = playerNPiecesRow_o6o7 (head board) (otherPlayer_o6o7 player)

--our max score is defined as the square of n
maxScore_o6o7 :: Int -> Int
maxScore_o6o7 n = n^2

--evaluateBoard_o6o7 board player
-- (a high score is good)
--
-- scoring method:
--		o player's pieces on the ith row contribute i to the score
--		o other player's pieces on the ith row contribue -(n - i + 1) to the score=
--
evaluateBoard_o6o7 :: [String] -> Char -> Int
evaluateBoard_o6o7 board player = evaluateBoard_o6o7' board player (length board) 1

evaluateBoard_o6o7' :: [String] -> Char -> Int -> Int -> Int
evaluateBoard_o6o7' [] _ _ _= 0
evaluateBoard_o6o7' (x:xs) player n i = (evaluateRow_o6o7 x player n i) + (evaluateBoard_o6o7' xs player n (i+1))

--evaluateRow_o6o7 row player n i
-- (a high score is good)
evaluateRow_o6o7 :: String -> Char -> Int -> Int -> Int
evaluateRow_o6o7 [] _ _ _ = 0
evaluateRow_o6o7 (x:xs) player n i 
	| x == player = i + evaluateRow_o6o7 xs player n i
	| x == otherPlayer_o6o7 player = (-(n - i + 1)) + evaluateRow_o6o7 xs player n i
	| otherwise = evaluateRow_o6o7 xs player n i


-- playerNPieces_o6o7 board player
--	returns the number of pieces remaining for player on board
playerNPieces_o6o7 :: [String] -> Char -> Int
playerNPieces_o6o7 [] _ = 0
playerNPieces_o6o7 (x:xs) player = playerNPiecesRow_o6o7 x player + playerNPieces_o6o7 xs player

-- playerNPiecesRow_o6o7 row player
--	returns the number of player's pieces in row
playerNPiecesRow_o6o7 :: String -> Char -> Int
playerNPiecesRow_o6o7 [] _ = 0
playerNPiecesRow_o6o7 (x:xs) player
	| x == player = 1 + playerNPiecesRow_o6o7 xs player
	| otherwise = playerNPiecesRow_o6o7 xs player

--return True if all player's pieces are at goal row
playerAllPiecesAtEnd_o6o7 :: [String] -> Char -> Bool
playerAllPiecesAtEnd_o6o7 board player = playerNPieces_o6o7 board player == playerNPiecesRow_o6o7 (last board) player


--return all players' moves
--player must be moving in the "forward direction"
--
--for the other player (moving in the "reverse direction")
--	call: map reverse (playerMoves_o6o7 (reverse board) otherPlayer)
--
--tests:
-- playerMoves_o6o7 ["wwww","---","--","---","bbbb"] 'w' 
--		returns [["-www","w--","--","---","bbbb"],["w-ww","-w-","--","---","bbbb"],["ww-w","--w","--","---","bbbb"],["w-ww","w--","--","---","bbbb"],["ww-w","-w-","--","---","bbbb"],["www-","--w","--","---","bbbb"]]
-- map reverse (playerMoves_o6o7 (reverse ["wwww","---","--","---","bbbb"]) 'b')
--		returns [["wwww","---","--","b--","-bbb"],["wwww","---","--","-b-","b-bb"],["wwww","---","--","--b","bb-b"],["wwww","---","--","b--","b-bb"],["wwww","---","--","-b-","bb-b"],["wwww","---","--","--b","bbb-"]]
-- playerMoves_o6o7 ["wwww","w-w","bb","---","bbbb"] 'w' 
--		returns [["w-ww","www","bb","---","bbbb"],["ww-w","www","bb","---","bbbb"],["wwww","w--","b-","-w-","bbbb"],["wwww","--w","-b","-w-","bbbb"]]
-- playerMoves_o6o7 ["wwww","w-w","bb","w-w","-bb-"] 'w' 
--		returns [["w-ww","www","bb","w-w","-bb-"],["ww-w","www","bb","w-w","-bb-"],["wwww","w--","b-","www","-bb-"],["wwww","--w","-b","www","-bb-"],["wwww","w-w","bb","w--","-bbw"],["wwww","w-w","bb","--w-","wbb"]]
playerMoves_o6o7 :: [String] -> Char -> [[String]]
playerMoves_o6o7 board player 
	| null result = [board] --player loses a turn
	| otherwise = result
	where
		result = playerMoves_o6o7' board player []


--helps playerMoves_o6o7 return all moves for player, considering 1 row at a time
playerMoves_o6o7' :: [String] -> Char -> [String] -> [[String]]
playerMoves_o6o7' (a:[]) _ _ = [] --no moves possible with just 1 row
playerMoves_o6o7' (a:b:[]) player visitedRows = [visitedRows ++ move | 
													move <- playerRowAdvanceMoves_o6o7 (a:b:[]) player] --no take moves possible with just 2 rows
playerMoves_o6o7' (a:b:c:xs) player visitedRows = concat [ [visitedRows ++ move ++ c:xs | move <- playerRowAdvanceMoves_o6o7 (a:b:[]) player], --advance moves from row a
															[visitedRows ++ move ++ xs | move <- playerRowTakeMoves_o6o7 (a:b:c:[]) player]] --take moves from row a
																++ playerMoves_o6o7' (b:c:xs) player (visitedRows ++ a:[]) --moves from row b (recursive call)

--return 2 changed rows (rowA and rowB) for advance moves for player from rowA to rowB
playerRowAdvanceMoves_o6o7 :: [String] -> Char -> [[String]]
playerRowAdvanceMoves_o6o7 rowsAB player = --trace ("playerRowAdvanceMoves_o6o7 " ++ show rowsAB ++ " " ++ show player)
												(concat [(playerRowRightAdvanceMoves_o6o7 rowsAB player),
													(playerRowLeftAdvanceMoves_o6o7 rowsAB player)])

--return 2 changed rows (rowA and rowB) for LEFT advance moves for player from rowA to rowB
playerRowLeftAdvanceMoves_o6o7 :: [String] -> Char -> [[String]]
playerRowLeftAdvanceMoves_o6o7 (rowA:rowB:[]) player
	| (length rowA) < (length rowB) = playerRowAdvanceMoves_o6o7' (rowA:(init rowB):[]) player ([],[],[],[last rowB])
	| otherwise = playerRowAdvanceMoves_o6o7' ((tail rowA):rowB:[]) player ([head rowA],[],[],[])

--return 2 changed rows (rowA and rowB) for RIGHT advance moves for player from rowA to rowB
playerRowRightAdvanceMoves_o6o7 :: [String] -> Char -> [[String]]
playerRowRightAdvanceMoves_o6o7 (rowA:rowB:[]) player
	| length rowA < length rowB = playerRowAdvanceMoves_o6o7' (rowA:(tail rowB):[]) player ([],[head rowB],[],[])
	| otherwise = playerRowAdvanceMoves_o6o7' ((init rowA):rowB:[]) player ([],[],[last rowA],[])

--
-- 	helps playerRowLeftAdvanceMoves_o6o7 and playerRowRightAdvanceMoves_o6o7 
--  return advance moves from rowA to rowB for player
--
--
--	in the tuple (fAs,fBs,lAs,lBs):
--				fAs = first part of rowA
--				fBs = first part of rowB
--				lAs = last part of rowA
--				lBs = last part of rowB
--
-- calls:
--			(default to tuple values = [])
--
-- when rowA is shorter than rowB: 
--		left moves : rowA, init rowB is passed in the [String], lBs == [last rowB] 
--		right moves : rowA, tail rowB is passed in the [String], fBs == [head rowB]
--		
-- when rowA is longer than rowB:
--		left moves : tail rowA, rowB is passed in the [String], fAs == [head rowA]
--		right moves : init rowA, rowB is passed in the [String], lAs == [last rowA]
--
--tests:
-- playerRowAdvanceMoves_o6o7' ["WB","--"] 'W' ([],[],[],[]) returns [["-B","W-"]]
-- playerRowAdvanceMoves_o6o7' ["WBWW","--W-"] 'W' ([],[],[],[]) returns [["-BWW","W-W-"],["WBW-","--WW"]]
--	
playerRowAdvanceMoves_o6o7' :: [String] -> Char -> (String,String,String,String) -> [[String]]
playerRowAdvanceMoves_o6o7' ([]:[]:[]) player (fAs,fBs,lAs,lBs) = []
playerRowAdvanceMoves_o6o7' ((a:as):(b:bs):[]) player (fAs,fBs,lAs,lBs)
	| a == player && b == '-' = ((fAs ++ (b:as) ++ lAs):(fBs ++ (a:bs) ++ lBs):[]):
										playerRowAdvanceMoves_o6o7' (as:bs:[]) player ((fAs ++ [a]),(fBs ++ [b]),lAs,lBs)
	| otherwise = playerRowAdvanceMoves_o6o7' (as:bs:[]) player ((fAs ++ [a]),(fBs ++ [b]),lAs,lBs)


--return 3 changed rows (rowA, rowB and rowC) for take moves for player from rowA to rowC (taking the other player's piece from rowB)
playerRowTakeMoves_o6o7 :: [String] -> Char -> [[String]]
playerRowTakeMoves_o6o7 rowsABC player = --trace ("playerRowTakeMoves_o6o7 " ++ show rowsABC ++ " " ++ show player)
											concat [(playerRowRightTakeMoves_o6o7 rowsABC player), 
													(playerRowLeftTakeMoves_o6o7 rowsABC player)]


--return 3 changed rows (rowA, rowB and rowC) for RIGHT take moves for player from rowA to rowC (taking the other player's piece from rowB)
-- helps playerRowTakeMoves_o6o7 make the left moves
playerRowLeftTakeMoves_o6o7 :: [String] -> Char -> [[String]]
playerRowLeftTakeMoves_o6o7 (rowA:rowB:rowC:[]) player
	| length rowA < length rowC = playerRowTakeMoves_o6o7' (rowA:(init rowB):(init2_o6o7 rowC):[]) player ([],[],[],[],[last rowB],(last2_o6o7 rowC)) --before the middle of the board
	| length rowA == length rowC = playerRowTakeMoves_o6o7' ((init rowA):rowB:(tail rowC):[]) player ([],[],[head rowC],[last rowA],[],[]) -- the middle of the board
	| otherwise = playerRowTakeMoves_o6o7' ((drop 2 rowA):(tail rowB):rowC:[]) player ((take 2 rowA),[head rowB],[],[],[],[]) -- after the middle of the board

--return 3 changed rows (rowA, rowB and rowC) for RIGHT take moves for player from rowA to rowC (taking the other player's piece from rowB)
-- helps playerRowTakeMoves_o6o7 make the right moves
playerRowRightTakeMoves_o6o7 :: [String] -> Char -> [[String]]
playerRowRightTakeMoves_o6o7 (rowA:rowB:rowC:[]) player
	| length rowA < length rowC = playerRowTakeMoves_o6o7' (rowA:(tail rowB):(drop 2 rowC):[]) player ([],[head rowB],(take 2 rowC),[],[],[]) -- before the middle of the board
	| length rowA == length rowC = playerRowTakeMoves_o6o7' ((tail rowA):rowB:(init rowC):[]) player ([head rowA],[],[],[],[],[last rowC]) -- the middle of the board
	| otherwise = playerRowTakeMoves_o6o7' ((init2_o6o7 rowA):(init rowB):rowC:[]) player ([],[],[],(last2_o6o7 rowA),[last rowB],[]) -- after the middle of the board

--
-- 	helps playerRowLeftTakeMoves_o6o7 and playerRowRightTakeMoves_o6o7 
--  return take moves for player from rowA to rowC (taking the other player's piece from rowB)
--
--
--	in the tuple (fAs,fBs,lAs,lBs):
--				fAs = first part of rowA
--				fBs = first part of rowB
--				fCs = first part of rowC
--				lAs = last part of rowA
--				lBs = last part of rowB
--				lCs = last part of rowC
--
-- calls:
--			(in this comment, default tuple member values == [])
--
-- when rowA is shorter than rowC: 
--		left moves : rowA, init rowB, init2_o6o7 rowC is passed in the [String]; lBs == [last rowB], lCs == (last2_o6o7 rowC) 
--		right moves : rowA, tail rowB, drop 2 rowC is passed in the [String]; fBs == [head rowB], fCs == (take 2 rowC)
--
-- when rowA is the same length as rowC (we are in the middle of the board):
--		left moves : tail rowA, rowB, init rowC is passed in the [String]; fAs == [head rowA], lCs == [last rowC]
--		right moves : init rowA, rowB, tail rowC is passed in the [String]; fCs == [head rowC], lAs == [last rowA] 
--		
-- when rowA is longer than rowC:
--		left moves : drop 2 rowA, tail rowB, rowC is passed in the [String]; fAs == [take 2 rowA], fBs == [head rowB]
--		right moves : init2_o6o7 rowA, init rowB, rowC is passed in the [String], lAs == (last2_o6o7 rowA), lBs == [last rowB]
--	
--
--tests: 
--playerRowTakeMoves_o6o7' ["w","b","-"] 'w' ([],[],[],[],[],[]) returns [["-","-","W"]]
--playerRowTakeMoves_o6o7' ["wbw","bww","---"] 'w' ([],[],[],[],[],[]) returns [["-BW","-WW","W--"]]
--playerRowTakeMoves_o6o7' ["wbw","bwb","---"] 'w' ([],[],[],[],[],[]) returns [["-BW","-WB","W--"],["WB-","BW-","--W"]]
--
playerRowTakeMoves_o6o7' :: [String] -> Char -> (String,String,String,String,String,String) -> [[String]]
playerRowTakeMoves_o6o7' ([]:[]:[]:[]) player (fAs,fBs,fCs,lAs,lBs,lCs) = []
playerRowTakeMoves_o6o7' ((a:as):(b:bs):(c:cs):[]) player (fAs,fBs,fCs,lAs,lBs,lCs)
	| a == player && b == (otherPlayer_o6o7 player) && c == '-' = ((fAs ++ (c:as) ++ lAs):(fBs ++ ('-':bs) ++ lBs):(fCs ++ (a:cs) ++ lCs):[]): --return this take move and
																playerRowTakeMoves_o6o7' (as:bs:cs:[]) player ((fAs ++ [a]),(fBs ++ [b]),(fCs ++ [c]),lAs,lBs,lCs) --next take moves
	| otherwise = playerRowTakeMoves_o6o7' (as:bs:cs:[]) player ((fAs ++ [a]),(fBs ++ [b]),(fCs ++ [c]),lAs,lBs,lCs) -- next take moves

--precondition: called on 'w' or 'b'
otherPlayer_o6o7 :: Char -> Char
otherPlayer_o6o7 'w' = 'b'
otherPlayer_o6o7 'b' = 'w'

--precondition: called on (board, score) tuples
score_o6o7 :: ([String],Int) -> Int
score_o6o7 (_, score) = score

--precondition: called on strings of length >= 2
last2_o6o7 :: String -> String
last2_o6o7 [a,b] = [a,b]
last2_o6o7 (x:xs) = last2_o6o7 xs

--precondition: called on strings of length >= 2
init2_o6o7 :: String -> String
init2_o6o7 [a,b] = []
init2_o6o7 (x:xs) = x:init2_o6o7 xs
