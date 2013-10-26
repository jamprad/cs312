import Data.List
import Debug.Trace

oshka_o6o7 :: [String] -> Char -> Int -> [String]
oshka_o6o7 start turn depth = []

--return all players' moves
--player must be moving in the "forward direction"
--
--for the other player (moving in the "reverse direction")
--	call: map reverse (playerMoves_o6o7 (reverse state) otherPlayer)
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
playerMoves_o6o7 state player = playerMoves_o6o7' state player []


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
	| (length rowA) < (length rowB) = playerRowAdvanceMoves_o6o7' (rowA:(init rowB):[]) player ([],[],[last rowB],[])
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
	| length rowA < length rowC = playerRowTakeMoves_o6o7' (rowA:(init rowB):(init2 rowC):[]) player ([],[],[],[],[last rowB],(last2 rowC)) --before the middle of the board
	| length rowA == length rowC = playerRowTakeMoves_o6o7' ((init rowA):rowB:(tail rowC):[]) player ([],[],[head rowC],[last rowA],[],[]) -- the middle of the board
	| otherwise = playerRowTakeMoves_o6o7' ((drop 2 rowA):(tail rowB):rowC:[]) player ((take 2 rowA),[head rowB],[],[],[],[]) -- after the middle of the board

--return 3 changed rows (rowA, rowB and rowC) for RIGHT take moves for player from rowA to rowC (taking the other player's piece from rowB)
-- helps playerRowTakeMoves_o6o7 make the right moves
playerRowRightTakeMoves_o6o7 :: [String] -> Char -> [[String]]
playerRowRightTakeMoves_o6o7 (rowA:rowB:rowC:[]) player
	| length rowA < length rowC = playerRowTakeMoves_o6o7' (rowA:(tail rowB):(drop 2 rowC):[]) player ([],[head rowB],(take 2 rowC),[],[],[]) -- before the middle of the board
	| length rowA == length rowC = playerRowTakeMoves_o6o7' ((tail rowA):rowB:(init rowC):[]) player ([head rowA],[],[],[],[],[last rowC]) -- the middle of the board
	| otherwise = playerRowTakeMoves_o6o7' ((init2 rowA):(init rowB):rowC:[]) player ([],[],[],(last2 rowA),[last rowB],[]) -- after the middle of the board

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
--		left moves : rowA, init rowB, init2 rowC is passed in the [String]; lBs == [last rowB], lCs == (last2 rowC) 
--		right moves : rowA, tail rowB, drop 2 rowC is passed in the [String]; fBs == [head rowB], fCs == (take 2 rowC)
--
-- when rowA is the same length as rowC (we are in the middle of the board):
--		left moves : tail rowA, rowB, init rowC is passed in the [String]; fAs == [head rowA], lCs == [last rowC]
--		right moves : init rowA, rowB, tail rowC is passed in the [String]; fCs == [head rowC], lAs == [last rowA] 
--		
-- when rowA is longer than rowC:
--		left moves : drop 2 rowA, tail rowB, rowC is passed in the [String]; fAs == [take 2 rowA], fBs == [head rowB]
--		right moves : init2 rowA, init rowB, rowC is passed in the [String], lAs == (last2 rowA), lBs == [last rowB]
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
	| a == player && b == (otherPlayer player) && c == '-' = ((fAs ++ (c:as) ++ lAs):(fBs ++ ('-':bs) ++ lBs):(fCs ++ (a:cs) ++ lCs):[]): --return this take move and
																playerRowTakeMoves_o6o7' (as:bs:cs:[]) player ((fAs ++ [a]),(fBs ++ [b]),(fCs ++ [c]),lAs,lBs,lCs) --next take moves
	| otherwise = playerRowTakeMoves_o6o7' (as:bs:cs:[]) player ((fAs ++ [a]),(fBs ++ [b]),(fCs ++ [c]),lAs,lBs,lCs) -- next take moves

--precondition: called on 'W' or 'B'
otherPlayer :: Char -> Char
otherPlayer 'w' = 'b'
otherPlayer 'b' = 'w'

--precondition: called on strings of length >= 2
last2 :: String -> String
last2 [a,b] = [a,b]
last2 (x:xs) = last2 xs

--precondition: called on strings of length >= 2
init2 :: String -> String
init2 [a,b] = []
init2 (x:xs) = x:init2 xs
