rush_hour :: [String] -> [[String]]
rush_hour initial = []

carMoveLeft :: String -> String
carMoveLeft ('-':b:c:xs) = (b:c:'-':xs)

carMoveRight :: String -> String
carMoveRight (a:b:'-':xs) = ('-':a:b:xs)

truckMoveLeft :: String -> String
truckMoveLeft ('-':b:c:d:xs) = (b:c:d:'-':xs)

truckMoveRight :: String -> String
truckMoveRight (a:b:c:'-':xs) = ('-':a:b:c:xs)

carMoveDown :: [String] -> [String]
carMoveDown ((a:as):(b:bs):('-':cs):xs) = (('-':as):(a:bs):(b:cs):xs)

carMoveUp :: [String] -> [String]
carMoveUp (('-':as):(b:bs):(c:cs):xs) = ((b:as):(c:bs):('-':cs):xs)

truckMoveDown :: [String] -> [String]
truckMoveDown ((a:as):(b:bs):(c:cs):('-':ds):xs) = (('-':as):(a:bs):(b:cs):(c:ds):xs)

truckMoveUp :: [String] -> [String]
truckMoveUp (('-':as):(b:bs):(c:cs):(d:ds):xs) = ((b:as):(c:bs):(d:cs):('-':ds):xs)


--return all states achieved by moving one vehicle one space, considering 1 column
--tests:
-- ["A","A","A","-","B","B"] [] should return [["-","A","A","A","B","B"],["A","A","A","B","B","-"]]
-- ["-","B","B","-","-"] [] should return [["B","B","-","-","-"],["-","-","B","B","-"]]
-- ["-","C","C","C","-"] [] should return [["C","C","C","-","-"],["-","-","C","C","C"]]
verticalMoves :: [String] -> [String] -> [[String]]
verticalMoves [] _ = []
verticalMoves (a:[]) _ = []
verticalMoves (a:b:[]) _ = []
verticalMoves ((a:as):(b:bs):(c:cs):[]) visitedRows
	| a == '-' && b /= '-' && b == c = [(visitedRows ++ carMoveUp ((a:as):(b:bs):(c:cs):[]))] --add car up state, we're done
	| a /= '-' && a == b && c == '-' = [(visitedRows ++ carMoveDown ((a:as):(b:bs):(c:cs):[]))] --add car down state, we're done
	| otherwise = []
verticalMoves ((a:as):(b:bs):(c:cs):(d:ds):xs) visitedRows
	| a == '-' && b /= '-' && b == c && c == d = (visitedRows ++ truckMoveUp ((a:as):(b:bs):(c:cs):(d:ds):xs)): --add truck up state
													verticalMoves ((b:bs):(c:cs):(d:ds):xs) (visitedRows ++ (a:as):[]) --advance one row
	| a /= '-' && a == b && b == c && d == '-' = (visitedRows ++ truckMoveDown ((a:as):(b:bs):(c:cs):(d:ds):xs)): --add truck down state
													verticalMoves ((d:ds):xs) (visitedRows ++ (a:as):(b:bs):(c:cs):[]) --advance three rows
	| a == '-' && b /= '-' && b == c = (visitedRows ++ carMoveUp ((a:as):(b:bs):(c:cs):(d:ds):xs)): --add car up state
											verticalMoves ((b:bs):(c:cs):(d:ds):xs) (visitedRows ++ (a:as):[]) --advance one row
	| a /= '-' && a == b && c == '-' = (visitedRows ++ carMoveDown ((a:as):(b:bs):(c:cs):(d:ds):xs)): --add car down state
											verticalMoves ((c:cs):(d:ds):xs) (visitedRows ++ (a:as):(b:bs):[]) --advance two rows
	| otherwise = verticalMoves ((b:bs):(c:cs):(d:ds):xs) (visitedRows ++ (a:as):[]) --advance one row

--return all states achieved by moving one vehicle one space, considering 1 row
--tests:
-- "AAA-BB" "" should return ["-AAABB","AAABB-"]
-- "-BB--" "" should return ["BB---","--BB-"]
-- "-CCC-" "" should return ["CCC--","--CCC"]
horizontalMoves :: String -> String -> [String]
horizontalMoves [] _ = [] 
horizontalMoves (a:[]) _ = []
horizontalMoves (a:b:[]) _ = []
horizontalMoves (a:b:c:[]) visitedCols
	| a == '-' && b /= '-' && b == c = [visitedCols ++ carMoveLeft (a:b:c:[])] --add car left state, we're done
	| a /= '-' && a == b && c == '-' = [visitedCols ++ carMoveRight (a:b:c:[])]--add car right state, we're done
	| otherwise = []
horizontalMoves (a:b:c:d:xs) visitedCols
	| a == '-' && b /= '-' && b == c && c == d = (visitedCols ++ truckMoveLeft (a:b:c:d:xs)): -- add truck left state
													horizontalMoves (b:c:d:xs) (visitedCols ++ a:[]) --advance one col
	| a /= '-' && a == b && b == c && d == '-' = (visitedCols ++ truckMoveRight (a:b:c:d:xs)): -- add truck right state
													horizontalMoves (d:xs) (visitedCols ++ a:b:c:[]) --advance three cols
	| a == '-' && b /= '-' && b == c = (visitedCols ++ carMoveLeft (a:b:c:d:xs)): -- add car left state
													horizontalMoves (b:c:d:xs) (visitedCols ++ a:[]) --advance one col
	| a /= '-' && a == b && c == '-' = (visitedCols ++ carMoveRight (a:b:c:d:xs)): -- add car right state
													horizontalMoves (c:d:xs) (visitedCols ++ a:b:[])--advance two cols
	| otherwise = horizontalMoves (b:c:d:xs) (visitedCols ++ a:[]) --advance one col


