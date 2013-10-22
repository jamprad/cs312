import Data.List

carMoveLeft :: String -> String
carMoveLeft ('-':b:c:xs) = (b:c:'-':xs)

carMoveRight :: String -> String
carMoveRight (a:b:'-':xs) = ('-':a:b:xs)

truckMoveLeft :: String -> String
truckMoveLeft ('-':b:c:d:xs) = (b:c:d:'-':xs)

truckMoveRight :: String -> String
truckMoveRight (a:b:c:'-':xs) = ('-':a:b:c:xs)

--vertical Moves state returns all states achieved by moving one vehicle one space vertically
--tests:
-- ["A-C","ABC","AB-","--D","--D"] should return:
-- [["--C","ABC","AB-","A-D","--D"],
-- ["ABC","ABC","A--","--D","--D"],
-- ["A-C","A-C","AB-","-BD","--D"],
-- ["A--","ABC","ABC","--D","--D"],
-- ["A-C","ABC,"ABD","--D","--D"]]
verticalMoves :: [String] -> [[String]]
verticalMoves state = map transpose (horizontalMoves (transpose state))

-- horizontalMoves state returns all states achieved by moving one vehicle one space horizontally
--tests:
-- ["AAA-BB","-BB---",""-CCC--""] should return:
-- [["-AAABB",...],
-- ["AAABB-",...],
-- [...,"BB---",...],
-- [...,"--BB--",...],
-- [...,"CCC--"],
-- [...,"CCC--"]]
horizontalMoves :: [String] -> [[String]]
horizontalMoves state = horizontalMoves' state []

horizontalMoves' :: [String] -> [String] -> [[String]] 
horizontalMoves' [] _ = []
horizontalMoves' (x:xs) visitedRows = [visitedRows ++ (move:xs) | move <- horizontalRowMoves x []] ++ horizontalMoves' xs (visitedRows ++ x:[])

--return all states achieved by moving one vehicle one space horizontally, considering 1 row
--tests:
-- "AAA-BB" "" should return ["-AAABB","AAABB-"]
-- "-BB---" "" should return ["BB---","--BB--"]
-- "-CCC--" "" should return ["CCC--","--CCC-"]
horizontalRowMoves :: String -> String -> [String]
horizontalRowMoves [] _ = [] 
horizontalRowMoves (a:[]) _ = []
horizontalRowMoves (a:b:[]) _ = []
horizontalRowMoves (a:b:c:[]) visitedCols
	| a == '-' && b /= '-' && b == c = [visitedCols ++ carMoveLeft (a:b:c:[])] --add car left state, we're done
	| a /= '-' && a == b && c == '-' = [visitedCols ++ carMoveRight (a:b:c:[])]--add car right state, we're done
	| otherwise = []
horizontalRowMoves (a:b:c:d:xs) visitedCols
	| a == '-' && b /= '-' && b == c && c == d = (visitedCols ++ truckMoveLeft (a:b:c:d:xs)): -- add truck left state
													horizontalRowMoves (b:c:d:xs) (visitedCols ++ a:[]) --advance one col
	| a /= '-' && a == b && b == c && d == '-' = (visitedCols ++ truckMoveRight (a:b:c:d:xs)): -- add truck right state
													horizontalRowMoves (d:xs) (visitedCols ++ a:b:c:[]) --advance three cols
	| a == '-' && b /= '-' && b == c = (visitedCols ++ carMoveLeft (a:b:c:d:xs)): -- add car left state
													horizontalRowMoves (b:c:d:xs) (visitedCols ++ a:[]) --advance one col
	| a /= '-' && a == b && c == '-' = (visitedCols ++ carMoveRight (a:b:c:d:xs)): -- add car right state
													horizontalRowMoves (c:d:xs) (visitedCols ++ a:b:[])--advance two cols
	| otherwise = horizontalRowMoves (b:c:d:xs) (visitedCols ++ a:[]) --advance one col
