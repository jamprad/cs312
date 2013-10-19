rush_hour :: [String] -> [[String]]
rush_hour initial = []

carMoveLeft :: String -> String
carMoveLeft ('-':b:c:xs)
	| b == c = (b:c:'-':xs)
	| otherwise = ('-':b:c:xs)

carMoveRight :: String -> String
carMoveRight (a:b:'-':xs)
	| a == b = ('-':a:b:xs)
	| otherwise = (a:b:'-':xs)

truckMoveLeft :: String -> String
truckMoveLeft ('-':b:c:d:xs)
	| (b == c) and (c == d) = (b:c:d:'-':xs)
	| otherwise = ('-':b:c:d:xs)

truckMoveRight :: String -> String
truckMoveRight (a:b:c:'-':xs)
	| (a == b) and (b == c) = ('-':a:b:c:xs)
	| otherwise = (a:b:c:'-':xs)

carMoveDown :: [String] -> [String]
carMoveDown ((a:as):(b:bs):('-':cs):xs)
	| a == b = (('-':as):(a:bs):(b:cs):xs)
	| otherwise = ((a:as):(b:bs):('-':cs):xs)

carMoveUp :: [String] -> [String]
carMoveUp (('-':as):(b:bs):(c:cs):xs)
	| b == c = ((b:as):(c:bs):('-':cs):xs)
	| otherwise = (('-':as):(b:bs):(c:cs):xs)

truckMoveDown :: [String] -> [String]
truckMoveDown ((a:as):(b:bs):(c:cs):('-':ds):xs)
	| (a == b) and (b == c) = (('-':as):(a:bs):(b:cs):(c:ds):xs)
	| otherwise = ((a:as):(b:bs):(c:cs):('-':ds):xs)

truckMoveUp :: [String] -> [String]
truckMoveUp (('-':as):(b:bs):(c:cs):(d:ds):xs)
	| (b == c) and (c == d) = ((b:as):(c:bs):(d:cs):('-':ds):xs)
	| otherwise = (('-':as):(b:bs):(c:cs):(d:ds):xs)