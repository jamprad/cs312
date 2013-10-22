import Data.List

-- start is the starting state
rushhour start = printStrMatrix (reverse (statesearch [start] []))
  
-- this searches all possible states for the goal state.
statesearch :: [[String]] -> [[String]] -> [[String]]
statesearch unexplored path
	--we've searched everywhere, no solution
	| null unexplored 				           = []
  -- just check if the thing we are looking at has been looked at already.
  | elem (head unexplored) path        = statesearch (tail unexplored) path
	| isgoalstate (head unexplored)      = (head unexplored):path
	| (not (null result)) 			         = result
	| otherwise						               = statesearch (tail unexplored) path
                                          where result = statesearch (generateNewStates (head unexplored))
                                                                      ((head unexplored):path)
printStrMatrix :: [[String]] -> IO ()
printStrMatrix [] = printStrList []
printStrMatrix (x:xs) = 
                  do 
                    printStrList x
                    printStrMatrix xs

printStrList :: [String] -> IO()
printStrList [] = putStrLn "" 
printStrList (x:xs) = 
                  do
                    putStrLn x
                    printStrList xs

-- checks whether we've reached a goal state (3rd row has XX is the last two columns)
isgoalstate :: [String] -> Bool
-- if the x car is in the last 2 places, return true.
isgoalstate candidate = ((drop 4 (candidate!!2)) == "XX")

generateNewStates currState = concat [generateNewRightCarStates currState,
                                        generateNewLeftCarStates currState,
                                        generateNewRightTruckStates currState,
                                        generateNewLeftTruckStates currState,
                                        generateNewUpTruckStates currState,
                                        generateNewUpCarStates currState, 
                                        generateNewDownCarStates currState, 
                                        generateNewDownTruckStates currState]

generateNewRightCarStates currState = generateNewStatesHelper currState 0 0 2 isCarMovableToRight moveCarRight
generateNewLeftCarStates currState = generateNewStatesHelper currState 0 0 2 isCarMovableToLeft moveCarLeft
generateNewRightTruckStates currState = generateNewStatesHelper currState 0 0 3 isTruckMovableToRight moveTruckRight
generateNewLeftTruckStates currState = generateNewStatesHelper currState 0 0 3 isTruckMovableToLeft moveTruckLeft

generateNewDownCarStates currState = map transpose (generateNewStatesHelper (transpose currState) 0 0 2 isCarMovableToRight moveCarRight)
generateNewUpCarStates currState = map transpose (generateNewStatesHelper (transpose currState) 0 0 2 isCarMovableToLeft moveCarLeft)
generateNewUpTruckStates currState = map transpose (generateNewStatesHelper (transpose currState) 0 0 3 isTruckMovableToLeft moveTruckLeft)
generateNewDownTruckStates currState = map transpose (generateNewStatesHelper (transpose currState) 0 0 3 isTruckMovableToRight moveTruckRight)

generateNewStatesHelper currState pos rownumber vehiclesize isMovable moveVehicle
  -- termination condition, we have searched all rows
  | (rownumber == (length currState))				= []
  --this means we are at the end of the row, go to the next one.
  | (pos + vehiclesize) > ((length (currState!!rownumber))-1) = generateNewStatesHelper currState 0 (rownumber+1) vehiclesize isMovable moveVehicle
  --if the car is movable, lets move it!
  | isMovable (currState!!rownumber) pos 	= (replaceIthRow 
															rownumber
															currState 
															(moveVehicle (currState!!rownumber) pos)):(generateNewStatesHelper currState (pos + vehiclesize) rownumber vehiclesize isMovable moveVehicle)
  --if it is not movable, don't !
  | otherwise										= generateNewStatesHelper currState (pos + 1) rownumber vehiclesize isMovable moveVehicle



--will have index out of bounds exception if used before checking bounds
moveCarRight row pos = replaceSegment row pos ['-', (row!!pos), (row!!(pos+1))]
moveCarLeft row pos = replaceSegment row pos [(row!!(pos+1)), (row!!(pos+2)), '-']
moveTruckRight row pos = replaceSegment row pos ['-', (row!!pos), (row!!(pos+1)), (row!!(pos+2))]
moveTruckLeft row pos = replaceSegment row pos [(row!!(pos+1)), (row!!(pos+2)), (row!!(pos+3)), '-']

isCarMovableToRight :: String -> Int -> Bool
isCarMovableToRight currRow pos 
  | pos == 0                            = (currRow!!pos == currRow!!(pos+1)) && (not (currRow!!pos == '-')) && (currRow!!(pos+2)) == '-'
  -- this check is to make sure we don't move only the 2nd and 3rd cells of a car. 
  -- fixes a bug with the case of "aaa---" -> "a-aa--", which should be invalid!
  | otherwise                           = (not (currRow!!(pos-1) == currRow!!pos)) &&
                                          (currRow!!pos == currRow!!(pos+1)) && 
                                            (not (currRow!!pos == '-')) &&
                                             (currRow!!(pos+2)) == '-'

isCarMovableToLeft :: String -> Int -> Bool
isCarMovableToLeft currRow pos = (currRow!!(pos+1) == currRow!!(pos+2)) && (not (currRow!!(pos+1) == '-')) && (currRow!!(pos)) == '-'

isTruckMovableToRight :: String -> Int -> Bool
isTruckMovableToRight currRow pos = (currRow!!pos == currRow!!(pos+1) && currRow!!pos == currRow!!(pos+2)) && (not (currRow!!pos == '-')) && (currRow!!(pos+3)) == '-'

isTruckMovableToLeft :: String -> Int -> Bool
isTruckMovableToLeft currRow pos = (currRow!!(pos+1) == currRow!!(pos+2) && currRow!!(pos+1) == currRow!!(pos+3)) && (not (currRow!!(pos+1) == '-')) && (currRow!!(pos)) == '-'

replaceIthRow i currState row 
  | (null currState)                = []
  | (i == 0)                    = row:(tail currState)
  | otherwise                   = (head currState):(replaceIthRow (i-1) (tail currState) row)

-- replaces the whatever is in the old list at position pos with the segment
replaceSegment oldList pos segment
   | pos == 0  = segment ++ drop (length segment) oldList
   | otherwise = 
        (head oldList):
        (replaceSegment (tail oldList) (pos - 1) segment)
