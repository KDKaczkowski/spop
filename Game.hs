module Game where
import CreekType


-- returns elements from List with coords
getListElementWithCoords :: Coordinate -> [CoordColoured] -> [CoordColoured]
getListElementWithCoords _ []         = []
getListElementWithCoords numberCoord board  = [ a | a <- board, fst a == numberCoord]


-- checks if there is element with passed coords on board 
isElementWithCoords :: Coordinate -> [CoordColoured] -> Bool
isElementWithCoords _ [] = False
isElementWithCoords numberCoord board
    | (length arr) > 0  = True
    | otherwise         = False
    where arr = [ a | a <- board, fst a == numberCoord]


-- Creates an empty board with uncoloured fields
createEmptyBoard :: Coordinate -> [CoordColoured]
createEmptyBoard (m, n)
    | m < 1 || n < 1    = []
    | otherwise         = [ ((a,b), False) | a <- [1..m], b <- [1..n]]


-- returns longer List of two passed
getLongerList :: [a] -> [a] -> [a]
getLongerList x y   | length x > length y = x
                | otherwise           = y


-- returns shorter List of two passed
getShorterList :: [a] -> [a] -> [a]
getShorterList x y  | length x <= length y  = x
                | otherwise             = y


-- returns list without duplicates
removeDuplicatesFromList :: Eq a => [a] -> [a]
removeDuplicatesFromList []       = []
removeDuplicatesFromList (x:xs)   = x : (removeDuplicatesFromList [ a | a <- xs, a /= x])


-- returns list without empty fields
removeEmptyListsFromList :: Eq a => [[a]] -> [[a]]
removeEmptyListsFromList []      = []
removeEmptyListsFromList x       = [ a | a <- x, a /= []]


-- return size of board
getBoardSize :: Creek -> BoardSize
getBoardSize (Creek size _)   = size


-- checks validity of numbers on board
-- returns false when requirements can not be satisfied
-- for example number can not be lower than 0 or greater than 4 in any case
checkNumberRequirements :: BoardSize -> [CoordValue] -> Bool
checkNumberRequirements (m, n) reqs
    | (length outRange) > 0     = False
    | otherwise                 = True
    where outRange = [ a | a <- reqs, ((fst (fst a)) > m || (snd (fst a)) > n || snd a < 0 || snd a > 4)] 


-- calculating all possibilites of colouring fields to match reqiurements --


-- returns coords of fields that might be coloured to match requirements for  number
-- placed in passed coords on boards with passed size
-- for example coords: (1,1) size: (2,2) -> possible fields to colour: [(1,1), (1,2), (2,1), (2,2)]
getFieldsToColour :: Coordinate -> BoardSize -> [Coordinate]
getFieldsToColour (x, y) (m, n) = 
    [ (a, b) | a <- [x, x + 1], b <- [y, y + 1], a > 0, b > 0, a <= m, b <= n ]


-- returns all n-element, where n is passed to function, combinations from passed list
possibilites :: Int -> [a] -> [[a]]
possibilites 0 _    = [[]]
possibilites _ []   = []
possibilites n (x:xs) = (map (x:) (possibilites (n-1) xs)) ++ (possibilites n xs)


-- returns all possibilities of field which if coloured will match requirements of
-- number with value = k, placed in passed coords for board with descripted size
getColourAroundNumberPossibilites :: CoordValue -> BoardSize -> [[Coordinate]]
getColourAroundNumberPossibilites ( numberCoord, k ) size = possibilites k (getFieldsToColour numberCoord size)


-- inserts white tiles if not all tiles are specified in all list of possibilities to colour tiles on board   -----------------------------DO POPRAWY
markWhiteTilesInPossibilites :: [[Coordinate]] -> Coordinate -> BoardSize -> [[CoordColoured]]
markWhiteTilesInPossibilites [] _ _ = []
markWhiteTilesInPossibilites arr numberCoord size = map (markWhiteTilesForPossibilityList numberCoord size) arr


-- inserts white tiles if not all tiles are specified in list of possibilities to colour tiles on board
markWhiteTilesForPossibilityList :: Coordinate -> BoardSize -> [Coordinate] -> [CoordColoured]
markWhiteTilesForPossibilityList numberCoord size arr = [ (x, (x `elem` arr)) | x <- (getFieldsToColour numberCoord size)]


-- checks if field (x) is already coloured in passed list (y:ys).
-- If so return False. In other case return True
checkIfFieldColoured :: [CoordColoured] -> CoordColoured -> Bool
checkIfFieldColoured []  _ = True
checkIfFieldColoured (y:ys) x      | x `elem` (y:ys)                               = True
                        | ((fst x) == (fst y)) &&  ((snd x) /= (snd y)) = False 
                        | otherwise                                     = checkIfFieldColoured ys x


-- Combine two lists of colouring possibilities
combineTwoNeighbours :: [CoordColoured] -> [CoordColoured] -> [CoordColoured]
combineTwoNeighbours [] x = x
combineTwoNeighbours x [] = x
combineTwoNeighbours x y
    | False `elem` res      = []
    | otherwise             =  removeDuplicatesFromList (x ++ y)
    where   shorter = getShorterList x y
            longer  = getLongerList x y
            res     = map (checkIfFieldColoured longer) shorter
            
            


-- returns all possible combinations for 2 neighbour numbers
combineTwoNumbersNeighbours :: [[CoordColoured]] -> [[CoordColoured]] -> [[CoordColoured]]
combineTwoNumbersNeighbours x y = removeEmptyListsFromList [ combineTwoNeighbours a b | a <- x, b <- y]


-- returns list of lists which elements match all number requirements on board
combineNumbersPossibleNeighbours :: [[[CoordColoured]]] -> [[CoordColoured]]
combineNumbersPossibleNeighbours []        = []
combineNumbersPossibleNeighbours (x:[])    = []
combineNumbersPossibleNeighbours (x:xs)    = foldl (combineTwoNumbersNeighbours) x xs

-- Complete all lists of elements matching all number requirements with 
-- elements unspecified by any numbers
colourUnresolvedFields :: [[CoordColoured]] -> BoardSize -> [[CoordColoured]]
colourUnresolvedFields [] _          = []
colourUnresolvedFields boards size   = concat ( map (colourUnresolvedFieldsSingleBoard size) boards)

-- Complete single list of elements matching all number requirements with 
-- elements unspecified by any numbers
colourUnresolvedFieldsSingleBoard :: BoardSize -> [CoordColoured] -> [[CoordColoured]]
colourUnresolvedFieldsSingleBoard _ []      = []
colourUnresolvedFieldsSingleBoard size board = [ board ++ a | a <- combinations]
    where   combinations = combineUnspecifiedFields' left
            left = getUnspecifiedFields board size

-- returns all possibilities of fields to colour which can complete all numbers requirements
findAllNumbersPossibleNeighbours :: Creek -> [[[CoordColoured]]]
findAllNumbersPossibleNeighbours (Creek (0, _) _)   = []
findAllNumbersPossibleNeighbours (Creek (_, 0) _)   = []
findAllNumbersPossibleNeighbours (Creek (_, _) [])  = []
findAllNumbersPossibleNeighbours (Creek size (x:xs)) =
    [ markWhiteTilesInPossibilites possibilites numberCoords size ] 
            ++ findAllNumbersPossibleNeighbours (Creek size xs)
    where   possibilites = getColourAroundNumberPossibilites x size
            numberCoords = fst x


-- solve unspecified fields


-- creating list of all possible combinations of unspecified fields
combineUnspecifiedFields' :: [Coordinate] -> [[CoordColoured]]
combineUnspecifiedFields' [] = []
combineUnspecifiedFields' x = foldl (combineUnspecifiedFields) [] x


-- add new unspecified field to list of all possible combations of its
combineUnspecifiedFields :: [[CoordColoured]] -> Coordinate -> [[CoordColoured]]
combineUnspecifiedFields [] coord   = [ [(coord, a)] | a <- [True,False]]
combineUnspecifiedFields x coord    = [ a ++ [(coord, b)] | a <- x, b <- [True, False]]


-- returns list of fields that aren't described by nubmer requirements
getUnspecifiedFields :: [CoordColoured] -> BoardSize -> [Coordinate]
getUnspecifiedFields [] (m,n)      = [ (a,b) | a <- [1..m], b <- [1..n]]
getUnspecifiedFields board (m,n)   = [ (a,b) | a <- [1..m], b <- [1..n], (isElementWithCoords (a,b) board) == False]

-- try to find boards with creek


-- iterate through list to find boards with creek
tryCreateCreek :: [[CoordColoured]] -> [[CoordColoured]]
tryCreateCreek []     = []
tryCreateCreek boards = [ a | a <- boards, checkIfSolutionHaveCreek a == True]

-- checks if it is possible to make a creek on board
checkIfSolutionHaveCreek :: [CoordColoured] -> Bool
checkIfSolutionHaveCreek []     = True
checkIfSolutionHaveCreek board 
    | (length whiteEl) > 0  = (compareCreek creek whiteEl)
    | otherwise             = True
    where   whiteEl = getWhiteFields board
            creek   = singleCreek board (whiteEl !! 0)


-- check if there is no white field on board which does not connect to main creek
compareCreek :: [CoordColoured] -> [CoordColoured] -> Bool
compareCreek _ [] = True
compareCreek [] _ = False
compareCreek (x:xs) board = compareCreek xs [ a | a <- board, a /= x ]


-- tries to connect all white fields into single creek
singleCreek :: [CoordColoured] -> CoordColoured -> [CoordColoured]
singleCreek [] _     = []
singleCreek board x  = x : neighbours ++ (concat (map (singleCreek newBoard) neighbours))
    where   neighbours = getWhiteNeighbours x board
            newBoard = [ a | a <- board, isNeighbour a x == False, a /= x ]


-- returns all white fields
getWhiteFields :: [CoordColoured] -> [CoordColoured]
getWhiteFields [] = []
getWhiteFields board = [ a | a <- board, snd a == False]


-- checks if two fields are adjacent to each other
isNeighbour :: CoordColoured -> CoordColoured -> Bool
isNeighbour ((a,b), _) ((c,d), _)
    | (a == c) && ( (b == (d-1)) || (b == (d+1) ))  = True
    | (b == d) && ( (a == (c-1)) || (a == (c+1) ))  = True
    | otherwise                                     = False

-- returns all adjacent white fields to the given one
getWhiteNeighbours :: CoordColoured -> [CoordColoured] -> [CoordColoured]
getWhiteNeighbours ((m,n), _) board = 
    [ a | a <- board, 
        snd a == False, 
        fst a `elem` (filter (>= (1,1)) [(m, n-1), (m, n+1), (m-1, n), (m+1,n)])]



findCreek :: Creek -> Either String [CoordColoured]
findCreek (Creek (0, _) _) = Left "Zly rozmiar planszy"
findCreek (Creek (_, 0) _) = Left "Zly rozmiar planszy"
findCreek (Creek size [])  = Right (createEmptyBoard size)
findCreek (Creek size reqs) 
    | (checkNumberRequirements size reqs) == False = Left "Nie można spelnic wymagan planszy"
    | (length creeks)  > 0              = Right (creeks !! 0)
    | otherwise                         = Left "Nie udalo sie znalezc potoku."
    where   req     = findAllNumbersPossibleNeighbours (Creek size reqs)
            res     = combineNumbersPossibleNeighbours req
            boards  = colourUnresolvedFields res size
            creeks  = tryCreateCreek boards
