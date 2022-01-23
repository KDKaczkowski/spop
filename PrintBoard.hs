module PrintBoard where
import CreekType

-- Build String with board
boardToStrl :: [CoordColoured] -> BoardSize -> Int -> Int -> String
boardToStrl [] _ _ _          = "Rozwiązanie nie zostało znalezione\n"
boardToStrl board (row,col) i j
    | j<=col && i<=row = (printElement board i j) ++" | "++(boardToStrl board (row,col) i (j+1))
    | j>col && (i+1)<=row = "\n"++(printElement board (i+1) 1)++" | "++(boardToStrl board (row,col) (i+1) 2 )
    | i>row = " "
    | otherwise = " "

-- get element with coords (i,j) on board
printElement:: [CoordColoured] -> Int -> Int -> String
printElement board i j = printElem [ a | a <- board, fst (fst a) == i && snd (fst a) == j ] 

-- print one element
printElem :: [CoordColoured] -> String
printElem [] = "[ ]"
printElem [coord] 
    | snd coord == True     = "[X]"
    | otherwise         = "[ ]"
      

-- print whole solution
printBoard :: [CoordColoured] -> BoardSize -> IO()
printBoard [] _         = putStrLn "Rozwiązanie nie zostało znalezione\n"
printBoard board size   =  putStrLn ( boardToStrl board size 1 1 )
