# sudoku

The solver populates possibile entries, eliminates inadmissible redundant entries, & then repeats until finds a solution.  

solve :: Grid -> [Grid]
solve  = search . prune . choices

search :: Matrix [Char]  [Grid]
search m
  | blocked m  = []
  | complete m = collapse m
  | otherwise  = [g | m' <- expand m
                    , g  <- search (prune m')]
