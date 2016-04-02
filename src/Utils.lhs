\begin{code}
module Utils where

import Data.List

showSize :: Integer -> String
showSize x = result where
    split = map (`mod` 1024) $ iterate (`div` 1024) x
    zipped = filter (not. null) $ map printPiece $ reverse $ zip ["", "K", "M", "G", "T"] split
    result = intercalate " " zipped
    printPiece (s,i) = if i > 0
        then show i ++ s ++ "B"
        else ""
\end{code}
