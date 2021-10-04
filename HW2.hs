module HW2
     where

{- 1. groupbyNTail - 10%-}

groupbyNTail::[a]->Int->[[a]]

groupbyNTail iL n = groupHelper iL n []
     where
          groupHelper [] n buf =  (reverse buf): []
          groupHelper (x:xs) n buf | (length buf) >= n = (groupHelper xs n ((x:[]) : (reverse buf)))
                                   | otherwise = groupHelper xs n (x:buf)
-----------------------------------------------------------

{- 2.  elemAll and stopsAt  -  20% -}

{- (a)  elemAll - 10%-}
-- please don't include the myCatsLog list in your solution file. 

{- (b) stopsAt - 10%-}

-----------------------------------------------------------

{- 3. isBigger and applyRange - 25% -}

--define the Timestamp datatype
data Timestamp =  DATE (Int,Int,Int) |  DATETIME (Int,Int,Int,Int,Int) 
                  deriving (Show, Eq)

{- (a)  isBigger - 15% -}

{- (b) applyRange - 10% -}


-----------------------------------------------------------
{-4 - foldTree, createRTree, fastSearch  - 35%-}

--define Tree and RTree data types
data Tree a = LEAF a | NODE a (Tree a) (Tree a)
               deriving (Show,  Eq, Ord)

data RTree a = RLEAF a | RNODE a (a,a) (RTree a) (RTree a)
                    deriving (Show, Eq, Ord)

{- (a) foldTree - 8% -}

{- (b) createRTree - 12% -}

{- (c) fastSearch - 15% -}

-------------------------------------------------------------------

{- Tree Examples 5% -}
-- include your tree examples in the test file. 

{-Testing your tree functions - 5%-}


