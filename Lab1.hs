-- CptS 355 -- Lab 1 (Haskell) - Spring 2021
-- Name: Musa Husseini
-- Collaborated with: 
module Lab1     
    where
-- 1.insert 


import GHC.Classes (Eq)
insert :: (Ord t1, Num t1) => t1 -> t2 -> [t2] -> [t2]
-- param1 -> param2 -> ... -> paramN -> returnType
insert 0 y [] = [y]
insert n y [] = []
insert 0 y (x:xs) = y:x:xs -- -> [x] ++ xs -> [x, y, ...]
insert n y (x:xs) = x:(insert (n - 1) y xs)
-- (x:xs) = [x,] OR [x,y,] OR [x,y,...]



--2. insertEvery

-- insertEvery :: (Eq t, Num t) => t -> a -> [a] -> [a]

-- insertEvery n y xs = insertEvery` n n y xs
--     where 
--         insertEvery` n 0 y xs = [y]
--         insertEvery` _ _ _ [] = []
--         insertEvery` n 0 y xs = y:(insertEvery` n n y xs)
--         insertEvery` n i h (x:xs) = x : (insertEvery` n  (i-1) xs)

        
    -- 3. getSales

getSales :: (Num p, Eq t) => t -> [(t,p)] -> p

getSales _ [] = 0
getSales targetDay ((day, numSold):rest)
    | day == targetDay =  numSold + getSales targetDay rest
    | otherwise = getSales targetDay rest
    -- 4. sumSales

sumSales :: (Num p) => String -> String -> [(String,[String, p])] -> p
sumSales s day [] = 0
sumSales s day ((store, log):xs) | s == store = (getSales day log) + (sumSales s day xs)
                                 | otherwise = sumSales s day xs
    -- 5. split
    -- 6. nSplit