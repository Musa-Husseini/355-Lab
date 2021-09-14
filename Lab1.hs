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

-- insertEvery :: Int -> a  -> [a] -> [a]

-- insertEvery n = insertEvery` n n 
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


    -- 5. split
    -- 6. nSplit