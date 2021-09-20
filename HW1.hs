-- CptS 355 - Fall 2021 -- Homework1 - Haskell
-- Name: Musa Husseini
-- Collaborators: Musa Husseini

module HW1
     where

-- Q1 everyOther

everyOther :: [a] -> [a]

everyOther = everyOther'
     where
          everyOther' [] = []
          everyOther' (x:xs) = x : everyOther xs 
          everyOther [] = []
          everyOther (x:xs) = everyOther' xs




-- Q2(a) eliminateDuplicates
eliminateDuplicates :: Eq a => [a] ->[a]

eliminateDuplicates [] = []
eliminateDuplicates (x:xs) 
      | x `elem` xs = eliminateDuplicates xs
      | otherwise = x  : eliminateDuplicates  xs



-- Q2(b) matchingSeconds


matchingSeconds :: (Eq t) => t -> [(t, a)] -> [a]

matchingSeconds _ [] = []
matchingSeconds targetAnimal ((animal, num) : rest)
     | targetAnimal == animal = num : (matchingSeconds targetAnimal rest)
     | otherwise = matchingSeconds targetAnimal rest
-- -- Q2(c) clusterCommon

clusterCommon::(Eq t,Eq a) => [(t,a)]->[(t,[a])]

clusterCommon [] = []
clusterCommon ((animal, num) : rest)  = animal : (matchingSeconds )

-- Q3 maxNumCases

maxNumCases::(Num p, Ord p, Eq t) => [(a, [(t,p)])] -> t-> p

maxNumCases [] t = 0
maxNumCases ((month, num) : xs) targetMonth | targetMonth == month = num + maxNumCases month xs
                                  | otherwise = maxNumCases month xs


-- Q4 groupIntoLists
groupIntoLists :: [a] -> [[a]]
groupIntoLists [] = [[]]
groupIntoLists (x:xs) = (x:xs) : groupIntoLists xs

-- Q5 getSlice 

getSlice :: Eq a => (a,a) -> [a] -> [a]

getSlice _ [] = []
getSlice (d1, d2) (x:xs) | d1 == x = x : getSlice (d1, d2) xs
                         | d2 == x = x : getSlice (d1, d2) xs
                         | otherwise = getSlice (d1, d2) xs

