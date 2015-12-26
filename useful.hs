
-------------------------------------------------------------------------------
--  Mike Pierce (mapierce271@gmail.com)
--  A Collection of Useful Haskell Functions
--  useful.hs
-------------------------------------------------------------------------------



module Useful where

-------------------------------------------------------------------------------
-- Sorting Functions
-- Quicksort and mergesort are about the same, quicksort being slightly better.
-- Iterative-mergesort take 5-7 times longer than the other two.
-------------------------------------------------------------------------------

-- Sorts a list using the QuickSort algorithm.
-- Dependencies: partition
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (z:zs) = quicksort (fst part) ++ z : quicksort (snd part)
  where part = partition (<z) zs

-- Usage: partition condition list
-- Partitions the given list into a pair of lists such that 
--   the elements of the first satisfy the condition and
--   the elements of the second don't satisfy the condition.
-- Implemented here so I didn't have to import Data.List.
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition = partition' ([], [])
  where partition' (f,s) _    [] = (reverse f, reverse s)
        partition' (f,s) cond (z:zs)
          | cond z    = partition' (z:f,s) cond zs 
          | otherwise = partition' (f,z:s) cond zs 

-- Sorts a list using the MergeSort algorithm.
-- Dependencies: sortedMerge
mergesort :: (Ord a) => [a] -> [a]
mergesort list = ms' (length list) list
  where ms' 0 l = l
        ms' 1 l = l
        ms' n l = sortedMerge $ split (n`div`2)
          where split mid = ms'' $ splitAt mid l
                  where ms'' (f,s) = (ms' mid f, ms' (n-mid) s)

-- Sorts a list using an Iterative MergeSort algorithm.
-- Dependencies: sortedMerge
mergesortIter :: (Ord a) => [a] -> [a]
mergesortIter list = msi' 2 list
  where msi' n l
          | n > 2*(length l) = l
          | otherwise = msi' (2*n) (concatMap sortedMerge $ splitter l)
          where splitter [] = [([],[])]
                splitter list = split (fst splat) : splitter (snd splat)
                  where split = splitAt (n`div`2) 
                        splat = splitAt n list
        
-- Given a pair of sorted lists, 
-- returns a merged version of the two lists that is also sorted.
sortedMerge :: (Ord a) => ([a],[a]) -> [a]
sortedMerge (l,[]) = l
sortedMerge ([],l) = l
sortedMerge (xxs@(x:xs), yys@(y:ys)) =
    case x`compare`y of
        LT ->   x: sortedMerge ( xs, yys)
        EQ -> x:y: sortedMerge ( xs,  ys)
        GT ->   y: sortedMerge (xxs,  ys)



-------------------------------------------------------------------------------
-- Functions created for Project Euler problem 010
-------------------------------------------------------------------------------

-- Returns a list of all prime numbers.
-- Note that this is not implemented as efficiently as it could be
--   because the sieve iterates over all unchecked composite numbers
--   rather than directly indexing to composites divisible by p.
-- Also since this is an infinite list we cannot utilize the
--   optimization of "stopping at the sqrt of n."
-- Dependencies: wheel2357, spin, sortedComplement
primes :: [Integer]
primes = 2 : 3 : 5 : 7 : sieve (spin wheel2357 11)
  where sieve (p:ps) = p : sieve (sortedComplement ps composites)
          where composites = [p^2,p^2+p..] :: [Integer]

-- Returns all primes less than n.
-- Dependencies: wheel2357, spin, sortedComplement
primesUnder :: Integer -> [Integer]
primesUnder n
  | n < 12    = takeWhile (<n) [2,3,5,7,11]
  | otherwise = 2:3:5:7 : pU (takeWhile (<n) (spin wheel2357 11))
      where pU pps@(p:ps)
              | n < p*p   = pps
              | otherwise = p : pU (sortedComplement ps [p^2,p^2+p..])

-- Utilized in primes and primesUnder.
-- The idea of using wheel2357 and spin is taken from
--   http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
wheel2357 = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:
            8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel2357
spin (x:xs) n = n : spin xs (n+x)

-- Complement (Sorted)
-- Returns the set complement of the given lists
--   assuming that the lists are sorted in ascending order.
-- The idea of using sortedComplement is taken from
--   http://en.literateprograms.org/Sieve_of_Eratosthenes_%28Haskell%29
sortedComplement :: (Ord a) => [a] -> [a] -> [a]
sortedComplement l [] = l
sortedComplement [] _ = []
sortedComplement xxs@(x:xs) yys@(y:ys) = 
  case x`compare`y of
    LT -> x: sortedComplement  xs yys
    EQ ->    sortedComplement  xs yys
    GT ->    sortedComplement xxs  ys



-------------------------------------------------------------------------------
-- Functions created for Project Euler problem 023
-------------------------------------------------------------------------------

-- Returns the proper divisors of n.
-- Example: divisors 12 = [1,3,2,6,4]
-- Dependencies: groupDups, factor
divisors :: Integer -> [Integer]
divisors n = init $ divisors' $ groupDups $ factor n
  where divisors' [] = [1]
        divisors' ((x,p):xps) = [pow*d | pow <- powers, d <- divisors' xps]
          where powers = map (x^) [0..p]

-- Returns a version of the list with consecutive duplicates 
--   grouped into a pair of the form (element, frequency).
-- Example: groupDups [1,2,2,2,3,1,1] = [(1,1),(2,3),(3,1),(1,2)]
groupDups :: (Eq a) => [a] -> [(a, Int)]
groupDups [] = []
groupDups all@(x:_) = (dups:) $ groupDups $ dropWhile (==x) all
  where dups = (x, length $ takeWhile (==x) all) 

-- Returns the prime factors of n.
-- Dependencies: primes
-- This *may* be faster if we use "primesUnder" instead of "primes".
factor :: Integer -> [Integer]
factor n = factor' n $ takeWhile (\q -> q*q < n+5) primes
  where factor' 1 _ = []
        factor' n [] = [n]
        factor' n all@(p:ps)
          | n`mod`p == 0 = p : factor' r (takeWhile (\x -> x*x < r+1) all)
          | otherwise    = factor' n ps
              where r = n`div`p



-------------------------------------------------------------------------------
-- Functions created for Project Euler problem 036
-------------------------------------------------------------------------------

-- Returns n as a list of its digits in base 2.
binDigits :: (Integral a) => a -> [Int]
binDigits n = binDigits' n (reverse $ takeWhile (<=n) $ map (2^) [0..])
  where binDigits' n (x:xs) = 
          case n`compare`x of
            LT -> 0 : binDigits' n xs
            GT -> 1 : binDigits' (n-x) xs
            EQ -> 1 : map (\_ -> 0) xs

-- Returns n as a list of its digits in base 10.
deciDigits :: (Show a) => a -> [Int]
deciDigits n = map (\x -> (read [x]) :: Int) $ show n 

-- Returns true if xs is a palindrome, false otherwise.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = isPalindrome' (take half xs) (reverse xs)
  where half = 1 + length xs `div` 2
        isPalindrome' ys zs = and $ zipWith (==) ys zs 



