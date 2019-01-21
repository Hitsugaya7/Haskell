import Data.List
import Data.Ord
import Data.Eq
import Data.Char
import Data.Monoid
import Data.Ratio

---------------------------------------------------
--FP1
id_ :: a -> a
id_ x=x

eval :: (a -> b, a) -> b
eval (a,b) = a b

exchange :: (a, b) -> (b, a)
exchange (a,b) = (b,a)

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g a = f(g a)

curry_ :: ((a,b) -> c) -> (a -> b -> c)
curry_ f = c where c a b = f (a,b)

associate :: (a, (b, c)) -> ((a, b), c)
associate (a, (b, c)) = ((a, b), c)


---------------------------------------------------
--FP2
minMax :: Ord a => [a] -> Maybe (a, a) -- (min, max)
minMax [] = Nothing
minMax [x] = Just(x,x)
minMax (x:xs) = Just(a,b) where
	(a,b) = helperOne(x:xs) where
		helperOne [x]=(x,x)
		helperOne (x:xs) = helperTwo x (helperOne xs) where
			helperTwo x (min,max) | x<min = (x,max) | x > max = (min,x) | otherwise = (min,max) 

---------------------------------------------------
--FP3


sumDigits :: Integer ->Integer
sumDigits number |abs(number) < 10 = abs(number) | otherwise = mod (abs(number)) 10 + sumDigits (div (abs(number)) 10)


countDigits :: Integer ->Integer
countDigits number  |abs(number) < 10 = 1 | otherwise = 1 + countDigits (div (abs(number)) 10)  

---------------------------------------------------
--FP4


mostCommon' :: Eq a => [a] -> Maybe a	
mostCommon' [] = Nothing
mostCommon' [x] = Just x
mostCommon' (x:xs) = (\(a,b) -> if a > 0 then (topCommon (x:xs) b) else Nothing) $ topElCommon xs 1 x

topElCommon [x] c symb | c == 0 = (c+1, x)
					   | symb == x = (c+1, symb)
					   | otherwise = (c-1, symb)
topElCommon (x:xs) c symb | c == 0 = topElCommon xs (c + 1) x
						  | symb == x = topElCommon xs (c + 1) symb
						  | otherwise = topElCommon xs (c - 1) symb

topCommon :: Eq a => [a] -> a -> Maybe a
topCommon (x:xs) n =
	topCommon' (x:xs) n ((length (x:xs)) `div` 2 + 1) where

		topCommon' [x] n len 
						| len == 0 = Just n
						| x == n && len <= 1 = Just n
						| otherwise = Nothing
		topCommon' (x:xs) n len 
						| len == 0 = Just n
						| x == n = topCommon' xs n (len - 1)
						| otherwise = topCommon' xs n len 

---------------------------------------------------
--FP5
f:: (a -> a) -> Int -> (a -> a)
f g a | a==0 = error "error"| a == 1 = g | a>1 = g .f g (a-1)



---------------------------------------------------
--FP6

fibbonachi :: Integer -> Integer

fibbonachi n = mod (helper 0 1 n) 10 where 
	helper a b n | n==0 = a |otherwise = helper (a+b) a (n-1) 


---------------------------------------------------
--FP7
isPalindrome :: String -> Bool
isPalindrome a = helper 0 ((length a)-1) where helper begin end | (map toLower a!!begin) == (map toLower a!!end) && (begin<=end) = helper (begin+1) (end-1) | begin>end = True |otherwise =  False


---------------------------------------------------
--FP1
circShiftL :: Int -> [a] -> [a]
circShiftL sdvig massiv | sdvig >= length massiv = circShiftL (mod sdvig (length massiv)) massiv |sdvig < 0 = circShiftL (length massiv - (mod sdvig (length massiv))) massiv | otherwise = (drop (length massiv - sdvig) massiv) ++ (take (length massiv - sdvig)massiv)



---------------------------------------------------
--FP2
indices :: [a] -> [(Integer, a)]
indices a = zip [0..] a

zeroBy :: Monoid a => [a] -> (a -> Bool) -> [a]
zeroBy x f = map (\t->if f t then t else mempty t) x

triplewiseSum :: [Integer] -> [Integer] -> [Integer] -> [Integer]
triplewiseSum a b c = map helper (zip3 a b c) where helper (x,y,z) = x+y+z



---------------------------------------------------
--FP3
revRange :: (Char,Char) -> [Char]
revRange = unfoldr fun
fun (begin,end) | begin>end = Nothing | (minBound::Char) == end = Just(end,(succ begin,end))|otherwise = Just(end, (begin,pred end))



---------------------------------------------------
--FP4
seriesK :: Integer -> [Rational]
seriesK a = helper 1 a where helper b a = (1%b) : helper (b*a) a


---------------------------------------------------
--FP5

newtype SortedList a = SortedList { getSorted :: [a] } deriving (Eq, Show)

myMerge:: Ord a => [a]->[a]->[a]
myMerge list [] = list
myMerge [] list = list
myMerge first@(element1:list1) second@(element2:list2) | element1<element2 = element1 : myMerge list1 second | otherwise = element2 : myMerge list2 first


instance Ord a => Semigroup (SortedList a) where
	  (SortedList as) <> (SortedList bs) =SortedList( myMerge as bs)

instance Ord a=> Monoid (SortedList a) where 
			mempty = SortedList []	  


---------------------------------------------------
--FP6

msort :: Ord a => [a] -> SortedList a 
msort [] = SortedList []
msort [x] = SortedList [x]		
msort list = left <> right where
	left = msort (take middle list)
	right = msort (drop middle list)
	middle = div (length list) 2

---------------------------------------------------
--FP7


data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Eq, Show)

newtype Preorder a = PreO (Tree a) deriving (Eq, Show)
newtype Postorder a = PostO (Tree a) deriving (Eq, Show) 
newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show) 

instance Foldable Tree where
    foldr f ini Nil = ini
    foldr f ini (Node l x r) = foldr f (f x (foldr f ini r)) l
 
instance Foldable Preorder where
    foldr f z (PreO tree) = go tree z
        where 
            go Nil z = z
            go (Node l x r) z = f x . go r . go l $ z
 
instance Foldable Postorder where
    foldr f z (PostO tree) = go tree z
        where 
            go Nil z = z
            go (Node l x r) z = go r . go l . f x $ z

instance Foldable Levelorder where
    foldr f z (LevelO tree) = foldr f z (levelorder tree)  
        where 
            levelorder t = loop [t]
                where 
                    loop []                  = []
                    loop (Nil          : xs) = loop xs
                    loop (Node l x r : xs) = x : loop (xs ++ [l,r]) 
























