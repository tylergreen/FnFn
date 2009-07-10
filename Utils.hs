module Utils
    where

mappend :: (a -> [b]) -> [a] -> [b]
mappend fn list = foldr1 (++) (map fn list)

find :: (Eq a) => a -> [a] -> Maybe a
find elem [] = Nothing
find elem (x:xs) = if elem == x then Just x
                   else find elem xs

unfold p f g x = if p x
                 then []
                 else f x : unfold p f g (g x)

unfold1 p f g x = if p x
                  then [f x]
	          else f x : unfold1 p f g (g x)

refold c n p f g = foldl c n . unfold p f g

foldp _ init _ [] = init
foldp op init p (x:xs) = if p x
                         then init
                         else foldp op (op x init) p xs 

hylo c n p1 f g p2 = foldp c n p1 . unfold p2 f g

aLookup :: (Eq a) => [(a,b)] -> a -> b -> b
aLookup [] k1 def = def
aLookup ((k,v):bs) k1 def | k == k1 = v
		      	  | otherwise = aLookup bs k1 def
hd :: [a] -> a
hd = head

tl :: [a] -> [a]
tl = tail

mapAccuml :: (a-> b -> (a,c)) -> a -> [b] -> (a, [c])
mapAccuml f acc [] = (acc, [])
mapAccuml f acc (x:xs) = (acc2, x1:xs1)
	    	       	 where (acc1, x1) = f acc x
			       (acc2, xs1) = mapAccuml f acc1 xs

