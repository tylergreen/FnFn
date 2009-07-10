module Heap where

import Utils

type Heap a = (Int, [Int], [(Int, a)])
type Addr = Int

hInit :: Heap a
hInit = (0, [1..], [])
hAlloc (size, (next:free), cts) n = ((size+1, free, (next,n):cts), next)

hUpdate (size, free, cts) a n = (size, free, (a,n) : remove cts a)
hFree (size, free, cts) a = (size-1, a:free, remove cts a)

hLookup (size, free, cts) a = aLookup cts a 
         	     	      	    (error ("can't find node " ++ showaddr a ++ " in heap"))

hAddresses (size, free, cts) = [addr | (addr, node) <- cts]
hSize (size, free, cts) = size

hNull = 0
hIsnull a = a == 0
showaddr a = "#" ++ show a  -- Print # to mark as address

remove :: [(Int, a)] -> Int -> [(Int, a)]
remove [] a = error ("Attempt to update or free nonexistent address #" ++ show a)

remove ((a1, n):cts) a | a == a1 = cts
       	     	       | otherwise = (a1, n) : remove cts a


 
