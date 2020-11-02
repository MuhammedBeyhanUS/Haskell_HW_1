module HW1 (
    form,
    constGrid,
    flatten,
    access,
    slice,
    vcat,
    hcat,
    without,
    matches2d
) where

-- do not modify the module declaration above!
-- this will ensure that you cannot load (compile)
-- the module without implementing all of the functions.

-- If you have functions you do not want to implement,
-- leave them as undefined or make them have another
-- default value. If you fully remove any of their definitions,
-- that will be a compilation error during evaluation,
-- and you will be eligible for (yay!) a 5 point deduction
-- (that's bad for your grade). Runtime errors in your code 
-- (rather than compilation errors) are acceptable and will simply
-- result in you getting zero from the specific test case causing
-- an error.

-------------------------
-- Fellowship of the Grid (25, 5, 5, 5 points)
form :: [a] -> (Int, Int) -> [[a]] 
form _ (0,_) = []
form l (r,c) = ((take c l):form (drop c l) (r-1, c))

constGrid :: a -> (Int, Int) -> [[a]]
constGrid v (r,c) = form (take (c*r) (cycle [v])) (r,c)

flatten :: [[a]] -> [a]
flatten xss = [x | xs<-xss, x<-xs]

access :: [[a]] -> (Int, Int) -> a
--------------------------------------------------------
--pos k (x:xs) = if k==0 then x else pos (k-1) xs   --correct but when you give bigger row or collumb than actually exist, it gives a respond anyway which is from one of teh next rows.
--access l (r,c) = pos ((r*(length l)+c)) (flatten l)
---------------------------------------------------------
--access l (r,c) = (!!) (flatten l) (r*(length (head l)+c)) -- smth that i learn now. using !! instead of pos funksion. but it it doent work if access elem is in the 1. row.
-------------------------------------------------------
--access l (r,c) = [x | x<-(flatten l), x = ((r*(length l)+c))]   wrong try

--------------------------------------------------------
access0 :: [a] -> Int -> a    --correct and no gap or problem 

access0 l c = if c==0 then head l 
              else access0 (drop 1 l) (c-1)

access ll (r,c) = if r==0 then access0 (head ll) c
                  else access (drop 1 ll) (r-1,c)

--------------------------------------------------------
-- The Two Signatures (10, 5, 5, 10 points)
slice :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]

slice0::[[a]]->(Int,Int)->[[a]]

slice0 [] _ = []
slice0 l (0,c1) = (take (c1) (head l)): slice0 (drop 1 l) (0,c1)
slice0 l (c0,c1) = (take (c1-1) (drop c0 (head l))): slice0 (drop 1 l) (c0,c1)

slice l (r0,r1) (c0,c1) = if r0/=0 then slice (drop 1 l) (r0-1,r1-1) (c0,c1)
                          else slice0 (take r1 l) (c0,c1)
 
vcat :: [[a]] -> [[a]] -> [[a]]
vcat [[]] [[]] = []
vcat [] l = l
vcat [[]] l = l
vcat l [[]] = l
vcat l0 l1 = vcat (take ((length l0)-1) l0) ((last l0):l1)

hcat :: [[a]] -> [[a]] -> [[a]]
hcat [[]] [[]] = []
hcat [] l = l
hcat [[]] l = l
hcat l [[]] = l
hcat l0 l1 = ((head l0)++(head l1)):(hcat (tail l0) (tail l1))

without :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]
--without0 :: [[a]] -> (Int, Int) -> [[a]]


delrow :: (Int,Int) -> [[a]] -> [[a]]
delrow (r0,r1) l  = take r0 l ++ (drop r1 l)


delcol :: (Int,Int) -> [a] -> [a]
delcol (c0,c1) l  = take c0 l ++ (drop c1 l)

lastt :: (Int,Int) -> [[a]] -> [[a]]
lastt (_,_) [] = []
lastt (c0,c1) l = delcol (c0,c1) (head l):(lastt (c0,c1) (drop 1 l))

without l (r0,r1) (c0,c1) = lastt (c0,c1) (delrow (r0,r1) l)
                           
----------------------------
-- Return of the Non-trivial (30 points, 15 subject to runtime constraints)
matches2d :: Eq a => [[a]] -> [[a]] -> [(Int, Int)]

--wholecheck :: [[a]] -> [[a]] -> (Int,Int) -> (Int,Int)
--wholecheck l k (r,c) = if ( l == k ) then (r,c) else ()

--matches2d0 :: [[a]] -> [[a]] -> (Int,Int) -> (Int,Int)
--matches2d0 l k (r,c) = if (access l (r,c)) == (access k (0,0)) then wholecheck (slice l (r,length k) (c,length(head k))) k (r,c) else ()

--arafunc :: [[a]] -> [[a]] -> (Int,Int) -> [(Int,Int)]
--arafunc l k (r,c) = if ((r+(length k) <= length l) && (c+(length (head k)) <= (length (head l)))) then matches2d0 l k (r,c) : arafunc l k incrc l (r,c) else []

--incrc:: [[a]] -> (Int,Int) -> (Int,Int)
--incrc l (r,c) = if (((length (head l))) == c+1) then (r+1,0) else (r,c+1)

matches2d l k = undefined -- arafunc l k (0,0)


--matches2d0 l k (r,c) = matches2d0 () l k ++ (cntrl (slice l () ()) k) --[(0,0)]
----------------------------
-- What is undefined? Just a value that will cause an error
-- when evaluated, from the GHC implementation:
-- undefined = error "Prelude.undefined"
-- But it allows your module to be compiled
-- since the function definitions will exist.
