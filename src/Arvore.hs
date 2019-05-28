module Main(main) where


import Cp
import List
import Data.Monoid
import Control.Applicative
import Nat
import Graphics.Gloss

data FTree a b = Unit b | Comp a (FTree a b) (FTree a b) deriving (Eq , Show)

type PTree = FTree Square Square
type Square = Float


inPTree :: Either Square (Square , (PTree ,PTree)) -> PTree
inPTree (Left x) = Unit x
inPTree (Right (x,(a,b)) ) = Comp x  a b

outPTree :: PTree -> Either Square (Square , (PTree ,PTree))
outPTree (Unit x) = i1 x
outPTree (Comp x  a b) = i2 (x,(a,b)) 

myfunc (a,(b,c)) = Comp a b c
inFTree = either Unit (myfunc)

outFTree (Unit c) = Left c
outFTree (Comp a b c) = Right(a,(b,c))

baseFTree f g h  = g -|- (f  >< (h >< h))

recFTree f = baseFTree id id f

cataFTree a = a . (recFTree (cataFTree a)) . outFTree

anaFTree f = inFTree . (recFTree (anaFTree f) ) . f

hyloFTree a c = cataFTree a . anaFTree c

instance BiFunctor FTree where
    bmap f g =  cataFTree ( inFTree . baseFTree f g id)


generatePTree n = anaFTree f (n,50) where
  f (n,i) = if(n <= 0) then i1 i else i2 (i,((n-1,g),(n-1,g)))
    where g = (i*(sqrt(2)/2))


drawPTree = cycle.cataFTree (either f g) where
  f q = [square q]
  g (q, (e,d)) = (f q) ++ auxP (f q) ((esq e),(dir d)) where 
   esq [] = []
   esq (h:t) = [(translate (-q/2) q (rotate (-45) h))] ++ (esq t)
   dir [] = []
   dir (h:t) = [(translate (q/2) q (rotate (45) h))] ++ (dir t)


auxP :: [Picture] -> ([Picture],[Picture]) -> [Picture]
auxP [p]((h1:t1),(h2:t2)) = pictures[p,h1,h2] : auxP [p] (t1,t2) 
auxP _ _ = []      

--main :: IO()
main = animatePTree 10 


window = (InWindow "CP" (800,800) (0,0))
square s = rectangleSolid s s


animatePTree :: Integer -> IO ()

animatePTree n = animate window white draw
    where
    pics = drawPTree (generatePTree n)
    draw t = pics !! (floor (t))









