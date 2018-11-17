import Control.Monad

data Cyclist a = Elem (Cyclist a) a (Cyclist a)

fromList :: [a] -> Cyclist a

fromList (x:xs) = res
        where
                res = Elem prev x next
                (next, prev) = aux res xs res

aux prev [] first = (first, prev)
aux prev (x:xs) first = (this, last)
        where
                this = Elem prev x tmp
                (tmp, last) = aux this xs first


forward :: Cyclist a -> Cyclist a
forward ( Elem _ _ a  ) = a

backward :: Cyclist a -> Cyclist a
backward ( Elem a _ _ ) = a

label :: Cyclist a -> a
label ( Elem _ a _ ) = a

-- ---------------

enumInts :: Cyclist Integer
enumInts = Elem neg 0 pos where 
    neg = negative enumInts (-1) 
    pos = positive enumInts 1 

negative :: (Num a) => Cyclist a -> a -> Cyclist a
negative gt x = this where 
    this = Elem lt x gt
    lt = negative this (x-1) 

positive :: (Num a) => Cyclist a -> a -> Cyclist a
positive lt x = this where 
    this = Elem lt x gt 
    gt = positive this (x+1) 


ex6 :: Integer
ex6 = label . forward . forward $ enumInts

newtype Cyc a b = Cyc (Cyclist a -> (b, Cyclist a)) 

instance Monad (Cyc a) where 
    (Cyc comp) >>= f = 
        Cyc (\st -> 
            let (x, st') = comp st 
                Cyc comp' = f x                                  
            in comp' st') 
    return x = Cyc (\st -> (x, st)) 


runCyc :: Cyclist a -> (Cyc a b) -> b 
runCyc st (Cyc comp) = fst $ comp st 

fwd :: Cyc a () 
fwd = Cyc (\st -> ((), forward st)) 

bkw :: Cyc a () 
bkw = Cyc (\st -> ((), backward st))

lbl :: Cyc a a
lbl = Cyc (\st -> (label st, st)) 

example :: Integer 
example = runCyc enumInts (do
        bkw
        bkw
        bkw
        bkw
        x <- lbl
        fwd
        fwd
        y <- lbl
        fwd
        z <- lbl
        return (x+y+z))
