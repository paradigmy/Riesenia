import Data.List

cartSucin3 :: [a] -> [b] -> [c] -> [(a,b,c)]
cartSucin3 a b c =  [(x,y,z) | x <- a, y <- b, z <- c]

idk :: Eq(a) => Eq(b) => Eq(c) => [a] -> [b] -> [c] -> [(a,b,c)]
idk a b c =  [(x,y,z) | x <- nub a, y <- nub b, z <- nub c]
