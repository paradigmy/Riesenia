module OkamzineSialenstvo where

import Data.List

-- vo farbeni kocky je ako prva horna strana, potom bocne, nakoniec dolna

data Farba = R | B | G | Y deriving (Show, Eq, Ord)
type FKocka = [Farba]
farbenie1 = [B,R,R,R,G,Y]
farbenie2 = [R,G,Y,G,B,B]
farbenie3 = [R,B,G,R,Y,Y]
farbenie4 = [G,B,R,Y,G,Y]

farbenie5 = [B,R,Y,R,G,Y]
farbenie6 = [G,G,Y,Y,G,B]
farbenie7 = [R,B,G,B,Y,R]
farbenie8 = [G,G,R,Y,G,Y]

data Tvar = Orange | Blue | Pink | Green | Yellow deriving (Show, Eq, Ord)
type TKocka = [Tvar]
farbenie9  = [Orange, Blue,Orange,Pink,Green,Yellow]
farbenie10 = [Green,Pink,Green,Yellow,Orange,Blue]
farbenie11 = [Pink,Green,Pink,Orange,Yellow,Blue]
farbenie12 = [Blue,Green,Blue,Pink,Orange,Yellow]
farbenie13 = [Yellow,Pink,Yellow,Orange,Green,Blue]


-- snaha o backtracking... nefunguje... aj ked som to prepisoval par hodin
naseba1' :: [FKocka]
naseba1' = (naseba'' [otoceniaX x | x <- [farbenie1, farbenie2, farbenie3, farbenie4]] [0..3])!!0

naseba'' :: Eq (a) => [[[a]]] -> [Int] -> [[[a]]]
naseba'' o []  = [[]]
naseba'' o ind = [y:z | i <- ind, y <- o!!i, z <- naseba'' o (ind\\[i]), good (y:z)]
-- koniec snahy


-- v zadani to znelo, ze staci vypisat jedno riesenie
-- tak to moj program aj robi... ale samozrejme generuje
-- ich vsetky, teda sa da dostat ku kazdemu rieseniu

-- riesenie prvej podulohy
naseba1 :: [FKocka]
naseba1 = (naseba [farbenie1, farbenie2, farbenie3, farbenie4])!!0 

-- riesenie druhej podulohy
naseba2 :: [FKocka]
naseba2 = (naseba [farbenie5, farbenie6, farbenie7, farbenie8])!!0

-- riesenie tretej podulohy
naseba3 :: [TKocka]
naseba3 = (naseba' [farbenie9, farbenie10, farbenie11, farbenie12, farbenie13])!!0

naseba :: [FKocka] -> [[FKocka]]
naseba kocky = [[w, x, y, z] | w <- a, x <- b, y <- c, z <- d, good [w, x, y, z]]
        where [a,b,c,d] = [otoceniaX k | k <- kocky]

naseba' :: [TKocka] -> [[TKocka]]
naseba' kocky = [[v, w, x, y, z] | v <- a, w <- b, x <- c, y <- d, z <- e, good [v, w, x, y, z]]
        where [a,b,c,d,e] = [otoceniaX k | k <- kocky]

good :: Eq (a) => [[a]] -> Bool
good x =  (length (nub [y!!1 | y <- x])) == (length x) &&
          (length (nub [y!!2 | y <- x])) == (length x) &&
          (length (nub [y!!3 | y <- x])) == (length x) &&
          (length (nub [y!!4 | y <- x])) == (length x)

otoceniaX :: Eq (a) => [a] -> [[a]]
otoceniaX [a,b,c,d,e,f] = nub ( concat [otoceniaY [a,b,c,d,e,f],
                                   otoceniaY [a,c,d,e,b,f],
                                   otoceniaY [a,d,e,b,c,f],
                                   otoceniaY [a,e,b,c,d,f]]
                              )

otoceniaY :: [a] -> [[a]]
otoceniaY [a,b,c,d,e,f] = concat [otoceniaZ [a,b,c,d,e,f],
                                   otoceniaZ [d,a,c,f,e,b],
                                   otoceniaZ [f,d,c,b,e,a],
                                   otoceniaZ [b,f,c,a,e,d]]

otoceniaZ :: [a] -> [[a]]
otoceniaZ [a,b,c,d,e,f] =  [[a,b,c,d,e,f],
                            [c,b,f,d,a,e],
                            [f,b,e,d,c,a],
                            [e,b,a,d,f,c]]