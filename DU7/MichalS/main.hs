import Data.List

-- uloha 1:
powers235 :: Int -> Integer
powers235 n | n <= 0 = error "n must be greater than 0"
            | n == 1 = 1
            | otherwise = powers n 1 1 1
            where powers :: Int -> Int -> Int -> Int -> Integer
                  powers 2 i j k = min (min (2 ^ i) (3 ^ j)) (5 ^ k)
                  powers n i j k = let p2 = (2 ^ i)
                                       p3 = (3 ^ j)
                                       p5 = (5 ^ k) in
                                   if p2 < p3 && p2 < p5 then powers (n - 1) (i + 1) j k
                                   else if p3 < p2 && p3 < p5 then powers (n - 1) i (j + 1) k
                                   else powers (n - 1) i j (k + 1)

-- riesenie a): powers235 1000
-- riesenie b): powers235 10000


-- uloha 2:
vso :: [t] -> Int -> [[t]]
vso _ 0 = [[]]
vso [] _ = []
vso zoz k = let vzoz = vso zoz (k - 1) in [ x:y | y <- vzoz, x <- zoz ]

count :: (Eq t) => t -> [t] -> Int
count e zoz = length (filter (\x -> x == e) zoz)

trieda :: [[String]]
trieda = map concat (permutations [["Adam"], ["Betka"], ["Cecilia"], ["Dasa"], ["Emil"], ["Ferko", "Filomena"]])
          ++
         map concat (permutations [["Adam"], ["Betka"], ["Cecilia"], ["Dasa"], ["Emil"], ["Filomena", "Ferko"]])

kocky :: [[String]]
kocky = filter (\x -> ((count "cervena" x) < 2) && ((count "modra" x) < 3) && ((count "zelena" x) < 4)) (vso ["cervena", "modra", "zelena"] 6)

gameofcodes :: [[String]]
gameofcodes = filter (\x -> (count "dole" x) == 5) (vso ["dole", "vpravo"] 10)

-- riesenie a): trieda
-- riesenie b): kocky
-- riesenie c): gameofcodes


-- uloha 5:
kruh :: [[Int]]
kruh = filter (\zoz -> nedelitelne357 (head zoz + last zoz) && spravny zoz) (permutations [1..10])
       where spravny :: [Int] -> Bool
             spravny [] = True
             spravny [_] = True
             spravny (x:xs) = if nedelitelne357 (x + head xs) then spravny xs else False
             nedelitelne357 :: Int -> Bool
             nedelitelne357 n = n `mod` 3 /= 0 && n `mod` 5 /= 0 && n `mod` 7 /= 0


-- uloha 6:
odblokuj :: [[Int]] -> [Int]
odblokuj [] = []
odblokuj zoz = nub (krok [] zoz)
        where krok :: [Int] -> [[Int]] -> [Int]
              krok vyst [] = vyst
              krok vyst zoz1 = let z0 = ((map last zoz1) \\ (map head zoz1)) in
                               let z = z0 ++ (map head (filter (\x -> elem (last x) z0) zoz1)) in 
                               krok (vyst ++ z) (filter (\x -> not (elem (head x) z && elem (last x) z)) zoz1)


-- uloha 8:
magickyStvorec :: [[Int]] -> Bool
magickyStvorec [] = True
magickyStvorec [[_]] = True
magickyStvorec zoz = riadky zoz && stlpce zoz && diag1 zoz 0 0 && diag2 zoz 0 0
          where n = sum (zoz!!0)
                len = length zoz - 1
                riadky :: [[Int]] -> Bool
                riadky [] = True
                riadky (x:xs) = if sum x == n then riadky xs else False
                stlpce :: [[Int]] -> Bool
                stlpce z | length (z!!0) == 1 = sum (concat z) == n
                         | otherwise = if sum (map head z) == n then stlpce (map tail z) else False
                diag1 :: [[Int]] -> Int -> Int -> Bool
                diag1 z index suma = if index > len then suma == n else diag1 z (index + 1) (suma + z!!index!!index)
                diag2 :: [[Int]] -> Int -> Int -> Bool
                diag2 z index suma = if index > len then suma == n else diag2 z (index + 1) (suma + z!!index!!(len - index))
