removeDup:: Eq a => [a] -> [a]
removeDup = foldl (\seen x -> if elem x seen
                                     then seen
                                     else seen ++ [x]) []
                                      
cartSucin :: (Eq a, Eq b,Eq c) =>  [a] -> [b] -> [c] -> [(a,b,c)]
cartSucin xs ys zs = 
          let x = [(x,y,z) | x<-xs, y <-ys,z<-zs]
          in  removeDup x
    
