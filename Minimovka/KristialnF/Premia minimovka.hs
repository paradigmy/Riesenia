cartSucin3::(Eq a,Eq b,Eq c)=>[a]->[b]->[c]->[(a, b, c)]
cartSucin3 a b c=duplikaty(a>>= \x->b>>= \y->c>>= \z->[(x,y,z)])
duplikaty::Eq a=>[a]->[a]
duplikaty=foldl(\z t->if(t`elem`z)==False then t:z else z)[]