let data Tree = Leaf Int | Node Tree Tree 
    data Root = Root Tree in
  let a1 = aspect
            | (Leaf i) lhs.min   = i 
	    | (Node l r) lhs.min = 1
         in let f = knit a1 (Leaf 5)
            in f