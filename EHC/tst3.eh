let data Tree = Empty | Leaf Int | Node Tree Tree in
  let a1 = aspect
            | (Leaf l) lhs.min  = \ag ->  5
	    | (Node r l) lhs.min = \ag -> 1
      a2 = aspect
     	    | (Empty) lhs.min = \ag -> 0
   in let a = join a1 a2
      	  in let f = knit
      in a