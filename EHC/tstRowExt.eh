let
  g = \r ->  select_l2 r -- (extRec_l1 2 (extRec_l2 'c' r))
  
 -- r1 = extRec_l1 1 emptyRec
 -- r2 = g r1
 -- f = extRec_l1 1 (extRec_l2 2 emptyRec) 
in let f = g (extRec_l2 3 emptyRec)
   in f
