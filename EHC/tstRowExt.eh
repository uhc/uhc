let
  g  = extRec_l2 2 
  r1 = extRec_l1 1 emptyRec
  r2 = g r1
  f = extRec_l1 1 (extRec_l2 2 emptyRec) 
in g
