let r1 = (| l1 = 1 |) in
let 
  r2 = \r -> (| l2 = 2 | r|)
  in let
     r3 = r2 (| l3 = 3 |)
     in 5