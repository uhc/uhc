let r1 = (| l1 = (\x -> x) |) in
let 
  r2 = \r -> (| l2 =  2 | r|)
  in let
     r3 = r2 (| l3= 3 |)
     in let
        x = \r -> (| l1 = 1, l2 = 2, l3 = 3 | r |)
	in x
