%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%[1 module Main
%%]

%%[1.data
data Tree  =  Tree_Leaf  Int
           |  Tree_Bin   Tree Tree
           deriving Show
%%]

%%[1.repmin
repmin ::  Tree -> Tree
repmin     t
  =  t'
  where  (t',tmin)                  =  r t tmin
         r (Tree_Leaf  i      )  m  =  (Tree_Leaf m        ,  i                )
         r (Tree_Bin   lt rt  )  m  =  (Tree_Bin  lt' rt'  ,  lmin `min` rmin  )
                                    where  (lt',lmin)  =  r lt m
                                           (rt',rmin)  =  r rt m
%%]

%%[1.tr
tr   = Tree_Bin (Tree_Leaf 3) (Tree_Bin (Tree_Leaf 4) (Tree_Leaf 5))
tr'  = repmin tr
%%]

%%[1.main
main  ::  IO ()
main  =   print tr'
%%]

