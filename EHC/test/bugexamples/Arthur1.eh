{-
20071129, Arthur:

dit is wat ik probeer
hugs keurt het af met een melding over ontsnappende existentieele types, en dat is maar goed ook
ik vroeg me af of ehc dit goedkeurt, omdat ehc ontsnappende dingen niet erg vindt
zou wel een probleem zijn denk ik


-}

data E s �= forall x . Lees x => �X ((s -> x) -> Int)
� � � � � | Y s
data A s = A (E s)


fun :: (forall s . Lees s => A s) -> Char
fun es =
� � let A e = es
� � in case e of
� � � � � X f -> k '1' (f i)
� � � � � Y s -> k '2' �(same (lees '1') s)


same :: a -> a -> a
same x y = x

i x = x
k x y = x

undef = undef

class Lees a where
� lees :: Char -> a

main = undef
