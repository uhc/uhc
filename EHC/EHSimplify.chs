% $Id: EHC.lag 199 2004-05-12 19:11:13Z andres $

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%[6_1 hs import (EHCommon, EHTy,EHCnstr,EHTyFitsIn)
%%]


%%[6_1 hs import (Data.Maybe)
%%]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Predicate simplification
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[6_1
--Row representation and to/from functions
data Row = Empty | Cons String Ty Row | RowVar TyVarId TyVarCateg

toRow :: Ty -> Row
toRow r
  | isEmptyRow r = Empty
  | isRowExt r = let Just (l,t,r) = matchRowExt r
                 in Cons l t (toRow r)
  | isTyVar r = let (Ty_Var tv c) = r
		in RowVar tv c
  | otherwise = error $  "Invalid row:\n" ++ show r

fromRow :: Row -> Ty
fromRow (Empty)       = tyEmptyRow
fromRow (Cons l ty r) = mkRowExt l ty (fromRow r)
fromRow (RowVar id c) = Ty_Var id c

predPart r1 r2 r = Pred_Part (fromRow r1) (fromRow r2) (fromRow r)
predLacks l r    = Pred_Lacks l (fromRow r)

-- Useful functions on rows
equivRow (Empty) (Empty) = True
equivRow (Cons l ty r) r' = elemRow l ty r' && equivRow r (remLab l r')
equivRow (RowVar a b) (RowVar c d) = a==c && b == d
equivRow _ _ = False

remLab l (Cons l' ty r)
  | l == l' = r
  | otherwise = Cons l' ty (remLab l r)
remLab l r = r

remLabs [] r = r
remLabs (l:ls) r = remLabs ls (remLab l r)

labels (Cons l _ r) = l : labels r
label _ = []

elemRow l ty (Empty) = False
elemRow l ty (Cons l' ty' r') 
  | l==l' && ty == ty' = True
  | l==l' && ty /= ty' = error $ "ElemRow: element type error:\n" ++ l ++ "\n" ++ show (ty,ty')
  | otherwise = elemRow l ty r'
elemRow l ty _ = False

--Predicate simplification for lacks and partition
simplify :: [Pred] -> [Pred]
simplify = concatMap simplifyPred

simplifyPred :: Pred -> [Pred]
simplifyPred p = case p of
	       (Pred_Lacks l r)    -> simplifyLacks l (toRow r)
	       (Pred_Part r1 r2 r) -> simplifyPart (toRow r1) (toRow r2) (toRow r)
	       
simplifyLacks l (Empty)        = []
simplifyLacks l (Cons l' ty r) = if l /= l' then simplifyLacks l r
					    else error $ "Unresolvable constraint:\n" ++ show (Pred_Lacks l (fromRow r))
simplifyLacks l r@(RowVar v c) = [predLacks l r]

simplifyPart (Empty) r2 r 
  | equivRow r2 r  = []
  | otherwise      = error $ "Unresolvable constraint:\n" ++ show (Pred_Part (fromRow Empty) (fromRow r2) (fromRow r))
simplifyPart r1@(Cons l ty r1') r2 r
  | elemRow l ty r = simplifyPred (predLacks l r2) ++ simplifyPart r1' r2 (remLab l r)
  | otherwise      = [predPart r1 r2 r]
simplifyPart r1@(RowVar v c) r2 r = [predPart r1 r2 r]


--Predicate improvement
improve :: [Pred] -> Cnstr
improve [] = emptyCnstr
improve (p:ps) = improvePred p |=> improve (improvePred p |=> ps)

improvePred   :: Pred -> Cnstr
improvePred p = case p of
                  (Pred_Lacks _ _)     -> emptyCnstr
		  (Pred_Part r1 r2 r)  -> improvePart (toRow r1) (toRow r2) (toRow r)

improvePart r1 r2 r 
  | disjoint r1 r2  = unify (fromRow (join r1 r2 (rowVar r))) (fromRow r)
  | otherwise       = emptyCnstr

rowVar (Cons _ _ r) = rowVar r
rowVar r@(RowVar _ _) = r
rowVar _ = error "Unresolvable constraint involving two row variables"

join (Empty) r' rv = r'
join (Cons l ty r) r' rv = Cons l ty (join r r' rv)
join r (Empty) rv = r
join r (Cons l ty r') rv = Cons l ty (join r r' rv)
join (RowVar _ _) (RowVar _ _) rv = rv 

disjoint (Empty) _ = True
disjoint (Cons l ty r) r' = not  (l `inRow` r') && disjoint r r'
disjoint (RowVar _ _) _ = True 

inRow l (Cons l' ty' r') 
  | l==l' = True
  | otherwise = inRow l r'
inRow l (Empty) = False
inRow l  (RowVar _ _) = False

unify t t' = foCnstr ( fitsIn strongFIOpts uidError t t')
uidError = error "EHSimplify:no uid"



%%]