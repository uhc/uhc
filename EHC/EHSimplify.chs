% $Id: EHC.lag 199 2004-05-12 19:11:13Z andres $

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%[6_1 hs import (EHCommon, EHTy,EHCnstr,EHTyFitsIn)
%%]


%%[6_1 hs import (Data.Maybe, Debug.Trace)
%%]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Predicate simplification
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[6_1
--Row representation and to/from functions
data Row = Empty | Cons Label Ty Row | RowVar TyVarId TyVarCateg deriving Show

toRow :: Ty -> Row
toRow r
  | isEmptyRow r = Empty
  | isRowExt r = let Just (l,t,r') = matchRowExt r
                 in Cons l t (toRow r')
  | isTyVar r = let (Ty_Var tv c) = r
		in RowVar tv c
  | isQuantTy r = toRow (stripQuant r)
  | otherwise = error $  "Invalid row:\n" ++ show r

fromRow :: Row -> Ty
fromRow (Empty)       = tyEmptyRow
fromRow (Cons l ty r) = mkRowExt l ty (fromRow r)
fromRow (RowVar id c) = Ty_Var id c

predPart r1 r2 r = Pred_Part (fromRow r1) (fromRow r2) (fromRow r)
predLacks l r    = Pred_Lacks l (fromRow r)
predKnit ag nt inh syn = Pred_Knit (fromRow ag) nt (fromRow inh) (fromRow syn)

-- Useful functions on rows
equivRow (Empty) (Empty) = True
equivRow (Cons l ty r) r' = elemRow l ty r' && equivRow r (remLab l r')
equivRow (RowVar a b) (RowVar c d) = a==c && b == d
equivRow _ _ = False

remLab l (Cons l' ty r)
  | l == l' = r
  | otherwise = Cons l' ty (remLab l r)
remLab l r = error ("EHSimplify.remLab.hs")

elemRow l ty (Empty) = False
elemRow l ty (Cons l' ty' r') 
  | l==l' && ty == ty' = True
  | l==l' && ty /= ty' = error $ "ElemRow: element type error:\n" ++ show l ++ "\n" ++ show (ty,ty')
  | otherwise = elemRow l ty r'
elemRow l ty _ = False

disjoint (Empty) _        =  True
disjoint (Cons l ty r) r' =  not  (l `inRow` r') && disjoint r r'
disjoint (RowVar _ _) _   =  True 

inRow l (Cons l' ty' r') 
  | l==l'              = True
  | otherwise          = inRow l r'
inRow l (Empty)        = False
inRow l (RowVar _ _)   = False

isEmpty (Empty) = True
isEmpty _	= False

isVar (RowVar _ _) = True
isVar _ = False

--Predicate simplification and improvement
simplifyImprove synPred uq preds cnstr =
  let (impCnstrs,uq') = improve uq preds
      subPreds = impCnstrs |=> preds
      (simpPreds,uq'') = simplify synPred uq' subPreds
  in if simpPreds == preds 
     then (simpPreds, cnstr |=> impCnstrs) 
     else trace ("simplify and improve") simplifyImprove synPred uq'' simpPreds (cnstr |=> impCnstrs)


--Predicate simplification for lacks and partition
simplify synPred uq [] = ([],uq)
simplify synPred uq (p:ps) = let (simpP,uq') = simplifyPred synPred uq p
				 (recurse,finalUnq) = simplify synPred uq' ps
				 preds = trace (show (simpP ++ recurse)) (simpP ++ recurse)
			     in (simpP ++ recurse,finalUnq)

simplifyPred synPred uq p = 
  case p of
    (Pred_Lacks l r)    -> (simplifyLacks l (toRow r),uq)
    (Pred_Part r1 r2 r) -> (simplifyPart (toRow r1) (toRow r2) (toRow r),uq)
    (Pred_Knit ag nt inh syn) -> simplifyKnit synPred uq (toRow ag) nt (toRow inh) (toRow syn)
    (Pred_Syns d attr ty nt ) -> ([p],uq)
    (Pred_Inhs d attr ty nt) -> ([p],uq)
	       
simplifyLacks l (Empty)        = []
simplifyLacks l (Cons l' ty r) = if l /= l' then simplifyLacks l r
					    else error $ "Unresolvable constraint:\n" ++ show (Pred_Lacks l (fromRow r))
simplifyLacks l r@(RowVar v c) = [predLacks l r]

simplifyPart r1 r2 (Empty) 
  | isEmpty r1 && isEmpty r2  = []
  | isVar r1 || isVar r2 = [predPart r1 r2 Empty]
  | otherwise      = error $ "Unresolvable constraint:\n" ++ show (Pred_Part (fromRow r1) (fromRow r2) (fromRow Empty))
simplifyPart r1 r2 row@(Cons l ty r)
  | elemRow l ty r1 = simplifyLacks l r2 ++ simplifyPart (remLab l r1) r2 r
  | elemRow l ty r2 = simplifyLacks l r1 ++ simplifyPart r1 (remLab l r2) r
  | otherwise       = [predPart r1 r2 row]
simplifyPart r1 r2 r@(RowVar _ _) = [predPart r1 r2 r]

simplifyKnit synPred uq ag nt inh syn = case ag of
  (Empty) -> if isEmpty inh && isEmpty syn
	     then ([],uq)
	     else  ([predKnit ag nt inh syn],uq)
  (RowVar v c) ->  ([predKnit ag nt inh syn],uq)
  (Cons (Aspect prod Syn attr) ty r) -> 
                   let (defRow,nt') = mkDefRow (Aspect prod Syn attr) ty synPred
		       (nextUID,restAGVar) = mkNewUID uq
		       (nextUID',restSynVar)  = mkNewUID nextUID
		       restAG = RowVar restAGVar TyVarCateg_Plain
		       restSyn = if nt == nt' then RowVar restSynVar TyVarCateg_Plain else syn
		       singleSyn = Cons (Label attr) ty Empty
		       (recurse,finalUID) = simplifyKnit synPred nextUID' restAG nt inh restSyn
		       recurseSyn = if nt == nt' then simplifyPart singleSyn restSyn syn else []
		   in (simplifyPart defRow restAG ag ++ recurseSyn ++ recurse, finalUID)

mkDefRow (Aspect prod Syn attr) ty synPred =
  let (nt,ps) = head (filter ( \ (nt,ps) -> prod `elem` ps) synPred)
  in (foldr (\l r -> Cons l ty r) Empty (map (\p -> Aspect p Syn attr) ps), hnmToTyCon nt)
mkDefRow _ _ _= error "not finished yet"

--Predicate improvement
improve :: UID -> [Pred] -> (Cnstr,UID)
improve unq []      =  (emptyCnstr,unq)
improve u (p:ps)    =  let (cnstr,unq) = improvePred u p
			   (subst,unq') = improve unq (cnstr |=> ps)
		       in (subst |=> cnstr,unq')

improvePred :: UID -> Pred -> (Cnstr,UID)
improvePred u p = case p of
                  (Pred_Lacks _ _)     -> (emptyCnstr,u)
	          (Pred_Part r1 r2 r)  -> improvePart u (toRow r1) (toRow r2) (toRow r)
	          (Pred_Knit ag nt inh syn) -> improveKnit u (toRow ag) nt (toRow inh) (toRow syn)
		  t			    -> (emptyCnstr,u)

improvePart u r1 r2 r 
  | disjoint r1 r2 = let (uid,nextUid) = mkNewUID u
		 	 joinedRows = join r1 r2 (RowVar uid TyVarCateg_Plain)
			 fo	    = unify nextUid (fromRow joinedRows) (fromRow r)
		     in  (foCnstr fo, foUniq fo)
  | otherwise = error $ "Unresolvable constraint: " ++ show (r1,r2,r) --(emptyCnstr,u)

improveKnit u Empty nt inh syn  =
  let fo = unify u (fromRow Empty) (fromRow (inh))
      fo' = unify (foUniq fo) (foCnstr fo |=> (fromRow (syn))) (fromRow Empty)
  in (foCnstr fo' |=> foCnstr fo,foUniq fo')
improveKnit u ag nt inh syn = let attrDefs = groupAttrs [] ag 
				  (cnstrs,unq) = unifyAttrTys u (map snd attrDefs)
                              in  (cnstrs,unq)

rowVar (Cons _ _ r)    = rowVar r
rowVar r@(RowVar _ _)  = r
rowVar _               = error "Unresolvable constraint involving two row variables"

join (Empty) r' rv                = r'
join (Cons l ty r) r' rv	  = Cons l ty (join r r' rv)
join r (Empty) rv		  = r
join r (Cons l ty r') rv	  = Cons l ty (join r r' rv)
join (RowVar _ _) (RowVar _ _) rv = trace "used fresh row variable\n" rv

groupAttrs g Empty = g
groupAttrs g (RowVar _ _) = g
groupAttrs g (Cons l ty r) = groupAttrs (add l ty g) r

add l ty [] = [(l,[ty])]
add l ty ((l',tys) : rest) = if sameAttr l l' then (l',ty:tys) : rest
					      else (l',tys) : add l ty rest

unifyAttrTys u [] = (emptyCnstr,u)
unifyAttrTys u (tys:tyss) = let (cnstr,unq) = unifyL u tys 
				(cnstrs,unq') = unifyAttrTys unq tyss 
			    in (cnstrs |=> cnstr,unq')

sameAttr a1 a2 = getAttrName a1 == getAttrName a2
sameNT = undefined --fix this

unify u t t' =  fitsIn strongFIOpts u t t'
unifyL u [] = (emptyCnstr,u)
unifyL u [x] = (emptyCnstr,u)
unifyL u (t1:t2:ts) = let fo = unify u t1 t2
			  resTy = foTy fo
			  (cnstrs,unq) = unifyL (foUniq fo) (resTy:ts)
		      in (cnstrs |=> foCnstr fo, unq) 
		      
uidError = error "EHSimplify:no uid"

%%]