% $Id: EHC.lag 199 2004-05-12 19:11:13Z andres $

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%[6_1 hs import (EHCommon, EHTy)
%%]


%%[6_1 hs import (Data.Maybe)
%%]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Predicate simplification
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[6_1
data Row = Empty | Cons String Ty Row | RowVar TyVarId TyVarCateg

toRow :: Ty -> Row
toRow r
  | isEmptyRow r = Empty
  | isRowExt r = let Just (l,t,r) = matchRowExt r
                 in Cons l t (toRow r)
  | isTyVar r = let (Ty_Var tv c) = r
		in RowVar tv c
  | otherwise = error $  "Invalid row:\n" ++ show r

simplifyPreds :: [Pred] -> [Pred]
simplifyPreds = concatMap simplify

simplify :: Pred -> [Pred]
simplify p = case p of
	       (Pred_Lacks l r) -> simplifyLacks l r
	       (Pred_Part r1 r2 r) -> simplifyPart r1 r2 r
	       (Pred_Knit ag nt i s) -> simplifyKnit ag nt i s

simplifyLacks l r
  | isEmptyRow r = []
  | isJust (matchRowExt r) = let Just (l',_,r') = matchRowExt r
			     in if l' /= l then simplifyLacks l r'
					   else error $ "Unresolvable constraint:\n" ++ show (Pred_Lacks l r)
  | isTyVar r = [Pred_Lacks l r]
  | otherwise = error $ "Wierd row:\n" ++ show r

simplifyPart r1 r2 r
  | isEmptyRow r1 = if equiv r2 r
		    then []
		    else error $ "Unresolvable constraint:\n" ++ show (Pred_Part r1 r2 r3)
  | isJust (matchRowExt r1) = let Just (l,t,r1') = matchRowExt r1
			      in
%%]