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
simplifyPreds :: [Pred] -> [Pred]
simplifyPreds = concatMap simplify

simplify :: Pred -> [Pred]
simplify p = case p of
	       (Pred_Lacks l r) -> simpLacks l r

simpLacks l r
  | isEmptyRow r = []
  | isJust (matchRowExt r) = let Just (l',_,r') = matchRowExt r
			     in if l' /= l then simpLacks l r'
					   else error $ "Unresolvable constraint:\n" ++ show (Pred_Lacks l r)
  | isTyVar r = [Pred_Lacks l r]
  | otherwise = error $ "Wierd row:\n" ++ show r
%%]