%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint Handling Rules: Constraint language
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast) module {%{EH}CHR.Constraint} import({%{EH}Base.Common},{%{EH}Ty},{%{EH}CHR},{%{EH}CHR.Key},{%{EH}Base.Trie})
%%]

%%[(9 hmtyinfer || hmtyast) import(EH.Util.Pretty)
%%]

%%[(9 hmtyinfer || hmtyast) import(qualified Data.Set as Set,qualified Data.Map as Map)
%%]

%%[(20 hmtyinfer || hmtyast) import(Control.Monad, {%{EH}Base.Binary}, {%{EH}Base.Serialize})
%%]
%%[(20 hmtyinfer || hmtyast) import(Data.Typeable(Typeable,Typeable2), Data.Generics(Data))
%%]

%%[(20 hmtyinfer || hmtyast) import({%{EH}Base.CfgPP})
%%]

%%[(99 hmtyinfer || hmtyast) import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast) export(Constraint(..))
data Constraint p info
  = Prove      		{ cnstrPred :: !p }														-- proof obligation
  | Assume     		{ cnstrPred :: !p }														-- assumed
  | Reduction  		{ cnstrPred :: !p, cnstrInfo :: !info, cnstrFromPreds :: ![p] }			-- 'side effect', residual info used by (e.g.) codegeneration
  deriving (Eq, Ord, Show)
%%]

%%[(20 hmtyinfer || hmtyast)
deriving instance Typeable2 Constraint
deriving instance (Data x, Data y) => Data (Constraint x y)
%%]

%%[(9 hmtyinfer || hmtyast)
reducablePart :: Constraint p info -> Maybe (String,p,p->Constraint p info)
reducablePart (Prove  p) = Just ("Prf",p,Prove)
reducablePart (Assume p) = Just ("Ass",p,Assume)
reducablePart _          = Nothing
%%]

%%[(9 hmtyinfer || hmtyast)
instance Keyable p => Keyable (Constraint p info) where
  toKey c = maybe [] (\(s,p,_) -> TK_One TKK_Normal (Key_Str s) : toKey p) $ reducablePart c

instance (CHRMatchable env p s) => CHRMatchable env (Constraint p info) s where
  chrMatchTo env s c1 c2
    = do { (_,p1,_) <- reducablePart c1
         ; (_,p2,_) <- reducablePart c2
         ; chrMatchTo env s p1 p2
         }

instance (CHRSubstitutable p v s,CHRSubstitutable info v s) => CHRSubstitutable (Constraint p info) v s where
  chrFtv c
    = case reducablePart c of
        Just (_,p,_) -> chrFtv p
        _            -> Set.empty

  chrAppSubst s      (Prove     p     ) = Prove      (chrAppSubst s p)
  chrAppSubst s      (Assume    p     ) = Assume     (chrAppSubst s p)
  chrAppSubst s      (Reduction p i ps) = Reduction  (chrAppSubst s p) (chrAppSubst s i) (map (chrAppSubst s) ps)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Mapping: constraint -> info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast) export(ConstraintToInfoMap,emptyCnstrMp)
type ConstraintToInfoMap p info = Map.Map (Constraint p info) [info]

emptyCnstrMp :: ConstraintToInfoMap p info
emptyCnstrMp = Map.empty
%%]

%%[(9 hmtyinfer || hmtyast) export(cnstrMpFromList,cnstrMpUnion,cnstrMpUnions)
cnstrMpFromList :: (Ord p, Ord i) => [(Constraint p i,i)] -> ConstraintToInfoMap p i
cnstrMpFromList l = Map.fromListWith (++) [ (c,[i]) | (c,i) <- l ]

cnstrMpUnion :: (Ord p, Ord i) => ConstraintToInfoMap p i -> ConstraintToInfoMap p i -> ConstraintToInfoMap p i
cnstrMpUnion = Map.unionWith (++)

cnstrMpUnions :: (Ord p, Ord i) => [ConstraintToInfoMap p i] -> ConstraintToInfoMap p i
cnstrMpUnions = Map.unionsWith (++)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Rule
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast) export(CHRRule)
type CHRRule p g s info = CHR (Constraint p info) g s
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Observations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast) export(cnstrRequiresSolve)
cnstrRequiresSolve :: Constraint p info -> Bool
cnstrRequiresSolve (Reduction _ _ _) = False
cnstrRequiresSolve _                 = True
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer || hmtyast)
instance (PP p, PP info) => PP (Constraint p info) where
  pp (Prove     p     ) = "Prove"  >#< p
  pp (Assume    p     ) = "Assume" >#< p
  pp (Reduction p i ps) = "Red"    >#< p >#< "<" >#< i >#< "<" >#< ppBracketsCommas ps
%%]

%%[(20 hmtyinfer || hmtyast)
instance (PPForHI p, PPForHI info) => PPForHI (Constraint p info) where
  ppForHI (Prove     p     ) = "Prove"     >#< ppCurlysCommasBlock [ppForHI p]
  ppForHI (Assume    p     ) = "Assume"    >#< ppCurlysCommasBlock [ppForHI p]
  ppForHI (Reduction p i ps) = "Reduction" >#< ppCurlysCommasBlock [ppForHI p, ppForHI i, ppCurlysCommasBlock $ map ppForHI ps]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, ForceEval, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 hmtyinfer || hmtyast)
instance (ForceEval p, ForceEval info) => ForceEval (Constraint p info) where
  forceEval x@(Prove     p     ) | forceEval p `seq` True = x
  forceEval x@(Assume    p     ) | forceEval p `seq` True = x
  forceEval x@(Reduction p i ps) | forceEval p `seq` forceEval i `seq` forceEval ps `seq` True = x
%%[[102
  fevCount (Prove     p     ) = cm1 "Prove"     `cmUnion` fevCount p
  fevCount (Assume    p     ) = cm1 "Assume"    `cmUnion` fevCount p
  fevCount (Reduction p i ps) = cm1 "Reduction" `cmUnion` fevCount p `cmUnion` fevCount i `cmUnion` fevCount ps
%%]]
%%]

%%[(20 hmtyinfer || hmtyast)
instance (Serialize p, Serialize i) => Serialize (Constraint p i) where
  sput (Prove     a    ) = sputWord8 0 >> sput a
  sput (Assume    a    ) = sputWord8 1 >> sput a
  sput (Reduction a b c) = sputWord8 2 >> sput a >> sput b >> sput c
  sget = do t <- sgetWord8
            case t of
              0 -> liftM  Prove     sget
              1 -> liftM  Assume    sget
              2 -> liftM3 Reduction sget sget sget
%%]

