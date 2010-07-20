%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Grin transformation

%%[8 module {%{EH}EHC.CompilePhase.TransformGrin}
%%]

-- general imports
%%[8 import(qualified Data.Map as Map, qualified Data.Set as Set)
%%]

%%[8 import({%{EH}EHC.Common})
%%]
%%[8 import({%{EH}EHC.CompileUnit})
%%]
%%[8 import({%{EH}EHC.CompileRun})
%%]

-- Language syntax: Grin
%%[(8 codegen grin) import(qualified {%{EH}GrinCode} as Grin)
%%]
%%[(8 codegen grin) import({%{EH}GrinCode.GrinInfo})
%%]
-- Language syntax: Grin bytecode
%%[(8 codegen grin) import(qualified {%{EH}GrinByteCode} as Bytecode(tagAllowsUnboxedLife))
%%]
-- Output
%%[8 import({%{EH}EHC.CompilePhase.Output(cpOutputGrin)})
%%]

-- Grin transformations
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.UnusedMetaInfoElim}, {%{EH}GrinCode.Trf.UnusedNameElim}, {%{EH}GrinCode.Trf.AliasElim}, {%{EH}GrinCode.Trf.MayLiveUnboxed})
%%]
%%[(8 codegen grin) hs import({%{EH}GrinCode.Trf.ConstPropagation}, {%{EH}GrinCode.Trf.FlattenSeq}, {%{EH}GrinCode.Trf.EvalElim}, {%{EH}GrinCode.Trf.Inline})
%%]
%%[(8_2 codegen grin) hs import({%{EH}GrinCode.Trf.PrettyVarNames})
%%]

-- Heeeel veel Grin-transformaties
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.DropUnreachableBindings(dropUnreachableBindings)})
%%]
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.MemberSelect})
%%]
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.SimpleNullary})
%%]
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.CleanupPass})
%%]
%%[(97 codegen grin) import({%{EH}GrinCode.Trf.ConstInt(constInt)})
%%]
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.BuildAppBindings(buildAppBindings)})
%%]
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.GlobalConstants(globalConstants)})
%%]
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.Inline(grInline)})
%%]
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.FlattenSeq(grFlattenSeq)})
%%]
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.SetGrinInvariant(setGrinInvariant)})
%%]
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.CheckGrinInvariant})
%%]
%%[(9 codegen grin) import({%{EH}GrinCode.Trf.MergeInstance})
%%]
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.EvalStored})
%%]
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.ApplyUnited(applyUnited)})
%%]
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.SpecConst})
%%]
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.DropUnusedExpr(dropUnusedExpr)})
%%]
%%[(8 codegen grin) import({%{EH}GrinCode.PointsToAnalysis(heapPointsToAnalysis)})
%%]
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.InlineEA(inlineEA)})
%%]
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.DropDeadBindings(dropDeadBindings)})
%%]
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.EmptyAlts(emptyAlts)})
%%]
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.ImpossibleCase(impossibleCase)})
%%]
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.SingleCase(singleCase)})
%%]
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.CopyPropagation(copyPropagation)})
%%]
%%[(8 codegen grin) import({%{EH}GrinCode.Trf.NumberIdents(numberIdents)})
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: transformations, on grin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen grin)
cpMsgGrinTrf :: HsName -> String -> EHCompilePhase ()
cpMsgGrinTrf modNm m
  = do { cr <- get
       ; let (_,_,_,fp) = crBaseInfo modNm cr
       ; cpMsg' modNm VerboseALot "Local GRIN optim" (Just m) fp     -- '
       }

cpFromGrinTrf :: HsName -> (Grin.GrModule -> Grin.GrModule) -> String -> EHCompilePhase ()
cpFromGrinTrf modNm trf m
  = do { cr <- get
       ; let (ecu,_,_,fp) = crBaseInfo modNm cr
       ; cpMsgGrinTrf modNm m
       ; cpUpdCU modNm $ ecuStoreGrin $ trf $ fromJust $ ecuMbGrin ecu
       }

cpIterGrinTrf :: HsName -> (Grin.GrModule -> (Grin.GrModule, Bool)) -> String -> EHCompilePhase ()
cpIterGrinTrf modNm trf m = do
    cr <- get
    let (_,_,_,fp) = crBaseInfo modNm cr
    cpMsg' modNm VerboseALot "Local GRIN optim (iterated)" (Just m) fp
    i <- caFixCount 1
    cpMsg' modNm VerboseALot ("  done in " ++ show i ++ " iteration(s)") (Just m) fp
  where caFixCount n = do
          cr <- get
          let (ecu,_,_,_) = crBaseInfo modNm cr
          let code = fromJust $ ecuMbGrin ecu
          (code, changed) <- return $ trf code
          cpUpdCU modNm $ ecuStoreGrin $ code
          if changed then (caFixCount $ n+1) else return n

-- Op (operation) is an ad-hoc generalisation of functions that take some InfoX
-- and a GrModule en return some result and new InfoX. For example
-- transformations, that have a GrModule as a result.
cpFullGrinInfoOp :: HsName -> GrinInfoPart i -> ([i] -> Grin.GrModule -> (result, i)) -> String -> EHCompilePhase result
cpFullGrinInfoOp modNm inf op m
  = do { cr <- get
       ; imps <- allImports modNm
       ; let (ecu,_,_,fp) = crBaseInfo modNm cr
       ; cpMsg' modNm VerboseALot "Full GRIN optim" (Just m) fp -- TODO maybe a strange msg for non-transformations
       ; let grin = fromJust $ ecuMbGrin ecu
       ; let sem  = fromJust $ ecuMbGrinSem ecu
       ; let (res, nws) = op (map (impSem inf cr) imps) grin
       ; let sem' = grinInfoUpd inf nws sem
       ; cpUpdCU modNm (ecuStoreGrinSem sem')
       ; return res
       }
  where
    impSem inf cr nm =
      let (ecu,_,_,_) = crBaseInfo nm cr
      in  fromJust (ecuMbGrinSem ecu >>= grinInfoGet inf)

-- Transformations are specific operations that store their result in the
-- compiler state.
cpFullGrinInfoTrf :: HsName -> GrinInfoPart i -> ([i] -> Grin.GrModule -> (Grin.GrModule, i)) -> String -> EHCompilePhase ()
cpFullGrinInfoTrf modNm inf trf m
  = do {
       ; grTrf <- cpFullGrinInfoOp modNm inf trf m
       ; cpUpdCU modNm (ecuStoreGrin grTrf)
       }
%%]

%%[(8 codegen grin) export(cpTransformGrin)
allImports :: HsName -> EHCompilePhase [HsName]
allImports modNm = do
  cr <- get
  let (ecu,_,_,_) = crBaseInfo modNm cr
  let imps        = ecuImpNmL ecu
  rec <- mapM allImports imps
  return $ nub (concat (imps : rec))

cpTransformGrin :: HsName -> EHCompilePhase ()
cpTransformGrin modNm
  =  do  {  cpUpdCU modNm (ecuStoreGrinSem emptyGrinInfo) -- temporary
         ;  cr <- get
         ;  imports    <- allImports modNm
         ;  let  (ecu,_,opts,_) = crBaseInfo modNm cr
                 fullProg    = ehcOptFullProgAnalysis opts
                 forBytecode = not fullProg
                 optimizing  = ehcOptOptimizationLevel opts >= OptimizationLevel_Normal
         
{- for debugging 
                 trafos  =     mk [mte,unb,flt,cpr,nme]
-}
                 trafos  =     (if forBytecode               then mk [metaElim, unbox]            else [])
                           ++  (if optimizing                then mk evel                         else mk [flatten])
                           ++  (if forBytecode && optimizing then inline : mk (evel++[constProp]) else [])
                           ++  (if optimizing                then mk [nameElim]                   else [])
                           ++  (if fullProg                  then grPerModuleFullProg modNm       else [])
                   -- NOTE the transformations here have always worked on
                   -- separate modules, so no need to look at them for
                   -- incrementalization.
                   where mk            = map mk1
                         mk1 (trf,msg) = (cpFromGrinTrf modNm trf msg, msg)

                         flatten    = ( grFlattenSeq                   , "flatten"          )
                         aliasElim  = ( grAliasElim                    , "alias elim"       )
                         nameElim   = ( grUnusedNameElim               , "unused name elim" )
                         evalElim   = ( grEvalElim opts                , "eval elim"        )
                         metaElim   = ( grUnusedMetaInfoElim           , "meta info elim"   )
                         constProp  = ( grConstPropagation             , "const prop"       )
                         unbox      = ( grMayLiveUnboxed (Bytecode.tagAllowsUnboxedLife opts)
                                                                 , "unbox"            )
%%[[8_2
                         uniform    = ( grPrettyNames                  , "rename uniform"   ) 
%%]]
%%[[8
                         evel = [ flatten, aliasElim, evalElim, flatten, aliasElim ]
%%][8_2
                         evel = [ flatten, aliasElim, uniform, evalElim, flatten, aliasElim ]
%%]]
%%[[8                              
                         inline = mk1 ( grInline True , "inline" )
%%][20                                
                         inline = ( do { cr <- get
                                       ; let (ecu,crsi,_,_) = crBaseInfo modNm cr
                                             expNmOffMp     = crsiExpNmOffMp modNm crsi
                                             optim          = crsiOptim crsi
                                             (g,gathInlMp)  = grInline True (Map.keysSet expNmOffMp) (optimGrInlMp optim) $ fromJust $ ecuMbGrin ecu
                                       ; cpMsgGrinTrf modNm "inline"
                                       ; cpUpdCU modNm (ecuStoreOptim (defaultOptim {optimGrInlMp = gathInlMp}) . ecuStoreGrin g)
                                       }
                                  , "inline" 
                                  ) 
%%]]                              
                              
                 optGrinNormal = map fst trafos
                 optGrinDump   = out 0 "from core" : concat [ [o,out n nm] | (n,(o,nm)) <- zip [1..] trafos ]
                        where out n nm = cpOutputGrin False ("-0" ++ show (10+n) ++ "-" ++ filter isAlpha nm) modNm
         ;  when (isJust $ ecuMbGrin ecu)
                 (cpSeq (if ehcOptDumpGrinStages opts then optGrinDump else optGrinNormal))
         
         -- -- print GrinInfo:
         -- ; cr <- get
         -- ; let (ecu,_,_,fp) = crBaseInfo modNm cr
         -- ; cpMsg' modNm VerboseALot (show (ecuMbGrinSem ecu)) (Just "aap") fp
         }


grPerModuleFullProg :: HsName -> [(EHCompilePhase (), String)]
grPerModuleFullProg modNm = trafos1 ++ invariant 0 ++ grSpecialize modNm ++ [dropUnreach] ++ invariant 1 -- ++ [hptAnalysis]
  where
    trafos1 =
      [ dropUnreach
%%[[9
      , full  grMergeInstance   "MergeInstance"   grinInfoMergeInstance
      , full  grMemberSelect    "MemberSelect"    grinInfoMemberSelect
    
      , dropUnreach
%%]]
      , full  grCleanupPass     "CleanupPass"     grinInfoCleanupPass
      , full  grSimpleNullary   "SimpleNullary"   grinInfoSimpleNullary
%%[[97
      , once constInt           "ConstInt"
%%]]
      , once buildAppBindings   "BuildAppBindings"
      , once globalConstants    "GlobalConstants"
%%[[8
      , once grInline False     "Inline"
%%][20
      , once (fst . grInline False Set.empty Map.empty) "Inline"
%%]]
      , once grFlattenSeq       "Flatten"
    
      , once singleCase         "singleCase"
      , once grFlattenSeq       "Flatten"
    
      , once setGrinInvariant   "SetGrinInvariant"
      ]

    dropUnreach = ( once id {- dropUnreachableBindings False -} "DropUnreachableBindings stub" )
    invariant i = [once setGrinInvariant "SetGrinInvariant", (checkInvariant i, "CheckGrinInvariant")]

    checkInvariant i =
      do { errors <- cpFullGrinInfoOp modNm (grinInfoCheckInvariantSpec i) checkGrinInvariant "CheckGrinInvariant"
         ; when (not (null errors)) (error (unlines errors))
         }
    
    -- hptAnalysis   = cpFullGrinInfoOp modNm (grinInfoHptAnalysis) (heapPointsToAnalysis modNm) "Partial HPT analysis"

    mk            = map mk1
    mk1 (trf,msg) = (cpFromGrinTrf modNm trf msg, msg)
    
    once trf m = (cpFromGrinTrf modNm trf m, m)
    iter trf m = (cpIterGrinTrf modNm trf m, m)
    full trf m i = (cpFullGrinInfoTrf modNm i trf m, m)
    

-- grSpecialize :: [(Grin.GrModule -> Grin.GrModule, String)]
grSpecialize modNm = concatMap (grSpecialize' modNm) [0..5]
grSpecialize' modNm pass =
    [ full grEvalStored                      "eval stored"    grinInfoEvalStoredSpec
    , once applyUnited                       "apply united"
    , once grFlattenSeq                      "flatten"
    -- , iter dropUnusedExpr                    "drop unused"
    , full grSpecConst                       "spec const"     grinInfoSpecConstSpec
    , iter copyPropagation                   "copy prop"
    , once singleCase                        "single case"
    , once grFlattenSeq                      "flatten"
    , full grSimpleNullary                   "simply nullary" grinInfoSimpleNullarySpec
    , full grMemberSelect                    "member select"  grinInfoMemberSelectSpec
    -- , once (dropUnreachableBindings False)   "drop unreachable"
    ]
  where once trf m = (cpFromGrinTrf modNm trf m, m)
        iter trf m = (cpIterGrinTrf modNm trf m, m)
        full trf m i = (cpFullGrinInfoTrf modNm (i pass) trf m, m)

%%]

