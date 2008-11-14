%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile output generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Output generation, on stdout or file

%%[8 module {%{EH}EHC.CompilePhase.Output}
%%]

-- general imports
%%[8 import(qualified EH.Util.FastSeq as Seq)
%%]
%%[8 import(qualified Data.Map as Map)
%%]

%%[8 import({%{EH}EHC.Common})
%%]
%%[8 import({%{EH}EHC.CompileUnit})
%%]
%%[8 import({%{EH}EHC.CompileRun})
%%]

%%[8 import(qualified {%{EH}Config} as Cfg)
%%]

-- HI syntax and semantics
%%[20 import(qualified {%{EH}HI} as HI, qualified {%{EH}HI.MainAG} as HISem)
%%]

-- Core output
%%[(8 codegen) import({%{EH}Core.Pretty})
%%]
-- Grin input and output
%%[(8 codegen grin) import({%{EH}GrinCode.Pretty})
%%]
-- Java output
%%[(8 codegen java) import({%{EH}Core.ToJava})
%%]

-- module admin
%%[20 import({%{EH}Module})
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(cpOutputCore)
cpOutputCore :: String -> HsName -> EHCompilePhase ()
cpOutputCore suff modNm
  =  do  {  cr <- get
         -- part 1: current .core
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbCore = ecuMbCore ecu
                 cMod   = panicJust "cpOutputCore" mbCore
                 fpC = fpathSetSuff suff fp                
         ;  when (ehcOptEmitCore opts) 
                 (do { cpMsg modNm VerboseALot "Emit Core"
                     ; lift $ putPPFile (fpathToStr (fpathSetSuff suff fp)) (ppCModule opts cMod) 100
                     })
         }
%%]

%%[(8 codegen java) export(cpOutputJava)
cpOutputJava :: String -> HsName -> EHCompilePhase ()
cpOutputJava suff modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbCore = ecuMbCore ecu
                 cMod   = panicJust "cpOutputJava" mbCore
                 (jBase,jPP) = cmodJavaSrc cMod
                 fpJ = fpathSetBase jBase fp                 
         ;  when (ehcOptEmitJava opts)
                 (do { cpMsg modNm VerboseALot "Emit Java"
                     ; lift (putPPFile (fpathToStr (fpathSetSuff suff fpJ)) jPP 100)
                     })
         }
%%]

%%[(8 codegen grin) export(cpOutputGrin)
cpOutputGrin :: String -> HsName -> EHCompilePhase ()
cpOutputGrin suff modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbGrin = ecuMbGrin ecu
                 grin   = panicJust "cpOutputGrin" mbGrin
                 grinPP = ppGrModule grin
                 fpG    = fpathSetSuff "grin" (fpathSetBase (fpathBase fp ++ suff) fp)
         ;  when (ehcOptEmitGrin opts)
                 (do { cpMsg modNm VerboseALot "Emit Grin"
                     ; lift $ putPPFile (fpathToStr fpG) grinPP 1000
                     })
         }
%%]

%%[(8 codegen grin) export(cpOutputByteCodeC)
cpOutputByteCodeC :: String -> HsName -> EHCompilePhase ()
cpOutputByteCodeC suff modNm
  =  do  {  cr <- get
         ;  let  (ecu,_,opts,fp) = crBaseInfo modNm cr
                 mbPP     = ecuMbBytecodeSem ecu
                 fpC      = fpathSetSuff suff fp
         ;  when (ehcOptEmitBytecode opts && isJust mbPP)
                 (do { cpMsg' modNm VerboseALot "Emit ByteCode C" Nothing fpC
                     ; lift $ putPPFile (fpathToStr fpC) (fromJust mbPP) 150
                     })
         }
%%]

%%[20 export(cpOutputHI)
cpOutputHI :: String -> HsName -> EHCompilePhase ()
cpOutputHI suff modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mmi    = panicJust "cpOutputHI.crsiModMp" $ Map.lookup modNm $ crsiModMp crsi
                 binds  = Seq.toList $ HI.hiFromHIInfo
                          $ ((ecuHIInfo ecu)
                               { HI.hiiExps       = mmiExps       mmi
                               , HI.hiiHiddenExps = mmiHiddenExps mmi
                               })
                 hi     = HISem.wrap_AGItf
                            (HISem.sem_AGItf
                              (HI.AGItf_AGItf $ HI.Module_Module modNm
                                $ HI.Binding_Stamp (Cfg.verTimestamp Cfg.version) (Cfg.verSig Cfg.version) (Cfg.verMajor Cfg.version) (Cfg.verMinor Cfg.version) (Cfg.verQuality Cfg.version) (Cfg.verSvn Cfg.version) (optsDiscrRecompileRepr opts) 0
                                  : binds))
                            (crsiHIInh crsi)
         ;  cpMsg modNm VerboseALot "Emit HI"
         ;  lift $ putPPFile (fpathToStr (fpathSetSuff suff fp)) (HISem.pp_Syn_AGItf hi) 120
         ;  now <- lift $ getClockTime
         ;  cpUpdCU modNm $ ecuStoreHITime now
         }

%%]


