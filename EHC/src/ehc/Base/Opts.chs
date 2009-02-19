%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Options of all sorts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.Opts} import(System.Console.GetOpt,{%{EH}Base.Common}) export(EHCOpts(..), defaultEHCOpts, ehcCmdLineOpts)
%%]

%%[4 import(EH.Util.Pretty)
%%]

%%[(4 hmtyinfer || hmtyast) import({%{EH}Ty})
%%]

%%[8 import(Data.List,Data.Char,qualified Data.Map as Map,{%{EH}Base.Builtin})
%%]

%%[8 import(EH.Util.FPath)
%%]
%%[8 import({%{EH}EHC.Environment})
%%]

%%[(8 codegen) import({%{EH}Base.Target})
%%]

%%[9 import(qualified Data.Set as Set)
%%]

%%[8 import(EH.Util.Utils)
%%]

%%[50 import({%{EH}Ty.Trf.Instantiate})
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Option after which its handling the compiler quits immediately
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(ImmediateQuitOption(..))
data ImmediateQuitOption
  = ImmediateQuitOption_Help								-- print help
  | ImmediateQuitOption_Version								-- print version info
  | ImmediateQuitOption_Meta_Variant						-- print variant number
  | ImmediateQuitOption_Meta_Targets						-- print all codegeneration targets (empty if no codegen)
  | ImmediateQuitOption_Meta_TargetDefault					-- print the default codegeneration target (dummy if no codegen)
%%[[99
  | ImmediateQuitOption_NumericVersion						-- print numerical version, for external version comparison
  | ImmediateQuitOption_Meta_ExportEnv (Maybe String)		-- export (write) environmental info of installation
  | ImmediateQuitOption_Meta_DirEnv 						-- print dir of environmental info of installation
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Transformation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(cmdLineTrfs)
data TrfOpt = TrfYes String | TrfNo String | TrfAllYes | TrfAllNo

cmdLineTrfs :: AssocL String String
cmdLineTrfs
  = [ ("CER"    , "Core Eta Reduction")
    , ("CETA"   , "Core Eliminate Trivial Applications")
    , ("CCP"    , "Core Constant Propagation (simple ones introduced by frontend)")
    , ("CRU"    , "Core Rename Unique (all identifiers)")
    , ("CLU"    , "Core Let Unrec (remove unnecessary recursive defs)")
    , ("CILA"   , "Core Inline Let Alias (remove unnecessary alpha renamings)")
    , ("CFL"    , "Core Full Laziness (give names to all expressions and float them outwards)")
    , ("CLL"    , "Core Lambda Lift")
    , ("CLGA"   , "Core Lambda Global as Arg")
    , ("CCGA"   , "Core CAF Global as Arg")
    , ("CLFG"   , "Core Lambda Float to Global")
%%[[9
    , ("CLDF"   , "Core Lift Dictionary Fields")
%%]]
%%[[102
    , ("CS"     , "Core Strip (debug)")
%%]]
    ]
%%]

%%[(8 codegen) export(trfOptOverrides)
trfOptOverrides :: [TrfOpt] -> String -> Maybe Bool
trfOptOverrides opts trf
  =  ovr opts
  where  ovr [] = Nothing
         ovr (TrfYes s   :os) | trf == s  = Just True
         ovr (TrfNo s    :os) | trf == s  = Just False
         ovr (TrfAllYes  :os)             = Just True
         ovr (TrfAllNo   :os)             = Just False
         ovr (_          :os)             = ovr os
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
mkSearchPath :: String -> [String]
mkSearchPath = wordsBy (`elem` ";,")
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compiler options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Convention: most option names/fields start with 'ehcOpt'

%%[1.EHCOpts
data EHCOpts
  = EHCOpts
      {  ehcOptAspects        ::  String            -- which aspects are included in this compiler
      ,  ehcOptShowHS         ::  Bool              -- show HS pretty print on stdout
      ,  ehcOptShowEH         ::  Bool              -- show EH pretty print on stdout
      ,  ehcOptPriv           ::  Bool				-- privately used (in general during switch between 2 impls of 1 feature)
%%[[1
      ,  ehcOptShowAst        ::  Bool              -- show decorated EH AST on stdout
%%][100
%%]]
%%[[(1 hmtyinfer)
      ,  ehcOptShowTopTyPP    ::  Bool              -- show EH type of expression
%%]]
      ,  ehcOptImmQuit        ::  Maybe ImmediateQuitOption
      ,  ehcOptDebug          ::  Bool              -- debug info
      ,  ehcStopAtPoint       ::  CompilePoint      -- stop at (after) compile phase
%%[[7_2
      ,  ehcOptUniqueness     ::  Bool
%%]]
%%[[(8 codegen)
      -- ,  ehcOptEmitCore       ::  Bool
      ,  ehcOptOptimise       ::  Optimise			-- optimisation level
      ,  ehcOptDumpCoreStages ::  Bool				-- dump intermediate Core transformation stages
      ,  ehcOptTrf            ::  [TrfOpt]
      ,  ehcOptTarget         ::  Target			-- code generation target
%%]]
%%[[(8 codegen grin)
      ,  ehcOptTimeCompile    ::  Bool

      ,  ehcOptGenCaseDefault ::  Bool
      ,  ehcOptOwn            ::  Int
      ,  ehcOptGenCmt         ::  Bool
      ,  ehcOptGenDebug       ::  Bool				-- generate runtime debug info
      ,  ehcOptGenTrace       ::  Bool

      ,  ehcOptGenRTSInfo     ::  Int				-- flags to tell rts to dump internal info, currently: 1=on
      ,  ehcOptDumpGrinStages ::  Bool				-- dump intermediate Grin transformation stages
%%]]
%%[[(8 codegen java)
      ,  ehcOptEmitJava       ::  Bool
%%]]
%%[[8
      ,  ehcOptEmitHS         ::  Bool
      ,  ehcOptEmitEH         ::  Bool
      ,  ehcOptUsrSearchPath  ::  [String]
      ,  ehcOptVerbosity      ::  Verbosity			-- verbosity level

      ,  ehcOptBuiltinNames   ::  EHBuiltinNames
      ,  ehcOptUseInplace     ::  Bool              -- use inplace runtime libraries
      ,  ehcOptEnvironment    ::  EHCEnvironment	-- runtime environment
      
%%]]
%%[[9
      ,  ehcCfgInstFldHaveSelf::  Bool				-- functions/fields of instance get as arg the dictionary as well
      ,  ehcOptPrfCutOffAt    ::  Int				-- cut off limit for context reduction
      ,  ehcCfgClassViaRec    ::  Bool				-- instance representation via record instead of data
      -- ,  ehcCfgCHRScoped      ::  CHRScoped			-- how to gen scoped CHR's (option is used only for paper writing + experimenting)
%%]]
%%[[11
      ,  ehcOptTyBetaRedCutOffAt					-- cut off for type lambda expansion
                              ::  Int
%%]]
%%[[(20 codegen)
      ,  ehcDebugStopAtCoreError
                              ::  Bool              -- stop when Core parse error occurs (otherwise errors are ignored, repaired .core is used)
%%]]
%%[[20
      ,  ehcOptCheckRecompile ::  Bool
      ,  ehcDebugStopAtHIError::  Bool              -- stop when HI parse error occurs (otherwise it is ignored, .hi thrown away)
      ,  ehcOptDoLinking      ::  Bool				-- do link, if False compile only
%%]]
%%[[(99 hmtyinfer)
      ,  ehcOptEmitDerivTree  ::  DerivTreeWay      -- show derivation tree on stdout
      ,  ehcOptEmitDerivTreePaperSize
      						  ::  String            -- the paper size to be used
      ,  ehcOptEmitDerivFitsIn
      						  ::  Bool              -- show fitsIn derivation tree as well
%%]]
%%[[99
      ,  ehcOptLibSearchPath  ::  [String]
      ,  ehcOptLibPackages    ::  [String]
      ,  ehcProgName          ::  FPath  			-- name of this program
      ,  ehcOptCPP            ::  Bool				-- do preprocess with C preprecessor CPP
      ,  ehcOptUseAssumePrelude						-- use & assume presence of prelude
                              ::  Bool
%%]]
      }

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Derived options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

These are there for (temporary) backwards compatibility.

%%[(8 codegen grin) export(ehcOptFullProgAnalysis)
-- do full GRIN program analysis
ehcOptFullProgAnalysis :: EHCOpts -> Bool
ehcOptFullProgAnalysis = targetIsFullProgAnal . ehcOptTarget
%%]

%%[(8 codegen grin) export(ehcOptErrAboutBytecode)
-- report when Grin ByteCode errors occur
ehcOptErrAboutBytecode :: EHCOpts -> Bool
%%[[8
ehcOptErrAboutBytecode _ = False
%%][99
ehcOptErrAboutBytecode   = targetIsGrinBytecode . ehcOptTarget
%%]]
%%]

%%[(8 codegen grin) export(ehcOptEmitExecBytecode, ehcOptEmitBytecode)
-- generate bytecode
ehcOptEmitExecBytecode :: EHCOpts -> Bool
ehcOptEmitExecBytecode = targetIsGrinBytecode . ehcOptTarget

ehcOptEmitBytecode :: EHCOpts -> Bool
ehcOptEmitBytecode = ehcOptEmitExecBytecode
%%]

%%[(8 codegen grin) export(ehcOptEmitC)
-- generate C
ehcOptEmitC :: EHCOpts -> Bool
ehcOptEmitC = targetIsC . ehcOptTarget
%%]

%%[(8 codegen grin) export(ehcOptEmitLLVM)
-- generate LLVM
ehcOptEmitLLVM :: EHCOpts -> Bool
ehcOptEmitLLVM = targetIsLLVM . ehcOptTarget
%%]

%%[(8 codegen grin) export(ehcOptEmitCLR)
-- generate CLR
ehcOptEmitCLR :: EHCOpts -> Bool
ehcOptEmitCLR = targetIsCLR . ehcOptTarget
%%]

%%[(8 codegen) export(ehcOptEmitCore)
-- generate Core
ehcOptEmitCore :: EHCOpts -> Bool
ehcOptEmitCore opts
  = ehcOptFullProgAnalysis opts || targetIsCore (ehcOptTarget opts)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Default compiler options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.defaultEHCOpts
defaultEHCOpts
  = EHCOpts
      {  ehcOptAspects          =   "%%@{%{ASPECTS}%%}"
      ,  ehcOptShowHS           =   False
      ,  ehcOptPriv             =   False
%%[[1
      ,  ehcOptShowEH           =   True
%%][99
      ,  ehcOptShowEH           =   False
%%]]
%%[[1
      ,  ehcOptShowAst          =   False
%%][100
%%]]
%%[[(1 hmtyinfer)
      ,  ehcOptShowTopTyPP      =   False
%%]]
      ,  ehcOptImmQuit			=	Nothing
      ,  ehcOptDebug            =   False
      ,  ehcStopAtPoint         =   CompilePoint_All
%%[[7_2
      ,  ehcOptUniqueness       =   True
%%]]
%%[[(8 codegen)
      ,  ehcOptDumpCoreStages   =   False
      ,  ehcOptOptimise         =   OptimiseNormal
      ,  ehcOptTrf              =   []
      ,  ehcOptTarget			=	defaultTarget
%%]]
%%[[(8 codegen grin)
      ,  ehcOptTimeCompile      =   False

      ,  ehcOptGenCaseDefault   =   False
      ,  ehcOptOwn              =   3
      ,  ehcOptGenDebug         =   True
      ,  ehcOptGenTrace         =   False
      ,  ehcOptGenRTSInfo       =   0

      ,  ehcOptDumpGrinStages   =   False
%%]]
%%[[(8 codegen java)
      ,  ehcOptEmitJava         =   False
%%]]
%%[[8
      ,  ehcOptEmitHS           =   False
      ,  ehcOptEmitEH           =   False
      
      ,  ehcOptUsrSearchPath    =   []
      ,  ehcOptVerbosity        =   VerboseNormal
      ,  ehcOptBuiltinNames     =   mkEHBuiltinNames (const id)
      ,  ehcOptUseInplace       =   True
      ,  ehcOptEnvironment      =	undefined	-- filled in at toplevel
      
%%]]
%%[[(8 codegen grin)
      ,  ehcOptGenCmt           =   True
%%][(99 codegen grin)
      ,  ehcOptGenCmt           =   False
%%]]
%%[[9
      ,  ehcCfgInstFldHaveSelf  =   False
      ,  ehcOptPrfCutOffAt      =   20
      ,  ehcCfgClassViaRec      =   False -- True
      -- ,  ehcCfgCHRScoped     =   CHRScopedAll
%%]]
%%[[11
      ,  ehcOptTyBetaRedCutOffAt
                                =   10
%%]]
%%[[(20 codegen)
      ,  ehcDebugStopAtCoreError=   False
%%]]
%%[[20
      ,  ehcOptCheckRecompile   =   True
      ,  ehcDebugStopAtHIError  =   False
      ,  ehcOptDoLinking        =   True
%%]]
%%[[(99 hmtyinfer)
      ,  ehcOptEmitDerivTree	=	DerivTreeWay_None
      ,  ehcOptEmitDerivTreePaperSize
      						    =   "2"
      ,  ehcOptEmitDerivFitsIn  =   False
%%]]
%%[[99
      ,  ehcOptLibSearchPath    =   []
      ,  ehcOptLibPackages      =   []
      ,  ehcProgName            =   emptyFPath
      ,  ehcOptCPP              =   False
      ,  ehcOptUseAssumePrelude =   True
%%]]
      }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Options as passed on the command line
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
ehcCmdLineOpts
  =  [  Option "d"  ["debug"]            (NoArg oDebug)                       "show debug information"
     ,  Option "p"  ["pretty"]           (OptArg oPretty "hs|eh|ast|-")       "show pretty printed source or EH abstract syntax tree, default=eh, -=off, (downstream only)"
     ,  Option ""   ["priv"]             (boolArg oPriv)                      "private flag, used during development of 2 impls of 1 feature"
%%[[1
     ,  Option "t"  ["target"]           (OptArg oTarget "")                  "code generation not available"
%%][(8 codegen)
     ,  Option "t"  ["target"]           (OptArg oTarget (showSupportedTargets' "|"))  ("generate code for target, default=" ++ show defaultTarget)
%%]]
%%[[(1 hmtyinfer)
     ,  Option ""   ["show-top-ty"]      (OptArg oShowTopTy "yes|no")         "show top ty, default=no"
%%]]
     ,  Option "h"  ["help"]             (NoArg oHelp)                        "only show this help"
     ,  Option ""   ["version"]          (NoArg oVersion)                     "only show version info"
     ,  Option ""   ["meta-variant"]     (NoArg oVariant)                     "meta: print variant"
     ,  Option ""   ["stopat"]
%%[[1
                                         (ReqArg oStopAt "0|1|2|3")           "stop at compile phase 0=imports, 1=parse, 2=hs, 3=eh"
%%][8
                                         (ReqArg oStopAt "0|1|2|3|4")         "stop at compile phase 0=imports, 1=parse, 2=hs, 3=eh, 4=core"
%%]]
%%[[7_2
     ,  Option ""   ["nounique"]         (NoArg oUnique)                      "do not compute uniqueness solution"
%%]]
%%[[(8 codegen)
     ,  Option ""   ["trf"]              (ReqArg oTrf ("([+|-][" ++ concat (intersperse "|" (assocLKeys cmdLineTrfs)) ++ "])*"))
                                                                              "switch on/off core transformations"
     ,  Option ""   ["dump-core-stages"] (boolArg optDumpCoreStages)          "dump intermediate Core transformation stages (no)"
%%]]
%%[[(8 codegen grin)
     ,  Option ""   ["dump-grin-stages"] (boolArg optDumpGrinStages)          "dump intermediate Grin and Silly transformation stages (no)"
     ,  Option "O"  ["optimise"]         (OptArg oOptimise "0|1|2")           "optimise, 0=none 1=normal 2=more, default=1"
     ,  Option ""   ["time-compilation"] (NoArg oTimeCompile)                 "show grin compiler CPU usage for each compilation phase (only with -v2)"

     ,  Option ""   ["gen-casedefault"]  (boolArg optSetGenCaseDefault)       "trap wrong casedistinction in C (no)"
     ,  Option "g"  ["gen-own"]          (OptArg  oOwn "0|1|2|3|4")           "generate own 1=parameters/tailjumps, 2=locals, 3=calls, 4=stack (3)"
     ,  Option ""   ["gen-cmt"]          (boolArg optSetGenCmt)               "include comment about code in generated code"
     ,  Option ""   ["gen-debug"]        (boolArg optSetGenDebug)             "include debug info in generated code (yes)"
%%]]
%%[[(8 codegen java)
%%]]
%%[[1
     ,  Option ""   ["meta-target-default"]   (NoArg oTargetDflt)             "meta: print the default codegeneration target"
     ,  Option ""   ["meta-targets"]     (NoArg oTargets)                     "meta: print list of supported codegeneration targets"
%%]]
%%[[8
     ,  Option ""   ["code"]             (OptArg oCode "hs|eh|exe[c]|lexe[c]|bexe[c]|-")  "write code to file, default=bexe (will be obsolete and/or changed, use --target)"
     ,  Option "v"  ["verbose"]          (OptArg oVerbose "0|1|2|3")          "be verbose, 0=quiet 1=normal 2=noisy 3=debug-noisy, default=1"
%%]]
%%[[(8 codegen grin)
     ,  Option ""   ["gen-trace"]        (boolArg optSetGenTrace)             "trace functioncalls in C (no)"
     ,  Option ""   ["gen-rtsinfo"]      (ReqArg oRTSInfo "<nr>")             "flags for rts info dumping (default=0)"
%%][100
%%]]
%%[[9
     -- ,  Option ""   ["chr-scoped"]       (ReqArg  oCHRScoped "0|1|2")         "scoped CHR gen: 0=inst, 1=super, 2=all (default=2)"
%%]]
%%[[(20 codegen)
     ,  Option ""   ["debug-stopat-core-error"]
                                         (boolArg oStopAtCoreError)           "debug: stop at .core parse error (default=off)"
%%]]
%%[[20
     ,  Option ""   ["no-recomp"]        (NoArg oNoRecomp)                    "turn off recompilation check (force recompile)"
     ,  Option ""   ["debug-stopat-hi-error"]
                                         (boolArg oStopAtHIError)             "debug: stop at .hi parse error (default=off)"
     ,  Option "c"  ["compile-only"]     (NoArg oCompileOnly)                 "compile only, do not link"
%%]]
%%[[99
     ,  Option ""   ["numeric-version"]  (NoArg oNumVersion)                  "only show numeric version"
     ,  Option "i"  ["import-path"]      (ReqArg oUsrSearchPath "path")       "search path for user files, path separators=';', appended to previous"
     ,  Option "L"  ["lib-search-path"]  (ReqArg oLibSearchPath "path")       "search path for library files, see also --import-path"
     ,  Option ""   ["package"]          (ReqArg oPackage "package")          "use package"
     ,  Option ""   ["no-prelude"]       (NoArg oNoPrelude)                   "do not assume presence of Prelude"
     ,  Option ""   ["cpp"]              (NoArg oCPP)                         "preprocess source with CPP"
     ,  Option ""   ["limit-tysyn-expand"]
                                         (intArg oLimitTyBetaRed)             "type synonym expansion limit"
     -- 20071002: limiting the number of context reduction steps is not supported starting with the use of CHRs
     -- ,  Option ""   ["limit-ctxt-red"]   (intArg oLimitCtxtRed)               "context reduction steps limit"
     
     ,  Option ""   ["meta-export-env"]  (OptArg oExportEnv "install rootdir[,variant]") "meta: export environmental info of installation"
     ,  Option ""   ["meta-dir-env"]     (NoArg oDirEnv)                      "meta: print directory holding environmental info of installation"
     ,  Option ""   ["use-inplace"]      (boolArg oUseInplace)                "use the inplace runtime libraries"
%%]]
%%[[(99 hmtyinfer)
     ,  Option ""   ["deriv-tree"]       (OptArg oDerivTree ("f|i[,p=[{0,1,2,3,4,5}|<n>m]][,f=" ++ boolArgStr ++ "]"))
                                                                              "emit derivation tree on .lhs file; f=final, i=infer, default=f; p=paper size (0=a0,...; <n>m=2^<n> meter), dflt=2; f=show subsumption"
%%][100
%%]]
     ]
%%]
%%[1
  where  oPretty     ms  o =  case ms of
                                Just "-"     -> o { ehcOptShowEH       = False     }
                                Just "no"    -> o { ehcOptShowEH       = False     }
                                Just "off"   -> o { ehcOptShowEH       = False     }
                                Just "hs"    -> o { ehcOptShowHS       = True      }
                                Just "eh"    -> o { ehcOptShowEH       = True      }
                                Just "pp"    -> o { ehcOptShowEH       = True      }
%%[[1
                                Just "ast"   -> o { ehcOptShowAst      = True      }
%%][100
%%]]
                                _            -> o
%%[[(1 hmtyinfer)
         oShowTopTy  ms  o =  case ms of
                                Just "yes"  -> o { ehcOptShowTopTyPP   = True      }
                                _           -> o
%%]]
         oHelp           o =  o { ehcOptImmQuit       = Just ImmediateQuitOption_Help    }
         oVersion        o =  o { ehcOptImmQuit       = Just ImmediateQuitOption_Version }
         oVariant        o =  o { ehcOptImmQuit   	  = Just ImmediateQuitOption_Meta_Variant }
         oDebug          o =  o { ehcOptDebug         = True
%%[[1
                                , ehcOptShowAst       = True
%%][100
%%]]
                                }
         oStopAt       s o =  o { ehcStopAtPoint       =
                                    case s of
                                      "0" -> CompilePoint_Imports
                                      "1" -> CompilePoint_Parse
                                      "2" -> CompilePoint_AnalHS
                                      "3" -> CompilePoint_AnalEH
%%[[(8 codegen)
                                      "4" -> CompilePoint_Core
%%]]
                                      _   -> CompilePoint_All
                                }
%%[[7_2
         oUnique         o =  o { ehcOptUniqueness    = False   }
%%]]
%%[[(8 codegen)
         oTimeCompile    o =  o { ehcOptTimeCompile       = True    }
%%]]
%%[[1
         oTarget     _   o =  o
%%][(8 codegen)
         oTarget     ms  o =  case ms of
                                Just t -> o { ehcOptTarget = Map.findWithDefault defaultTarget t supportedTargetMp }
                                _      -> o
%%]]
%%[[1
         oTargets        o =  o { ehcOptImmQuit       = Just ImmediateQuitOption_Meta_Targets    	}
         oTargetDflt     o =  o { ehcOptImmQuit       = Just ImmediateQuitOption_Meta_TargetDefault  }
                                      
%%]]
%%[[8
         oCode       ms  o =  case ms of
                                Just "hs"    -> o { ehcOptEmitHS           = True   }
                                Just "eh"    -> o { ehcOptEmitEH           = True   }
%%[[(8 codegen)
                                Just "-"     -> o -- { ehcOptEmitCore         = False  }
                                Just "core"  -> o { ehcOptTarget           = Target_Core
                                                  }
%%]]
%%[[(8 codegen java)
                                Just "java"  -> o { ehcOptEmitJava         = True   }
%%]]
%%[[(8 codegen grin)
                                Just "grin"  -> o -- { ehcOptEmitGrin         = True   }
                                Just "bc"    -> o -- { ehcOptEmitBytecode     = True 
                                                  -- , ehcOptFullProgAnalysis = False
                                                  -- }
                                Just m | m `elem` ["bexe","bexec"]
                                             -> o { ehcOptTarget           = Target_Interpreter_Grin_C
                                                  }

                                Just "c"     -> o -- { ehcOptEmitC            = True
                                                  -- , ehcOptFullProgAnalysis = True
                                                  -- , ehcOptEmitExecBytecode = False
                                                  -- , ehcOptEmitBytecode     = False
                                                  -- , ehcOptErrAboutBytecode = False
                                                  -- }

                                Just m | m `elem` ["exe","exec"]
                                             -> o { ehcOptTarget           = Target_FullProgAnal_Grin_C
                                                  }

                                Just "llvm"  -> o -- { ehcOptEmitLLVM         = True
                                                  -- , ehcOptFullProgAnalysis = True
                                                  -- , ehcOptEmitExecBytecode = False
                                                  -- , ehcOptEmitBytecode     = False
                                                  -- , ehcOptErrAboutBytecode = False
                                                  -- }
                                Just m | m `elem` ["lexe", "lexec"]
                                             -> o { ehcOptTarget           = Target_FullProgAnal_Grin_LLVM
                                                  }
                                Just "cil"   -> o { ehcOptTarget           = Target_FullProgAnal_Grin_CLR
                                                  }
%%]]
%%[[(99 hmtyinfer)
                                Just "dt"    -> o { ehcOptEmitDerivTree    = DerivTreeWay_Final   }
%%]]
                                _            -> o

%%[[(8 codegen)
         oTrf        s   o =  o { ehcOptTrf           = opt s   }
                           where  opt "" =  []
                                  opt o  =  let  (pm,o2) = span (\c -> c == '+' || c == '-') o
                                                 (tr,o3) = span isAlpha o2
                                                 opt2    = opt o3
                                            in   case (pm,tr) of
                                                   ("+",_:_)  -> TrfYes tr : opt2
                                                   ("-",_:_)  -> TrfNo tr : opt2
                                                   ("+",_)    -> [TrfAllYes]
                                                   ("-",_)    -> [TrfAllNo]
                                                   _          -> []
%%]]
%%[[(8 codegen grin)
         oOwn        ms  o =  case ms of
                                Just "0"    -> o { ehcOptOwn     = 0       }
                                Just "1"    -> o { ehcOptOwn     = 1       }
                                Just "2"    -> o { ehcOptOwn     = 2       }
                                Just "3"    -> o { ehcOptOwn     = 3       }
                                Just "4"    -> o { ehcOptOwn     = 4       }
                                Just "5"    -> o { ehcOptOwn     = 5       }
                                Nothing     -> o { ehcOptOwn     = 3       }
                                _           -> o { ehcOptOwn     = 3       }
         oRTSInfo    s   o =  o { ehcOptGenRTSInfo     = read s       }
%%]]
         oVerbose    ms  o =  case ms of
                                Just "0"    -> o { ehcOptVerbosity     = VerboseQuiet       }
                                Just "1"    -> o { ehcOptVerbosity     = VerboseNormal      }
                                Just "2"    -> o { ehcOptVerbosity     = VerboseALot        }
                                Just "3"    -> o { ehcOptVerbosity     = VerboseDebug       }
                                Nothing     -> o { ehcOptVerbosity     = VerboseALot        }
                                _           -> o
%%[[(8 codegen grin)
         oOptimise   ms  o =  case ms of
                                Just "0"    -> o { ehcOptOptimise      = OptimiseNone       }
                                Just "1"    -> o { ehcOptOptimise      = OptimiseNormal     }
                                Just "2"    -> o { ehcOptOptimise      = OptimiseALot       }
                                Nothing     -> o { ehcOptOptimise      = OptimiseALot       }
                                _           -> o
%%]]
%%]]
%%[[9
{-
         oCHRScoped    s o =  o { ehcCfgCHRScoped       =
                                    case s of
                                      "0" -> CHRScopedInstOnly
                                      "1" -> CHRScopedMutualSuper
                                      "2" -> CHRScopedAll
                                      _   -> CHRScopedAll
                                }
-}
%%]]
%%[[20
         oNoRecomp         o   = o { ehcOptCheckRecompile       = False    }
         oCompileOnly      o   = o { ehcOptDoLinking 		    = False    }
%%]]
%%[[99
         oNumVersion       o   = o { ehcOptImmQuit    			= Just ImmediateQuitOption_NumericVersion }
         oUsrSearchPath  s o   = o { ehcOptUsrSearchPath 		= ehcOptUsrSearchPath o ++ mkSearchPath s }
         oLibSearchPath  s o   = o { ehcOptLibSearchPath 		= ehcOptLibSearchPath o ++ mkSearchPath s }
         oPackage        s o   = o { ehcOptLibPackages   		= ehcOptLibPackages   o ++ [s] }
         oNoPrelude        o   = o { ehcOptUseAssumePrelude		= False   }
         oCPP              o   = o { ehcOptCPP        			= True    }
         oLimitTyBetaRed   o l = o { ehcOptTyBetaRedCutOffAt 	= l }
         oLimitCtxtRed     o l = o { ehcOptPrfCutOffAt       	= l }
         oUseInplace       o b = o { ehcOptUseInplace 			= b }
         oExportEnv     ms o   = o { ehcOptImmQuit   			= Just (ImmediateQuitOption_Meta_ExportEnv ms) }
         oDirEnv           o   = o { ehcOptImmQuit   			= Just ImmediateQuitOption_Meta_DirEnv }
%%]]
%%[[(99 hmtyinfer)
         oDerivTree  ms  o =  case ms of
                                Just ('f':a) -> opts a $ o { ehcOptEmitDerivTree    = DerivTreeWay_Final  }
                                Just ('i':a) -> opts a $ o { ehcOptEmitDerivTree    = DerivTreeWay_Infer  }
                                Nothing      ->          o { ehcOptEmitDerivTree    = DerivTreeWay_Final  }
                                _            ->          o
                           where opts (',':'p':'=':sz:'m':r) o = opts r $ o { ehcOptEmitDerivTreePaperSize = ['m',sz] }
                                 opts (',':'p':'=':sz    :r) o = opts r $ o { ehcOptEmitDerivTreePaperSize = [sz] }
                                 opts (',':'f':'='       :r) o = maybe o (\(b,r) -> opts r $ o {ehcOptEmitDerivFitsIn = b}) (optBooleanTake r)
                                 opts _                      o = o
%%][100
%%]]
%%]

%%[99
intArg  tr = ReqArg (optInt tr) "<nr>"

optInt :: (EHCOpts -> Int -> EHCOpts) -> String -> EHCOpts -> EHCOpts
optInt tr s o
 = tr o $ read s
%%]

%%[1
optBooleanTake :: String -> Maybe (Bool,String)
optBooleanTake s
  = case s of
      ('-':r)           -> Just (False,r)
      ('n':'o':r)       -> Just (False,r)
      ('o':'f':'f':r)   -> Just (False,r)
      ('0':r)           -> Just (False,r)
      ('+':r)           -> Just (True ,r)
      ('y':'e':'s':r)   -> Just (True ,r)
      ('o':'n':r)       -> Just (True ,r)
      ('1':r)           -> Just (True ,r)
      _                 -> Nothing

optBoolean :: (EHCOpts -> Bool -> EHCOpts) -> Maybe String -> EHCOpts -> EHCOpts
optBoolean tr ms o
 = case ms of
     Just s -> maybe o (tr o . fst) (optBooleanTake s)
     _      -> o

boolArgStr = "0|1|no|yes|off|on|-|+"
boolArg tr = OptArg (optBoolean tr) boolArgStr
%%]

%%[1
oPriv                o b = o { ehcOptPriv           = b }
%%]

%%[(8 codegen)
optDumpCoreStages    o b = o { ehcOptDumpCoreStages = b }
%%]

%%[(8 codegen grin)
optSetGenTrace       o b = o { ehcOptGenTrace       = b }
optSetGenRTSInfo     o b = o { ehcOptGenRTSInfo     = b }
optSetGenCaseDefault o b = o { ehcOptGenCaseDefault = b }
optSetGenCmt         o b = o { ehcOptGenCmt         = b }
optSetGenDebug       o b = o { ehcOptGenDebug       = b }
optDumpGrinStages    o b = o { ehcOptDumpGrinStages = b {-, ehcOptEmitGrin = b -} }
%%]

%%[(20 codegen)
oStopAtCoreError     o b = o { ehcDebugStopAtCoreError     = b }
%%]

%%[20
oStopAtHIError       o b = o { ehcDebugStopAtHIError       = b }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Discrimination options for recompile, represent as string, difference means recompile
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(optsDiscrRecompileRepr)
optsDiscrRecompileRepr :: EHCOpts -> String
optsDiscrRecompileRepr opts
  = concat
    $ intersperse " "
    $ [ show (ehcOptAspects opts)
%%[[(20 codegen)
      , o "clsrec"          (ehcCfgClassViaRec      opts)
      -- , o "exec"            (ehcOptEmitExecC        opts)
      -- , o "bexec"           (ehcOptEmitExecBytecode opts)
      , show (ehcOptTarget opts)
      , show (ehcOptOptimise opts)
%%]]
      ]
  where o m v = if v then m else ""
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fitting options (should be in FitsIn, but here it avoids mut rec modules)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Difference strong/weak:

strong: in a context where information is known (i.e. type signature)
strong allows impredicative binding whereas weak will instantiate quantifiers

%%[(9 hmtyinfer) export(FIOBind(..),fioBindIsYes,fioBindNoSet)
data FIOBind = FIOBindYes | FIOBindNoBut TyVarIdS

fioBindNoSet :: FIOBind -> TyVarIdS
fioBindNoSet (FIOBindNoBut s) = s

fioBindIsYes :: FIOBind -> Bool
fioBindIsYes FIOBindYes = True
fioBindIsYes _          = False
%%]

%%[(4 hmtyinfer).FIOpts.hd export(FIOpts(..))
data FIOpts =  FIOpts   {  fioLeaveRInst     ::  Bool                ,  fioBindRFirst           ::  Bool
                        ,  fioBindLFirst     ::  Bool                ,  fioBindLBeforeR         ::  Bool
                        ,  fioMode           ::  FIMode              ,  fioUniq                 ::  UID
%%[[7
                        ,  fioNoRLabElimFor  ::  [HsName]            ,  fioNoLLabElimFor        ::  [HsName]
%%]]
%%[[9
                        ,  fioPredAsTy       ::  Bool                ,  fioAllowRPredElim       ::  Bool
                        ,  fioDontBind       ::  TyVarIdS
                        ,  fioBindLVars      ::  FIOBind             ,  fioBindRVars            ::  FIOBind
%%]]
%%[[16
                        ,  fioFitFailureToProveObl    :: Bool
                        ,  fioFitVarFailureToProveObl :: Bool
%%]]
%%[[50
                        ,  fioAllowEqOpen    ::  Bool                ,  fioInstCoConst          ::  HowToInst
%%]]
                        }
%%]

%%[(4 hmtyinfer).strongFIOpts.hd export(strongFIOpts)
strongFIOpts :: FIOpts
strongFIOpts =  FIOpts  {  fioLeaveRInst     =   False               ,  fioBindRFirst           =   True
                        ,  fioBindLFirst     =   True                ,  fioBindLBeforeR         =   True
                        ,  fioMode           =   FitSubLR            ,  fioUniq                 =   uidStart
%%[[7
                        ,  fioNoRLabElimFor  =   []                  ,  fioNoLLabElimFor        =   []
%%]]
%%[[9
                        ,  fioPredAsTy       =   False               ,  fioAllowRPredElim       =   True
                        ,  fioDontBind       =   Set.empty
                        ,  fioBindLVars      =   FIOBindYes          ,  fioBindRVars            =   FIOBindYes
%%]]
%%[[16
                        ,  fioFitFailureToProveObl    = False
                        ,  fioFitVarFailureToProveObl = False
%%]]
%%[[50
                        ,  fioAllowEqOpen    =   False               ,  fioInstCoConst          =   instCoConst
%%]]
                        }
%%]

%%[(4 hmtyinfer)
instance Show FIOpts where
  show o =  "FIOpts"
%%]

%%[(4 hmtyinfer)
instance PP FIOpts where
  pp   o =  "FIOpts{"
            >#< "leaveRInst=" >|< pp (fioLeaveRInst o)
            >#< "bindLFirst=" >|< pp (fioBindLFirst o)
            >#< "bindRFirst=" >|< pp (fioBindRFirst o)
%%[[7
            >#< "fioNoLLabElimFor=" >|< pp (show $ fioNoLLabElimFor o)
            >#< "fioNoRLabElimFor=" >|< pp (show $ fioNoRLabElimFor o)
%%]]
%%[[9
            >#< "allowRPredElim=" >|< pp (fioAllowRPredElim o)
%%]]
            >#< "}"
%%]

%%[(4 hmtyinfer).FIOpts.instLFIOpts export(instLFIOpts)
instLFIOpts :: FIOpts
instLFIOpts = strongFIOpts {fioBindRFirst = False}
%%]

%%[(4 hmtyinfer).FIOpts.instLRFIOpts export(instLRFIOpts)
instLRFIOpts :: FIOpts
instLRFIOpts = strongFIOpts {fioBindRFirst = False, fioBindLFirst = False}
%%]

%%[(4 hmtyinfer).FIOpts.instFIOpts export(unifyFIOpts,instFIOpts)
unifyFIOpts :: FIOpts
unifyFIOpts = strongFIOpts {fioMode = FitUnify}

instFIOpts :: FIOpts
instFIOpts = instLFIOpts {fioLeaveRInst = True, fioBindLFirst = False}
%%]

%%[(4_2 hmtyinfer).FIOpts.defaults export(meetFIOpts,joinFIOpts,impredFIOpts)
meetFIOpts :: FIOpts
meetFIOpts = unifyFIOpts {fioMode = FitMeet}

joinFIOpts :: FIOpts
joinFIOpts = unifyFIOpts {fioMode = FitJoin}

impredFIOpts :: FIOpts
impredFIOpts = strongFIOpts {fioBindToTyAlts = True}
%%]

%%[(5 hmtyinfer) export(weakFIOpts)
weakFIOpts :: FIOpts
weakFIOpts = strongFIOpts {fioLeaveRInst = True, fioBindRFirst = False}
%%]

%%[(9 hmtyinfer) export(predFIOpts,implFIOpts)
predFIOpts :: FIOpts
predFIOpts = strongFIOpts {fioPredAsTy = True, fioLeaveRInst = True}

implFIOpts  :: FIOpts
implFIOpts = strongFIOpts {fioAllowRPredElim = False}
%%]

%%[(4 hmtyinfer) export(fioSwapPolarity, fioSwapOpts)
fioSwapOpts :: FIOpts -> FIOpts
fioSwapOpts fio
  = fio
      { fioBindRFirst   = fioBindLFirst fio
      , fioBindLFirst   = fioBindRFirst fio
      , fioBindLBeforeR = not (fioBindLBeforeR fio)
%%[[9
      , fioBindLVars    = fioBindRVars fio
      , fioBindRVars    = fioBindLVars fio
%%]]
      }

fioSwapPolarity :: Polarity -> FIOpts -> FIOpts
fioSwapPolarity pol fio = fio {fioMode = fimSwapPol pol (fioMode fio)}
%%]

%%[(4 hmtyinfer).fioMkStrong export(fioMkStrong)
fioMkStrong :: FIOpts -> FIOpts
fioMkStrong fi = fi {fioLeaveRInst = False, fioBindRFirst = True, fioBindLFirst = True}
%%]

%%[(4 hmtyinfer).fioMkUnify export(fioMkUnify)
fioMkUnify :: FIOpts -> FIOpts
fioMkUnify fi = fi {fioMode = FitUnify}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FitsIn opts related
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer) export(fioIsSubsume)
fioIsSubsume :: FIOpts -> Bool
fioIsSubsume fio =  case fioMode fio of {FitSubLR -> True ; _ -> False}
%%]

%%[(4_2 hmtyinfer) export(fioIsMeetJoin)
fioIsMeetJoin :: FIOpts -> Bool
fioIsMeetJoin fio =  case fioMode fio of {FitMeet -> True ; FitJoin -> True ; _ -> False}
%%]
