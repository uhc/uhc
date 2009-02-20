%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

LLVM compilation

%%[8 module {%{EH}EHC.CompilePhase.CompileLLVM}
%%]

-- general imports
%%[8 import({%{EH}EHC.Common})
%%]
%%[8 import({%{EH}EHC.CompileUnit})
%%]
%%[8 import({%{EH}EHC.CompileRun})
%%]

%%[8 import(qualified {%{EH}Config} as Cfg)
%%]
%%[8 import({%{EH}EHC.Environment})
%%]
%%[(8 codegen) import({%{EH}Base.Target})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: LLVM compilation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen grin) export(cpCompileWithLLVM)
cpCompileWithLLVM :: HsName -> EHCompilePhase()
cpCompileWithLLVM modNm
  = do { cr <- get
       ; let  (_,_,opts,fp) = crBaseInfo modNm cr
              fpLL          = mkOutputFPath opts modNm fp "ll"
              fpExec        = maybe (mkOutputFPath opts modNm fp "") (\s -> mkOutputFPath opts modNm fp s) Cfg.mbSuffixExec
              variant       = ehcenvVariant (ehcOptEnvironment opts)
              libs          = map (\lib -> "-l " ++ lib) $
                              [ Cfg.mkInstallFilePrefix opts Cfg.LIB variant ++ "prim.o"
                              , Cfg.mkInstallFilePrefix opts Cfg.LIB variant ++ "llvm-gc.o"
                              , Cfg.mkInstallFilePrefix opts Cfg.LIB variant ++ "timing.o"
                              , Cfg.mkInstallFilePrefix opts Cfg.LIB_SHARED variant ++ "libgc.a"
                              ]
              inputOpts     = [ fpathToStr fpLL ]
              outputOpts    = ["-o " ++ fpathToStr fpExec]
       ; when ( targetIsLLVM (ehcOptTarget opts) )
         (  do { let compileLL 
                       = concat $ intersperse " "
                         $  [ Cfg.shellCmdLLVM ]
                         ++ libs
                         ++ outputOpts
                         ++ inputOpts
               ; when (ehcOptVerbosity opts >= VerboseALot)
                 (  do { cpMsg' modNm VerboseALot "LLVM" Nothing fpExec
                       ; lift $ putStrLn compileLL
                       }
                 )
               ; cpSystem compileLL
               }
         ) 
       }                 
%%]

