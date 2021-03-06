%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utilities for Ty which cannot be placed elsewhere (e.g. because of module cycles)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(2 hmtyinfer || hmtyast) module {%{EH}Ty.Utils1} import({%{EH}Base.Common}, {%{EH}Substitutable}, {%{EH}VarMp}, {%{EH}Ty}, {%{EH}Ty.Pretty}) 
%%]

%%[(2 hmtyinfer || hmtyast) import(EH.Util.Pretty) 
%%]

%%[(98 hmtyinfer || hmtyast) import({%{EH}Base.Builtin}, {%{EH}Opts}) 
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Top level main type
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(2 hmtyinfer || hmtyast) hs export(ppTyS)
ppTyS :: VarUpdatable Ty m => m -> Ty -> PP_Doc
ppTyS = ppS ppTy
%%]

%%[(98 hmtyinfer || hmtyast) hs export(tyTopLevelMain)
tyTopLevelMain :: EHCOpts -> TyVarId -> Ty
tyTopLevelMain opts uniq = mk1ConApp (ehcOptBuiltin opts ehbnIO) (mkTyVar uniq)
%%]

