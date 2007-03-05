%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gamma utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4 module {%{EH}Gam.Utils} import(Data.List,{%{EH}Base.Common},{%{EH}Base.Opts},{%{EH}Ty},{%{EH}Ty.FitsInCommon},{%{EH}Ty.FitsIn},{%{EH}Error},{%{EH}Gam},{%{EH}Cnstr},{%{EH}Substitutable})
%%]

%%[4_2 import({%{EH}Ty.Trf.ElimAlts}) export(valGamElimAlts)
%%]

%%[50 import({%{EH}Ty.Trf.ElimEqual}) export(valGamElimEqual)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Alts elim
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4_2.valGamElimAlts
valGamElimAlts :: FIOpts -> FIEnv -> TyVarIdL -> UID -> Cnstr -> ValGam -> (ValGam,Cnstr,ErrGam)
valGamElimAlts opts env globTvL uniq gCnstr g
  =  let  (g',(c,eg,_))
            =  gamMapThr
                  (\(n,vgi) (c,eg,u)
                  	->  let  (u',u1) = mkNewLevUID u
                  	         fo = tyElimAlts (mkFitsInWrap' env) opts globTvL u1 (c |=> vgiTy vgi)
                  	         cg = cnstrFilterTyAltsMappedBy gCnstr (foCnstr fo)
                  	    in   ((n,vgi {vgiTy = foTy fo}),(foCnstr fo |=> c |=> cg,gamAdd n (foErrL fo) eg,u'))
                  )
                  (emptyCnstr,emptyGam,uniq) (gCnstr |=> g)
     in   (g',tyElimAltsCleanup gCnstr c,eg)
%%]
valGamElimAlts :: FIOpts -> FIEnv -> TyVarIdL -> UID -> Cnstr -> ValGam -> (ValGam,Cnstr,ErrGam)
valGamElimAlts opts env globTvL uniq gCnstr g
  =  let  (g',(c,eg,_))
            =  gamMapThr
                  (\(n,vgi) (c,eg,u)
                  	->  let  (u',u1) = mkNewLevUID u
                  	         (t,ce,e) = tyElimAlts (mkFitsInWrap env) opts globTvL u1 (c |=> vgiTy vgi)
                  	    in   ((n,vgi {vgiTy = t}),(ce |=> c,gamAdd n e eg,u'))
                  )
                  (emptyCnstr,emptyGam,uniq) (gCnstr |=> g)
          c2 = cnstrDelAlphaRename c
          c3 = cnstrKeys c2 `cnstrDel` cnstrFilterAlphaRename gCnstr
     in   (g',c2 |=> c3,eg)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Equal elim
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50.valGamElimEqual
valGamElimEqual :: ValGam -> ValGam
valGamElimEqual g
  =  let  (g',_)
            =  gamMapThr
                  (\(n,vgi) _
                  	->  let  (t,_) = tyElimEqual (vgiTy vgi)
                  	    in   ((n,vgi {vgiTy = t}),())
                  )
                  () g
     in   g'
%%]
