% $Id: EHC.lag 199 2004-05-12 19:11:13Z andres $

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module EHParser import(IO, UU.Parsing, UU.Parsing.Offside, UU.Scanner.Position, UU.Scanner.GenToken, EHCommon, EHMainAG)
%%]

%%[1.Scanner import(UU.Scanner)
%%]

%%[1 export(pAGItf, offsideScanHandle)
%%]

%%[4 import (EHTy)
%%]

%%[7.Scanner -1.Scanner import(EHScanner)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scanner
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.ScanOpts
data ScanOpts
  =  ScanOpts
        {   scoKeywordsTxt      ::  [String]
        ,   scoKeywordsOps      ::  [String]
        ,   scoSpecChars        ::  String
        ,   scoOpChars          ::  String
        }
%%]

%%[1.scanOpts
scanOpts :: ScanOpts
scanOpts
%%]
%%[1.defaultScanOpts
  =  ScanOpts
%%]
%%[7 -1.defaultScanOpts
  =  defaultScanOpts
%%]
%%[1
        {   scoKeywordsTxt      =   
                [ "in"
%%]
%%[4
                , "forall", "exists"
%%]
%%[5
                , "data", "case", "if", "then", "else"
%%]
%%[8
                , "foreign", "import", "jazy"
%%]
%%[9
                , "class", "instance"
%%]
%%[1
                ] ++ offsideTrigs
        ,   scoKeywordsOps      =
                [ "=", "\\", show hsnArrow, "::", "@"
%%]
%%[2
                , "..."
%%]
%%[4
                , "."
%%]
%%[5
                , "|"
%%]
%%[6
                , "*"
%%]
%%[7
                , ":="
%%]
%%[9
                , "=>", "<:"
%%]
%%[1
                ]
        ,   scoSpecChars        =
                "();,[]{}"
        ,   scoOpChars          =
                "!#$%&*+/<=>?@\\^|-:.~"
%%]
%%[7 -1.ScanOpts
        ,   scoSpecPairs        =
                [  show hsnORow, show hsnCRow
                ,  show hsnOSum, show hsnCSum
%%]
%%[9
                ,  show hsnOImpl, show hsnCImpl
%%]
%%[7
                ]
%%]
%%[1
        }
%%]

%%[1.offsideTrigs
offsideTrigs  =  [ "let" ]
%%]

%%[5.offsideTrigs -1.offsideTrigs
offsideTrigs  =  [ "let", "of" ]
%%]

%%[9.offsideTrigs -5.offsideTrigs
offsideTrigs  =  [ "let", "of", "where" ]
%%]

%%[1.scanHandle
scanHandle :: ScanOpts -> FilePath -> Handle -> IO [Token]
scanHandle opts fn fh
  = do  {  txt <- hGetContents fh
        ;  return (scan (scoKeywordsTxt opts) (scoKeywordsOps opts) (scoSpecChars opts) (scoOpChars opts) (initPos fn) txt) 
        }
%%]

%%[7 -1.scanHandle
%%]

%%[1.offsideScanHandle
offsideScanHandle fn fh
  = do  {  tokens <- scanHandle scanOpts fn fh
        ;  return (scanOffside moduleT oBrace cBrace triggers tokens)
        }
  where   moduleT   = reserved "let" noPos
          oBrace    = reserved "{" noPos
          cBrace    = reserved "}" noPos
          triggers  = [ reserved x noPos | x <- offsideTrigs ]
%%]

%%[1
instance Position (Maybe Token) where
  line    =  maybe (-1)  (line.position) 
  column  =  maybe (-1)  (column.position)
  file    =  maybe ""    (file.position)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser signatures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.parserSigs
type EHParser         ep    =    (IsParser (OffsideParser i o Token p) Token,InputState i Token p, OutputState o, Position p)
                                    => OffsideParser i o Token p ep

pAGItf                      ::   EHParser T_AGItf

pExpr, pExprApp, pExprBase  ::   EHParser T_Expr
pExprPrefix                 ::   EHParser (T_Expr -> T_Expr)

pDecls                      ::   EHParser T_Decls
pDecl                       ::   EHParser T_Decl

pPatExpr, pPatExprBase      ::   EHParser T_PatExpr

pTyExpr, pTyExprBase        ::   EHParser T_TyExpr

pInt                        ::   EHParser Int
pChr                        ::   EHParser Char
pKeyw                       ::   Show k => k -> EHParser String

pCon                        ::   EHParser HsName
pVar                        ::   EHParser HsName
%%]

%%[4
pTyExprPrefix               ::   EHParser (T_TyExpr -> T_TyExpr)
%%]

%%[5
pCaseAlts                   ::   EHParser T_CaseAlts
pCaseAlt                    ::   EHParser T_CaseAlt

pDataConstr                 ::   EHParser T_DataConstr
pDataConstrs                ::   EHParser T_DataConstrs

pTyExprApp                  ::   EHParser T_TyExpr

pTyVars                     ::   EHParser T_TyVars
pTyVar                      ::   EHParser T_TyVar
%%]

%%[7
pDataLabFields              ::   EHParser T_DataFields
pDataFields                 ::   EHParser T_DataFields
pDataLabField               ::   EHParser T_DataField
pDataField                  ::   EHParser T_DataField
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parsers, shared/common
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.pApp
pApp alg p      =    mkApp alg <$> pList1 p
%%]

%%[1.pParenProd
pParenProd alg@(_,_,_,par) pE
                =    pParens pP
                     where
                       pP  =    mkProdApp alg <$> pSucceed []
                           <|>  pE
                                <**>  (    (\es e -> mkProdApp alg (e:es))
                                           <$>  pList1 (pComma *> pE)
                                      <|>  pSucceed par
                                      )
%%]

%%[1.scanWrappers
pChr            =    head <$> pChar
pInt            =    read <$> pInteger
pKeyw k         =    pKey (show k)
pCon            =    HNm <$> pConid
pVar            =    HNm <$> pVarid
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for the root
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.pAGItf
pAGItf          =    sem_AGItf_AGItf <$> pExpr    
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for Decl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- common
%%[pDecl.1
pDecls          =    foldr sem_Decls_Cons sem_Decls_Nil
                                      <$>  pBlock pOCurly pSemi pCCurly pDecl
pDecl           =    sem_Decl_Val     <$>  pPatExprBase  <*   pKey "="   <*> pExpr
                <|>  sem_Decl_TySig   <$>  pVar          <*   pKey "::"  <*> pTyExpr
%%]

%%[pDecl.5
                <|>  sem_Decl_Data    <$   pKey "data"   <*>  pCon       <*> pTyVars
                                                         <*   pKey "="   <*> pDataConstrs
%%]

%%[pDecl.6
                <|>  sem_Decl_KiSig   <$>  pCon          <*   pKey "::"  <*> pKiExpr
%%]

%%[pDecl.8
                <|>  (\conv saf imp nm sig -> sem_Decl_FFI conv saf (if null imp then show nm else imp) nm sig)
                     <$   pKey "foreign" <* pKey "import" <*> pKey "jazy"
                     <*>  ((pKey "safe" <|> pKey "unsafe") `opt` "safe")
                     <*>  (pString `opt` "")
                     <*>  pVar
                     <*   pKey "::" <*> pTyExpr
%%]

%%[pDecl.9
                <|>  pDeclClass
                <|>  pDeclInstance
%%]

-- versions
%%[1.pDecl
%%@pDecl.1
%%]

%%[5.pDecl -1.pDecl
%%@pDecl.1
%%@pDecl.5
%%]

%%[6.pDecl -5.pDecl
%%@pDecl.1
%%@pDecl.5
%%@pDecl.6
%%]

%%[8.pDecl -6.pDecl
%%@pDecl.1
%%@pDecl.5
%%@pDecl.6
%%@pDecl.8
%%]

%%[9.pDecl -8.pDecl
%%@pDecl.1
%%@pDecl.5
%%@pDecl.6
%%@pDecl.8
%%@pDecl.9
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for PatExpr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- common
%%[pPatExprBase.1
pPatExprBase    =    pVar <**>  (    flip sem_PatExpr_VarAs <$ pKey "@" <*> pPatExprBase
                                <|>  pSucceed sem_PatExpr_Var
                                )
                <|>  sem_PatExpr_Con <$> pCon
%%]

%%[pPatExprBaseParenProd.1
                <|>  pParenProd patExprAlg pPatExpr
%%]

%%[patExprAlg.1
patExprAlg      =    (sem_PatExpr_Con,sem_PatExpr_App
                     ,sem_PatExpr_AppTop,sem_PatExpr_Parens)
%%]

%%[patExpr.1
pPatExpr        =    pApp patExprAlg pPatExprBase
%%]

%%[patExpr.4
pPatExpr        =    pP <??> (sem_PatExpr_TypeAs <$ pKey "::" <*> pTyExpr)
                where pP = pApp patExprAlg pPatExprBase
%%]

-- versions
%%[1.pPatExprBase
%%@pPatExprBase.1
%%@pPatExprBaseParenProd.1
%%]

%%[7.pPatExprBase -1.pPatExprBase
%%@pPatExprBase.1
                <|>  pParenRow True (show hsnORec) (show hsnCRec) "=" Nothing
                        (sem_RecPatExpr_Empty,sem_RecPatExpr_Expr . sem_PatExpr_Var,sem_RecPatExpr_Ext,sem_PatExpr_Rec,sem_PatExpr_Parens)
                        pSel pPatExpr
%%]

%%[1.pPatExpr
%%@patExprAlg.1
%%@patExpr.1
%%]

%%[4.pPatExpr -1.pPatExpr
%%@patExprAlg.1
%%@patExpr.4
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for KiExpr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[6
pKiExpr, pKiExprBase        ::   EHParser T_KiExpr

kiExprAlg       =    (sem_KiExpr_Con,sem_KiExpr_App,sem_KiExpr_AppTop,sem_KiExpr_Parens)
pKiExprBase     =    sem_KiExpr_Con <$> (pCon <|> HNm <$> pKey "*")
                <|>  sem_KiExpr_Var <$> pVar
                <|>  pParens pKiExpr
pKiExpr         =    pChainr (mkArrow kiExprAlg <$ pKeyw hsnArrow) pKiExprBase
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for TyExpr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- common
%%[pTyExprCommon.1
tyExprAlg       =    (sem_TyExpr_Con,sem_TyExpr_App
                     ,sem_TyExpr_AppTop,sem_TyExpr_Parens)
%%]

%%[pTyExprBase.1
pTyExprBase     =    sem_TyExpr_Con   <$>  pCon
                <|>  pParenProd tyExprAlg pTyExpr
%%]

%%[pTyExprBase.2
pTyExprBase     =    sem_TyExpr_Con   <$>  pCon
                <|>  sem_TyExpr_Wild  <$   pKey "..."
                <|>  pParenProd tyExprAlg pTyExpr
%%]

%%[pTyExprBase.3.Head
pTyExprBase     =    sem_TyExpr_Con   <$>  pCon
                <|>  sem_TyExpr_Var   <$>  pVar
                <|>  sem_TyExpr_Wild  <$   pKey "..."
%%]

%%[pTyExprBase.3
                <|>  pParenProd tyExprAlg pTyExpr
%%]

%%[pTyExprBase.7
                <|>  pParenRow False (show hsnORow) (show hsnCRow) "::" Nothing
                        (sem_RowTyExpr_Empty,const sem_RowTyExpr_Empty,sem_RowTyExpr_Ext,sem_TyExpr_Row,id)
                        pVar pTyExpr
                <|>  pParenRow True (show hsnORec) (show hsnCRec) "::" Nothing
                        (sem_RowTyExpr_Empty,const sem_RowTyExpr_Empty,sem_RowTyExpr_Ext
                            ,\r -> mkConApp tyExprAlg hsnRec [sem_TyExpr_Row r]
                            ,sem_TyExpr_Parens)
                        pVar pTyExpr
                <|>  pParenRow False (show hsnOSum) (show hsnCSum) "::" Nothing
                        (sem_RowTyExpr_Empty,const sem_RowTyExpr_Empty,sem_RowTyExpr_Ext
                            ,\r -> mkConApp tyExprAlg hsnSum [sem_TyExpr_Row r]
                            ,id)
                        pVar pTyExpr
%%]

%%[pTyExprBase.9
                <|>  pParenRow False (show hsnORow) (show hsnCRow) "::" Nothing
                        (sem_RowTyExpr_Empty,sem_RowTyExpr_Var,sem_RowTyExpr_Ext,sem_TyExpr_Row,id)
                        pVar pTyExpr
                <|>  pParenRow True (show hsnORec) (show hsnCRec) "::" Nothing
                        (sem_RowTyExpr_Empty,sem_RowTyExpr_Var,sem_RowTyExpr_Ext
                            ,\r -> mkConApp tyExprAlg hsnRec [sem_TyExpr_Row r]
                            ,sem_TyExpr_Parens)
                        pVar pTyExpr
                <|>  pParenRow False (show hsnOSum) (show hsnCSum) "::" Nothing
                        (sem_RowTyExpr_Empty,sem_RowTyExpr_Var,sem_RowTyExpr_Ext
                            ,\r -> mkConApp tyExprAlg hsnSum [sem_TyExpr_Row r]
                            ,id)
                        pVar pTyExpr
%%]

%%[pTyExprApp.5
pTyExprApp      =    pApp tyExprAlg pTyExprBase
%%]

%%[pTyExpr.1
pTyExpr         =    pChainr
                       (mkArrow tyExprAlg <$ pKeyw hsnArrow)
                       pTyExprBase
%%]

%%[pTyExpr.4
pTyExpr         =    pTyExprPrefix <*> pTyExpr
                <|>  pTyExprBase <??> (flip (mkArrow tyExprAlg) <$ pKeyw hsnArrow <*> pTyExpr)
%%]

%%[pTyExpr.5
pTyExpr         =    pTyExprPrefix <*> pTyExpr
                <|>  pTyExprApp <??> (flip (mkArrow tyExprAlg) <$ pKeyw hsnArrow <*> pTyExpr)
%%]

%%[pTyExprPrefix.4
pTyExprPrefix   =    sem_TyExpr_Quant
                     <$>  (TyQu_Forall <$ pKey "forall" <|> TyQu_Exists <$ pKey "exists")
                     <*>  pVar <* pKey "."
%%]

%%[pPackImpl.9
pPackImpl       ::   EHParser p -> EHParser p
pPackImpl       =    pPacked (pKeyw hsnOImpl) (pKeyw hsnCImpl)
%%]

%%[pTyExprPrefix.9
                <|>  mkArrow tyExprAlg
                     <$>  pPackImpl
                            (    pPr
                            <|>  pIm
                            <|>  pSucceed  sem_TyExpr_NoImpls
                            )
                     <*   pKeyw hsnArrow
                <|>  (    mkArrow tyExprAlg <$> (pPr <|> pIm)
                     <|>  flip (foldr (mkArrow tyExprAlg))
                          <$> pParens ((++) <$> pList1Sep pComma pPr <*> ((:[]) <$ pComma <*> pIm `opt` []))
                     )
                     <*   pKey "=>"
                where  pPr = sem_TyExpr_Pred   <$>  pPrExpr
                       pPr :: EHParser T_TyExpr
                       pIm = sem_TyExpr_Impls  <$   pKey "..."
                       pIm :: EHParser T_TyExpr
%%]

--versions
%%[1.pTyExpr
%%@pTyExprCommon.1
%%@pTyExprBase.1
%%@pTyExpr.1
%%]

%%[2.pTyExpr -1.pTyExpr
%%@pTyExprCommon.1
%%@pTyExprBase.2
%%@pTyExpr.1
%%]

%%[3.pTyExpr -2.pTyExpr
%%@pTyExprCommon.1
%%@pTyExprBase.3.Head
%%@pTyExprBase.3
%%@pTyExpr.1
%%]

%%[4.pTyExpr -3.pTyExpr
%%@pTyExprCommon.1
%%@pTyExprBase.3.Head
%%@pTyExprBase.3
%%@pTyExpr.4
%%]

%%[5.pTyExpr -4.pTyExpr
%%@pTyExprCommon.1
%%@pTyExprBase.3.Head
%%@pTyExprBase.3
%%@pTyExprApp.5
%%@pTyExpr.5
%%]

%%[7.pTyExpr -5.pTyExpr
%%@pTyExprCommon.1
%%@pTyExprBase.3.Head
%%@pTyExprBase.7
%%@pTyExprApp.5
%%@pTyExpr.5
%%]

%%[9.pTyExpr -7.pTyExpr
%%@pTyExprCommon.1
%%@pTyExprBase.3.Head
%%@pTyExprBase.9
%%@pTyExprApp.5
%%@pTyExpr.5
%%]

%%[4.pTyExprPrefix
%%@pTyExprPrefix.4
%%]

%%[9.pTyExprPrefix -4.pTyExprPrefix
%%@pTyExprPrefix.4
%%@pTyExprPrefix.9
%%@pPackImpl.9
%%]

%%[5
pTyExprs        ::   EHParser T_TyExprs
pTyExprs        =    pFoldr (sem_TyExprs_Cons,sem_TyExprs_Nil) pTyExprBase
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for Expr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- common
%%[pExprBaseCommon.1
pExprBase       =    sem_Expr_IConst     <$>  pInt
                <|>  sem_Expr_CConst     <$>  pChr
                <|>  sem_Expr_Var        <$>  pVar
                <|>  sem_Expr_Con        <$>  pCon
%%]

%%[pExprBaseParenProd.1
                <|>  pParenProd exprAlg pExpr
%%]

%%[pExprBaseCommon.5
                <|>  sem_Expr_Case       <$   pKey "case" <*> pExpr <* pKey "of" <*> pCaseAlts
%%]

%%[pExprBase.7
                <|>  pParenRow True (show hsnORec) (show hsnCRec) "=" (Just (":=",sem_RecExpr_Upd))
                        (sem_RecExpr_Empty,sem_RecExpr_Expr . sem_Expr_Var,sem_RecExpr_Ext,sem_Expr_Rec,sem_Expr_Parens)
                        pVar pExpr
%%]

%%[pExprBase.8
                <|>  sem_Expr_Undefined  <$   pKey "..."
%%]

%%[pExprApp.1
pExprApp        =    pApp exprAlg pExprBase
%%]

%%[pExprApp.7
pExprApp        =    pApp exprAlg (pExprBase <**> pExprSelSuffix)
%%]

%%[pExprApp.9
pExprApp        =    let  pE = pExprBase <**> pExprSelSuffix
                          pA = flip sem_Expr_App <$> pE
                          pI = pPackImpl ((\a p e -> sem_Expr_AppImpl e p a) <$> pExpr <* pKey "<:" <*> pPrExpr)
                     in   pE <??> ((\l e -> sem_Expr_AppTop (foldl (flip ($)) e l)) <$> pList1 (pA <|> pI))
%%]

%%[pExprPrefix.1
pExprPrefix     =    sem_Expr_Let      <$ pKey "let"
                     <*> pDecls        <* pKey "in"
%%]

%%[pExprPrefix.1.Lam
                <|>  sem_Expr_Lam      <$ pKey "\\"
                     <*> pPatExprBase  <* pKey "->"
%%]

%%[pExprPrefix.7.Lam
                <|>  (\ps -> \e -> foldr sem_Expr_Lam e ps)  <$ pKey "\\"
                     <*> pList1 pPatExprBase                 <* pKey "->"
%%]

%%[pExprPrefix.9.Lam
                <|>  (flip (foldr ($)))
                     <$   pKey "\\"
                     <*>  pList1  (    sem_Expr_Lam <$> pPatExprBase
                                  <|>  pPackImpl (flip sem_Expr_LamImpl <$> pPatExpr <* pKey "<:" <*> pPrExpr)
                                  )
                     <*   pKey "->"
%%]

%%[pExprPrefix.5.If
                <|>  (\c t e ->  sem_Expr_Case c
                                   (sem_CaseAlts_Cons (sem_CaseAlt_Pat (sem_PatExpr_Con (HNm "True")) t)
                                      (sem_CaseAlts_Cons (sem_CaseAlt_Pat (sem_PatExpr_Con (HNm "False")) e)
                                         sem_CaseAlts_Nil
                     )             )  )
                     <$ pKey "if" <*> pExpr <* pKey "then" <*> pExpr <* pKey "else"
%%]

%%[exprAlg.1
exprAlg         =    (sem_Expr_Con,sem_Expr_App
                     ,sem_Expr_AppTop,sem_Expr_Parens)
%%]

%%[pExpr.1
pExpr           =    pExprPrefix <*> pExpr
                <|>  pExprApp
%%]

%%[pExpr.4
pExpr           =    pE <??> (sem_Expr_TypeAs <$ pKey "::" <*> pTyExpr)
                where pE  =    pExprPrefix <*> pE
                          <|>  pExprApp
%%]

-- versions
%%[1.pExprBase
%%@pExprBaseCommon.1
%%@pExprBaseParenProd.1
%%]

%%[5.pExprBase -1.pExprBase
%%@pExprBaseCommon.1
%%@pExprBaseParenProd.1
%%@pExprBaseCommon.5
%%]

%%[7.pExprBase -5.pExprBase
%%@pExprBaseCommon.1
%%@pExprBaseCommon.5
%%@pExprBase.7
%%]

%%[8.pExprBase -7.pExprBase
%%@pExprBaseCommon.1
%%@pExprBaseCommon.5
%%@pExprBase.7
%%@pExprBase.8
%%]

%%[1.pExpr
%%@exprAlg.1
%%@pExpr.1
%%@pExprApp.1
%%@pExprPrefix.1
%%@pExprPrefix.1.Lam
%%]

%%[4.pExpr -1.pExpr
%%@exprAlg.1
%%@pExpr.4
%%@pExprApp.1
%%@pExprPrefix.1
%%@pExprPrefix.1.Lam
%%]

%%[5.pExpr -4.pExpr
%%@exprAlg.1
%%@pExpr.4
%%@pExprApp.1
%%@pExprPrefix.1
%%@pExprPrefix.1.Lam
%%@pExprPrefix.5.If
%%]

%%[7.pExpr -5.pExpr
%%@exprAlg.1
%%@pExpr.4
%%@pExprApp.7
%%@pExprPrefix.1
%%@pExprPrefix.7.Lam
%%@pExprPrefix.5.If
%%]

%%[9.pExpr -7.pExpr
%%@exprAlg.1
%%@pExpr.4
%%@pExprApp.9
%%@pExprPrefix.1
%%@pExprPrefix.9.Lam
%%@pExprPrefix.5.If
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for Case/Data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[pDataConstrs.5
pDataConstrs    =    pFoldrSep (sem_DataConstrs_Cons,sem_DataConstrs_Nil) (pKey "|") pDataConstr
%%]

%%[5.DataConstr
pDataConstr     =    sem_DataConstr_Constr <$> pCon <*> pTyExprs
%%@pDataConstrs.5
%%]

%%[7.DataConstr -5.DataConstr
pDataConstr     =    sem_DataConstr_Constr
                     <$> pCon <*> (pDataFields <|> pCurly pDataLabFields)
pDataField      =    sem_DataField_Field Nothing <$> pTyExprBase
pDataLabField   =    sem_DataField_Field <$> (Just <$> pList1Sep pComma pVar) <* pKey "::" <*> pTyExpr
pDataFields     =    pFoldr (sem_DataFields_Cons,sem_DataFields_Nil) pDataField
pDataLabFields  =    pFoldr1Sep (sem_DataFields_Cons,sem_DataFields_Nil) pComma pDataLabField
%%@pDataConstrs.5
%%]

%%[5
pCaseAlts       =    foldr sem_CaseAlts_Cons sem_CaseAlts_Nil
                     <$> pBlock1 pOCurly pSemi pCCurly pCaseAlt
pCaseAlt        =    sem_CaseAlt_Pat  <$>  pPatExpr <* pKey "->" <*> pExpr

pTyVars         =    pFoldr (sem_TyVars_Cons,sem_TyVars_Nil) pTyVar
pTyVar          =    sem_TyVar_Var <$> pVar
%%]

%%[9
pTyVars1        ::   EHParser T_TyVars
pTyVars1        =    pFoldr1 (sem_TyVars_Cons,sem_TyVars_Nil) pTyVar
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for Records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[7.pParenRow -1.pParenProd
data RowFld a = FldSel HsName a | FldNoSel a | FldUpd HsName a

pParenRow       ::   Bool -> String -> String -> String -> Maybe (String,r -> HsName -> e -> r)
                     -> (r,HsName -> r,r -> Maybe HsName -> e -> r,r -> e,e -> e)
                     -> EHParser HsName -> EHParser e -> EHParser e

pParenRow singleAsIs o c sep mbUpd (semEmpty,semVar,semExt,semRow,semParens) pSel pE
                =    pKey o *> pRowFlds <* pKey c
                where  pFld          =    ((pSel <**> pSep) <|> pSucceed FldNoSel) <*> pE
                       pFlds         =    pListSep pComma pFld
                       pExtFlds      =    mkE <$> (pRowNested <|> semVar <$> pVar) <* pKey "|" <*> pFlds
                       pFldsOrExt    =    mkE semEmpty <$> pFlds <|> pExtFlds
                       pRowNested    =    pKey o *> pFldsOrExt <* pKey c
                       pRowFlds      =    if singleAsIs
                                          then       pFld <**>  (    (\fs f -> mkR (f:fs)) <$ pComma <*> pFlds
                                                                <|>  pSucceed (\le -> case le of {FldNoSel e -> semParens e; _ -> mkR [le]})
                                                                )
                                                <|>  semRow <$> pExtFlds
                                                <|>  pSucceed (mkR [])
                                          else  semRow <$> pFldsOrExt
                       mkR fs        =    semRow (mkE semEmpty fs )
                       mkE ext fs    =    foldl (\r f -> case f of 
                                                            FldSel l e -> semExt r (Just l) e
                                                            FldNoSel e -> semExt r Nothing e
                                                            FldUpd l e -> semUpd r l e
                                                ) ext fs
                       (pSep,semUpd) =    case mbUpd of
                                            Just (sepUpd,sem) -> (FldSel <$ pKey sep <|> FldUpd <$ pKey sepUpd,sem)
                                            Nothing           -> (FldSel <$ pKey sep,\r _ _ -> r)
%%]

%%[7
pExprSelSuffix  ::   EHParser (T_Expr -> T_Expr)
pExprSelSuffix  =    (\lbls e -> foldl sem_Expr_Sel e lbls)
                     <$> pList (pKey "." *> pSel)

pSel            ::   EHParser HsName
pSel            =    pVar <|> pCon <|> HNPos <$> pInt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for Predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
pPrExprClass    ::   EHParser T_PrExpr
pPrExprClass    =    sem_PrExpr_Class  <$> pCon <*> pTyExprs

pPrExpr         ::   EHParser T_PrExpr
pPrExpr         =    pPrExprClass
%%]
%%[10
                <|>  pVar <**>  (    (\s v -> sem_PrExpr_Lacks (sem_RowTyExpr_Var v) s)
                                     <$ pKey "\\" <*> pSel
%%]
%%[11
                                <|>  (flip sem_PrExpr_Equal)
                                     <$ pKey "=" <*> pTyExpr
%%]
%%[10
                                )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for Class & Instance
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
pClassHead      ::   EHParser (T_PrExprs,T_PrExpr)
pClassHead      =    pPrExprClass <**>  (    (\p c -> (sem_PrExprs_Cons c sem_PrExprs_Nil,p))
                                             <$ pKey "=>" <*> pPrExprClass
                                        <|>  pSucceed (\p -> (sem_PrExprs_Nil,p))
                                        )
                <|>  (,) <$> pParens (pFoldrSep (sem_PrExprs_Cons,sem_PrExprs_Nil) pComma pPrExprClass)
                     <* pKey "=>" <*> pPrExprClass

pDeclClass      ::   EHParser T_Decl
pDeclClass      =    (uncurry sem_Decl_Class)
                     <$   pKey "class"
                     <*>  pClassHead
                     <*>  (pKey "|" *> pFoldrSep  (sem_FuncDeps_Cons,sem_FuncDeps_Nil) pComma
                                                  (sem_FuncDep_Dep <$> pTyVars1 <* pKey "->" <*> pTyVars1)
                          `opt` sem_FuncDeps_Nil
                          )
                     <*   pKey "where" <*> pDecls

pDeclInstance   ::   EHParser T_Decl
pDeclInstance   =    pKey "instance"
                     *>   (    (\ne -> uncurry (sem_Decl_Instance ne))
                               <$>  ((\n e -> Just (n,e)) <$> pVar <*> (True <$ pKey "<:" <|> False <$ pKey "::") `opt` Nothing)
                               <*>  pClassHead
                               <*   pKey "where" <*> pDecls
                          <|>  sem_Decl_InstanceIntro <$> pExpr <* pKey "<:" <*> pPrExprClass
                          )
%%]

