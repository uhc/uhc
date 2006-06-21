%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}HS.Parser} import(IO, UU.Parsing, UU.Parsing.Offside, UU.Scanner.GenToken, EH.Util.ScanUtils, {%{EH}Base.Common}, {%{EH}Base.ScannerCommon}, {%{EH}HS})
%%]

%%[1 export(pAGItf)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scanner related
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
tokConcat :: Token -> Token -> Token
tokConcat t1 t2 = Reserved (genTokVal t1 ++ genTokVal t2) (position t1)

tokEmpty :: Token
tokEmpty = Reserved "" noPos
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
type HSParser         ep    =    (IsParser (OffsideParser i o Token p) Token,InputState i Token p, OutputState o, Position p)
                                    => OffsideParser i o Token p ep

pAGItf :: HSParser AGItf
pAGItf = AGItf_AGItf <$> pModule
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abstractions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
pPacked' :: HSParser Token -> HSParser Token -> HSParser (Range -> v) -> HSParser v
pPacked' pO pC pMk = (\o mk c -> mk (mkRange2 o c)) <$> pO <*> pMk <*> pC

pParens' :: HSParser (Range -> v) -> HSParser v
pParens' = pPacked' pOPAREN pCPAREN

pBracks' :: HSParser (Range -> v) -> HSParser v
pBracks' = pPacked' pOBRACK pCBRACK

pCurlys' :: HSParser (Range -> v) -> HSParser v
pCurlys' = pPacked' pOCURLY pCCURLY
%%]

%%[9
pImpls' :: HSParser (Range -> v) -> HSParser v
pImpls' = pPacked' pOIMPL pCIMPL

pImpls :: HSParser v -> HSParser v
pImpls = pPacked pOIMPL pCIMPL
%%]

%%[1.pApp
pApp            ::   SemApp ep => HSParser ep -> HSParser ep
pApp p          =    mkApp <$> pList1 p
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Names, misc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
commas :: HSParser Token 
commas =  genTokMap (strProd . length) <$> pFoldr (tokConcat,tokEmpty) pCOMMA
%%]

%%[1
modid :: HSParser Token
modid =   pCONID
%%]
%%[8
      <|> pQCONID
%%]

%%[8
qcnames :: HSParser [Token] 
qcnames =  pListSep pCOMMA qcname

qcname  :: HSParser Token   -- Variable or data constructor
qcname  =  qvar <|> gcon                    
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Names, general
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
identifier :: HSParser Token 
identifier =  qvar      
          <|> gcon  
          <|> qop       

depreclist :: HSParser [Token] 
depreclist = pList1Sep pCOMMA deprec_var

deprec_var :: HSParser Token
deprec_var = var <|> tycon          

gcon    :: HSParser Token   -- Data constructor namespace
gcon    =  sysdcon      
       <|> qcon         
-- the case of '[:' ':]' is part of the production `parr'

sysdcon :: HSParser Token   -- Wired in data constructors
sysdcon =  pParens commas 
       <|> tokConcat <$> pOBRACK <*> pCBRACK

var     :: HSParser Token
var =  varid            
   <|> pParens varsym 

qvar    :: HSParser Token
qvar =  qvarid      
    <|> pParens
          (   varsym
%%]
%%[8
          <|> qvarsym1
%%]
%%[1
          )
-- We've inlined qvarsym here so that the decision about
-- whether it's a qvar or a var can be postponed until
-- *after* we see the close paren.

{-
ipvar   :: HParser (IPName RdrName)
ipvar =  liftM (Dupable . mkUnqual varName) <$> pDUPIPVARID
     <|> liftM (Linear . mkUnqual varName)  <$> pSPLITIPVARID
-}

qcon    :: HSParser Token
qcon    = qconid <|> pParens qconsym


varop   :: HSParser Token
varop   =  varsym 
       <|> pBACKQUOTE *> varid <* pBACKQUOTE
       
qvarop :: HSParser Token
qvarop = qvarsym    
       <|> pBACKQUOTE *> qvarid <* pBACKQUOTE

qvaropm :: HSParser Token
qvaropm =  qvarsym_no_minus 
       <|> pBACKQUOTE *> qvarid <* pBACKQUOTE

conop :: HSParser Token
conop =  consym     
       <|> pBACKQUOTE *> conid <* pBACKQUOTE

qconop :: HSParser Token
qconop =  qconsym       
      <|> pBACKQUOTE *> qconid <* pBACKQUOTE

%%]

%%[1
-----------------------------------------------------------------------------
-- Variables 

qvarsym :: HSParser Token 
qvarsym =  varsym
%%]
%%[8
       <|> qvarsym1
%%]

%%[1
qvarsym_no_minus :: HSParser Token
qvarsym_no_minus =  varsym_no_minus
%%]
%%[8
                <|> qvarsym1
%%]

%%[8
qvarsym1 :: HSParser Token
qvarsym1 = pQVARSYM 
%%]

%%[1
varsym :: HSParser Token 
varsym  =  varsym_no_minus  
       <|> pMINUS       


varsym_no_minus :: HSParser Token  -- varsym not including '-'
varsym_no_minus = pVARSYM <|> special_sym

-- See comments with special_id
special_sym :: HSParser Token 
special_sym 
        =  pBANG    
       <|> pDOT     
       <|> pSTAR    

-----------------------------------------------------------------------------
-- Data constructors

qconid :: HSParser Token    -- Qualified or unqualifiedb
qconid =  conid
%%]
%%[8
      <|> pQCONID
%%]

%%[1
conid   :: HSParser Token
conid   =  pCONID           

qconsym :: HSParser Token   -- Qualified or unqualified
qconsym = consym
%%]
%%[8
       <|> pQCONSYM
%%]

%%[1
consym :: HSParser Token
consym =  pCONSYM       
      <|> pCOLON
    -- ':' means only list cons
    -- NB: SrcName because we are reading source
%%]

%%[1
con :: HSParser Token
con = conid <|> pParens consym
%%]

%%[1
-----------------------------------------------------------------------------
-- Any operator

op  :: HSParser Token   -- used in infix decls
op  = varop <|> conop

qop :: HSParser Token   -- used in sections
qop = qvarop <|> qconop

qopm    :: HSParser  Token    -- used in sections
qopm    = qvaropm <|> qconop

-----------------------------------------------------------------------------
-- VarIds

qvarid :: HSParser Token
qvarid = varid
%%]
%%[8
     <|> pQVARID
%%]

%%[1
varid :: HSParser Token
varid = varid_no_unsafe
%%]
%%[8
     <|> pUNSAFE        
     <|> pSAFE      
     <|> pTHREADSAFE 
%%]

%%[1
varid_no_unsafe :: HSParser Token
varid_no_unsafe =  pVARID
%%]
%%[4
               <|> pFORALL
%%]
%%[8
               <|> special_id
%%]

%%[1
tyvar   :: HSParser Token
tyvar   =  pVARID
%%]
%%[8
       <|> special_id       
       <|> pUNSAFE
       <|> pSAFE
       <|> pTHREADSAFE      
%%]

%%[8
-- These special_ids are treated as keywords in various places, 
-- but as ordinary ids elsewhere.   'special_id' collects all these
-- except 'unsafe' and 'forall' whose treatment differs depending on context
special_id :: HSParser Token 
special_id =
       pAS      
   <|> pQUALIFIED   
   <|> pHIDING
   <|> pEXPORT  
   <|> pLABEL   
   <|> pDYNAMIC
   <|> pSTDCALL 
   <|> pCCALL    
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Misc abstractions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
sepblock :: HSParser sep -> HSParser a -> HSParser [a]
sepblock sep p =  glue <$> pList1Sep sep ((:) <$> p `opt` id ) 
  where glue xs = foldr (.) id xs []

block items
  =    pOCURLY   *>  items <* pCCURLY
  <|>  pVOCURLY  *>  items <* close

close :: HSParser Token
close = pVCCURLY
%%]

close :: HParser () 
close = pWrap f g (pVCCURLY)
  where g state steps1 k = (state,ar,k)
          where ar = if not (hasSuccess steps1) 
                       then case unP popContext state of
                             POk state' _   -> let steps2 = k state'
                                               in  if  hasSuccess steps2 then steps2 else steps1                      
                             _              -> steps1  
                       else steps1                             
        f acc state steps k = let (stl,ar,str2rr) = g state (val snd steps)  k
                              in (stl ,val (acc (return ())) ar , str2rr )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
pModule :: HSParser Module
%%]
%%[1.pModule
pModule
  =   (\b -> Module_Module emptyRange Nothing b) <$> pBody
  <|> (\t m b -> Module_Module (mkRange1 t) (Just $ mkQName $ m) b) <$> pMODULE <*> modid <* pWHERE <*> pBody
%%]
%%[8.pModule -1.pModule
pModule
  =   (\b -> Module_Module emptyRange Nothing Nothing b) <$> pBody
  <|> (\t m e b -> Module_Module (mkRange1 t) (Just $ mkQName $ m) e b) <$> pMODULE <*> modid <*> pMaybeExports <* pWHERE <*> pBody
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module body
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
pBody :: HSParser Body
pBody
  = Body_Body emptyRange
%%]
%%[8
      []
%%]
%%[1
    <$> pDeclarations
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Declarations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
pDeclaration :: HSParser Declaration
pDeclaration
  =   pDeclarationValue
%%]
%%[5
  <|> pDeclarationData
%%]
%%[9
  <|> pDeclarationClass
  <|> pDeclarationInstance
%%]

%%[1
pDeclarations :: HSParser Declarations
pDeclarations
  =   pBlock pOCURLY pSEMI pCCURLY pDeclaration

pDeclarations1 :: HSParser Declarations
pDeclarations1
  =   pBlock1 pOCURLY pSEMI pCCURLY pDeclaration
%%]

%%[1
pWhere :: HSParser MaybeDeclarations
pWhere = Just <$ pWHERE <*> pDeclarations `opt` Nothing
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for Class & Instance
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
pClassHead      ::   HSParser Type
pClassHead      =    pTyPrefixContextItems <*> pHd <|> pHd
                where pHd = Type_Pred <$> pContextItemClass
%%]

%%[9
pDeclarationClass :: HSParser Declaration
pDeclarationClass
  = (\t -> Declaration_Class (mkRange1 t))
    <$> pCLASS
    <*> pContextItemsPrefixOpt <*> pSimpleType
    <*> (pVBAR *> pListSep pCOMMA pFunctionalDependency
        `opt` []
        )
    <*> pWhere
  where pFunctionalDependency :: HSParser FunctionalDependency
        pFunctionalDependency
          = (\vs1@(v:_) vs2 -> FunctionalDependency_Dependency (mkRange1 v) (mkQNames vs1) (mkQNames vs2))
            <$> pList1 tyvar <* pRARROW <*> pList1 tyvar
%%]

%%[9
pDeclarationInstance :: HSParser Declaration
pDeclarationInstance
  = pINSTANCE
    <**> (   (\(n,u) c cl ts d t -> Declaration_Instance (mkRange1 t) n u c (mkQName cl) ts d)
             <$> ((\n e -> (Just (mkQName n),e)) <$> varid <*> (True <$ pLTCOLON <|> False <$ pDCOLON) <|> pSucceed (Nothing,False))
             <*> pContextItemsPrefixOpt <*> qconid <*> pList1 pType
             <*> pWhere
         <|> (\e cl ts t -> Declaration_InstanceUseImplicitly (mkRange1 t) e (mkQName cl) ts)
             <$> pExpression <* pLTCOLON <*> qconid <*> pList1 pType
         )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Value definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
pDeclarationValue :: HSParser Declaration
pDeclarationValue
  =   (\(v:vs) t -> Declaration_TypeSignature (mkRange1 v) (mkQNames (v:vs)) t)
      <$> pList1Sep pCOMMA var <* pDCOLON <*> pType
  <|> (\l r -> Declaration_FunctionBindings emptyRange [FunctionBinding_FunctionBinding emptyRange l r]) <$> pLhs <*> rhs
  <|> Declaration_PatternBinding emptyRange <$> pPatternOp <*> rhs
  where rhs = pRhs pEQUAL
%%]

%%[1
pRhs :: HSParser Token -> HSParser RightHandSide
pRhs pSep
  =   (RightHandSide_Expression . mkRange1) <$> pSep <*> pExpression <*> pWhere
%%]
%%[5
  <|> RightHandSide_Guarded emptyRange
      <$> pList1 ((GuardedExpression_GuardedExpression . mkRange1) <$> pVBAR <*> pExpression <* pSep <*> pExpression)
      <*> pWhere
%%]

%%[1
pLhs :: HSParser LeftHandSide
pLhs
  =   mkRngNm LeftHandSide_Function <$> qvar <*> pLhsTail
  <|> pParens' ((\l r t -> LeftHandSide_Parenthesized r l t) <$> pLhs) <*> pLhsTail
  <|> (\l o r -> LeftHandSide_Infix (mkRange1 o) l (mkQName o) r) <$> pPatternOp <*> varop <*> pPatternOp
  where pLhsTail = pList1 pPatternBase
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Data definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[5
pDeclarationData :: HSParser Declaration
pDeclarationData
  = (Declaration_Data . mkRange1) <$> pDATA
%%]
%%[9
    <*> pContextItemsPrefixOpt
%%]
%%[5
    <*> pSimpleType <*> (pEQUAL *> pConstructors `opt` [])
%%]
%%[9
    <*  pDERIVING <*> (mkQNames <$> ((:[]) <$> qconid <|> pParens (pList1Sep pCOMMA qconid)))
%%]

%%[5.pConstructor
pConstructor :: HSParser Constructor
pConstructor
  =   con
      <**> (   (\ts c -> mkRngNm Constructor_Constructor c ts) <$> pList pTB
%%]
%%[7
           <|> pCurlys' ((\fs r c -> mkRngNm Constructor_Record c fs) <$> pList1Sep pCOMMA pFieldDeclaration)
%%]
%%[5
           )
  <|> (\l o r -> Constructor_Infix (mkRange1 o) l (mkQName o) r) <$> pT <*> conop <*> pT
  where pT  = pAnnotatedType pType
        pTB = pAnnotatedType pTypeBase
%%]

%%[5
pConstructors :: HSParser Constructors
pConstructors
  = pListSep pVBAR pConstructor
%%]

%%[7
pFieldDeclaration :: HSParser FieldDeclaration
pFieldDeclaration
  = (\vs@(v:_) -> FieldDeclaration_FieldDeclaration (mkRange1 v) (mkQNames vs))
    <$> pList1Sep pCOMMA var <* pDCOLON <*> pAnnotatedType pType
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Type signatures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
sig_vars :: HSParser [Token] 
sig_vars =  pList1Sep pCOMMA var
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Export
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
pExport :: HSParser Export
pExport
  =   (\t m -> Export_Module (mkRange1 t) (mkQName m)) <$> pMODULE <*> modid
  <|> mkRngNm Export_Variable <$> qvar
  <|> oqtycon
      <**> (   pParens
                 (   (\c n -> mkRngNm Export_TypeOrClass n (Just (mkQNames c))) <$> qcnames
                 <|> mkRngNm Export_TypeOrClassComplete <$ pDOTDOT
                 )
           <|> pSucceed (\n -> mkRngNm Export_TypeOrClass n Nothing)
           )

pExports :: HSParser Exports
pExports = sepblock pCOMMA pExport

pMaybeExports :: HSParser MaybeExports
pMaybeExports = Just <$> pParens pExports `opt` Nothing         
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Type
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
pTypeBase :: HSParser Type
pTypeBase
  =   mkRngNm Type_Constructor <$> gtycon
%%]
%%[2.pTypeBase
  <|> (Type_Wildcard . mkRange1) <$> pTDOT
%%]
%%[3.pTypeBase
  <|> mkRngNm Type_Variable <$> tyvar
  <|> mkRngNm Type_NamedWildcard <$ pPERCENT <*> tyvar
%%]
%%[1.pTypeBase.prod
  <|> pParens' pInParens
  where pInParens :: HSParser (Range -> Type)
        pInParens
          =   (pType
               <**> (   (\es e r -> Type_Application r False (Type_Constructor r $ hsnProd $ length es + 1) (e:es))
                        <$>  pList1 (pComma *> pType)
                    <|> pSucceed (flip Type_Parenthesized)
              )     )
          <|> pSucceed (\r -> Type_Constructor r (hsnProd 0))
%%]
%%[7 -1.pTypeBase.prod
  <|> pParenRow True pOROWREC pCROWREC pDCOLON False undefined undefined
         (Type_RowRecEmpty,Type_Variable,RowTypeUpdate_Extends,Type_RowRecUpdate,Type_Parenthesized)
         qvarid pType
  <|> pParenRow False pOROWROW pCROWROW pDCOLON False undefined undefined
         (Type_RowEmpty,Type_Variable,RowTypeUpdate_Extends,Type_RowUpdate,const id)
         qvarid pType
  <|> pParenRow False pOROWSUM pCROWSUM pDCOLON False undefined undefined
         (Type_RowSumEmpty,Type_Variable,RowTypeUpdate_Extends,Type_RowSumUpdate,const id)
         qvarid pType
%%]

%%[1.pType
pType           ::   HSParser Type
pType           =    pChainr (mk1Arrow <$ pRARROW) pTypeBase
%%]
%%[4.pType -1.pType
pType           ::   HSParser Type
pType           =    pTypePrefix <*> pType
                <|>  pTypeApp <??> (flip mk1Arrow <$ pRARROW <*> pType)
%%]

%%[4.pTypePrefix
pTypePrefix     ::   HSParser (Type -> Type)
pTypePrefix     =    ((Type_Forall . mkRange1) <$> pFORALL <|> (Type_Exists . mkRange1) <$> pEXISTS)
                     <*> (mkQNames <$> pTyVarBinds) <* pDOT
%%]
%%[9.pTypePrefix
                <|>  pTyPrefixContextItems
%%]

%%[4.pTypeApp
pTypeApp        ::   HSParser Type
pTypeApp        =    pTypeBase
%%]
%%[5.pTypeApp -4.pTypeApp
pTypeApp        ::   HSParser Type
pTypeApp        =    pApp pTypeBase
%%]

%%[4
pTyVarBind      ::   HSParser Token
pTyVarBind      =    tyvar

pTyVarBinds     ::   HSParser [Token]
pTyVarBinds     =    pList1 pTyVarBind
%%]

%%[5
pSimpleType :: HSParser SimpleType
pSimpleType
  = mkRngNm SimpleType_SimpleType <$> gtycon <*> (mkQNames <$> pList tyvar)
%%]

%%[5
pAnnotatedType :: HSParser Type -> HSParser AnnotatedType
pAnnotatedType pT
  = AnnotatedType_Type emptyRange False <$> pT
%%]

%%[9.pTyPrefixContextItems
pContextItemsPrefixOpt :: HSParser ContextItems
pContextItemsPrefixOpt = pContextItemsPrefix <|> pSucceed []

pContextItemsPrefix :: HSParser ContextItems
pContextItemsPrefix
  =   mkL
      <$> pImpls'
            (    const <$> (pContextItem <|> pIm)
            <|>  pSucceed ContextItem_NoImplicits
            )
      <*  pRARROW
  <|> (   mkL <$> (pContextItemBase <|> pIm)
      <|> pParens ((:) <$> pContextItem
                       <*> (   pImO
                           <|> (++) <$> pList1 (pCOMMA *> pContextItem) <*> pImO
                  )        )
      )
      <*  pDARROW
  where pIm   =   (ContextItem_Implicits . mkRange1) <$> pTDOT
        pImO  =   (:[]) <$ pCOMMA <*> pIm `opt` []
        mkL   =   (:[])
        pIm  :: HSParser ContextItem
        pImO :: HSParser ContextItems

pTyPrefixContextItems :: HSParser (Type -> Type)
pTyPrefixContextItems
  = Type_Qualified emptyRange <$> pContextItemsPrefix
%%]

%%[9.pPackImpl
%%]
pPackImpl       ::   HSParser p -> HSParser p
pPackImpl       =    pPacked (pKeyw hsnOImpl) (pKeyw hsnCImpl)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for Predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
pContextItemClass :: HSParser ContextItem
pContextItemClass
  =    mkRngNm ContextItem_Class <$> qconid <*> pList1 pType
%%]

%%[9
pContextItemPrefix :: HSParser (ContextItem -> ContextItem)
pContextItemPrefix
  =   ContextItem_Arrow emptyRange <$> pContextItemBase <* pDARROW
  <|> (ContextItem_Forall . mkRange1) <$> pFORALL <*> (mkQNames <$> pTyVarBinds) <* pDOT
%%]

%%[9
pContextItem :: HSParser ContextItem
pContextItem
  =   pContextItemPrefix <*> pContextItem
  <|> pContextItemBase
%%]

%%[9
%%]
pTyContextItem :: HSParser Type
pTyContextItem
  =   pTyPrefixContextItems <*> pTyContextItem
  <|> (\c -> Type_Qualified emptyRange [c]) <$> pContextItemBase

%%[9
pContextItemBase ::   HSParser ContextItem
pContextItemBase
  =   pContextItemClass
  <|> pParens pContextItem
%%]
%%[1010
  <|>  ContextItem_DynVar <$> pDynVar <* pKey "::" <*> pType
%%]
%%[10
  <|> tyvar <**>  (    (\s v -> mkRngNm ContextItem_RowLacksLabel v (mkQName s))
                       <$ pLAM <*> pSelector
%%]
%%[11
                  <|>  (flip ContextItem_Equal)
                       <$ pKey "=" <*> pType
%%]
%%[10
                  )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Type constructors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
gtycon  :: HSParser Token   -- A "general" qualified tycon
gtycon  =   oqtycon
        <|> pParens
              (   commas
              <|> pRARROW
              )
%%]
%%[5
        <|> tokConcat <$> pOBRACK <*> pCBRACK
%%]
%%[99
        <|> tokConcat <$> pOPABRACK <*> pCPABRACK
%%]

%%[1
tycon   :: HSParser Token   -- Unqualified
tycon   = pCONID                

oqtycon :: HSParser Token   -- An "ordinary" qualified tycon
oqtycon =  qtycon
       <|> pParens qtyconsym  

qtycon :: HSParser Token    -- Qualified or unqualified
qtycon =  tycon
%%]
%%[8
      <|> pQCONID
%%]

%%[1      
qtyconsym :: HSParser Token
qtyconsym =  tyconsym
%%]
%%[8
         <|> pQCONSYM
%%]

%%[1
tyconsym :: HSParser Token
tyconsym = pCONSYM          
%%]

%%[8
qtyconop :: HSParser Token  -- Qualified or unqualified
qtyconop = qtyconsym
       <|> pBACKQUOTE *> qtycon <* pBACKQUOTE

tyconop :: HSParser Token   -- Unqualified
tyconop = tyconsym  
       <|> pBACKQUOTE *> tycon <* pBACKQUOTE
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Literal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
pLiteralNumber :: HSParser Literal
pLiteralNumber
  =   mkRngStr Literal_Int  <$> pIntegerTk

pLiteral :: HSParser Literal
pLiteral
  =   pLiteralNumber
  <|> mkRngStr Literal_Char <$> pCharTk
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Expression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
pExpressionBase :: HSParser Expression
pExpressionBase
  =   Expression_Literal emptyRange  <$> pLiteral
  <|> mkRngNm Expression_Variable    <$> qvar
  <|> gcon
      <**> (   pSucceed (mkRngNm Expression_Constructor)
%%]
%%[7
           <|> pCurlys' ((\bs _ c -> mkRngNm Expression_RecordConstruction c bs) <$> pListSep pCOMMA pRecordExpressionBinding)
%%]
%%[1
           )
%%]
%%[5
  <|> pExpressionList
%%]
%%[1
  <|> pParens' pInParens
  where pInParens :: HSParser (Range -> Expression)
        pInParens
          =   (pExpression
               <**> (   (\o e r -> Expression_InfixApplication r (Just e) (mkOpExpr o) Nothing) <$> qop
%%]
%%[1.parenExprAsProdA
                    <|> pSucceed (flip Expression_Parenthesized)
                    <|> (\es e r -> Expression_NormalApplication r (Expression_Constructor r $ hsnProd $ length es + 1) (e:es))
                        <$>  pList1 (pComma *> pExpression)
%%]
%%[1
              )     )
          <|> (\o e r -> Expression_InfixApplication r Nothing (mkOpExpr o) (Just e)) <$> qopm <*> pExpression
%%]
%%[1.parenExprAsProdB
          <|> pSucceed (\r -> Expression_Constructor r (hsnProd 0))
%%]
%%[7 -(1.parenExprAsProdA 1.parenExprAsProdB)
          <|> pParenRow' True pOROWREC pCROWREC pEQUAL True pCOLEQUAL RowRecordExpressionUpdate_Update
                 (Expression_RowRecordEmpty,Expression_Variable,RowRecordExpressionUpdate_Extends,Expression_RowRecordUpdate,Expression_Parenthesized)
                 qvarid pExpression
%%]
%%[1
        mkOpExpr = mkRngNm Expression_Variable
%%]

%%[5
pExpressionList :: HSParser Expression
pExpressionList
  = pBracks'
      (pExpression
       <**> (   pDOTDOT
                *> (     (\e3 e1 r -> Expression_Enum r e1 Nothing (Just e3)) <$> pExpression
                   `opt` (\   e1 r -> Expression_Enum r e1 Nothing  Nothing )
                   )
            <|> pCOMMA
                *> (pExpression
                    <**> (   pDOTDOT
                             *> (     (\e3 e2 e1 r -> Expression_Enum r e1 (Just e2) (Just e3)) <$> pExpression
                                `opt` (\   e2 e1 r -> Expression_Enum r e1 (Just e2)  Nothing )
                                )
                         <|> (\es e2 e1 r -> Expression_List r (e1:e2:es)) <$> pList (pComma *> pExpression)
                   )     )
            <|> pVBAR
                *> ((\c e r -> Expression_Comprehension r e (c ++ [Qualifier_Empty emptyRange])) <$> pListSep pCOMMA pQualifier) 
            `opt` flip one
            )
      `opt` zero
      )
  where zero r   = Expression_List r []
        one  r h = Expression_List r [h]
        pQualifier :: HSParser Qualifier
        pQualifier
          =   Qualifier_Guard emptyRange <$> pExpression
          <|> (Qualifier_Let . mkRange1) <$> pLET <*> pDeclarations
          <|> Qualifier_Generator emptyRange <$> pPattern <* pLARROW <*> pExpression
%%]

%%[88.pExprBase
                <|>  Expr_Undefined  <$   pKey "..."
%%]
%%[1010.pExprBase
                <|>  Expr_DynVar     <$>  pDynVar
%%]

%%[1
pExpressionUpd :: HSParser Expression
pExpressionUpd
  = pExpressionBase
%%]
%%[7
    <**> (   pCurlys'
               ((\bs r e -> Expression_RecordUpdate r e bs) <$> pListSep pCOMMA pRecordExpressionBinding
               )
         <|> pRowRecordSelectionSuffix
         <|> pSucceed id
         )
%%]

%%[7
pRecordExpressionBinding :: HSParser RecordExpressionBinding
pRecordExpressionBinding
  = mkRngNm RecordExpressionBinding_RecordExpressionBinding <$> qvar <* pEQUAL <*> pExpression
%%]

%%[1.pExpressionApp
pExpressionApp :: HSParser Expression
pExpressionApp
  = pE <??> pA
  where pE =   pExpressionUpd
        pA =   (\es e -> Expression_NormalApplication emptyRange e es) <$> pList1 pE
%%]
%%[4
           <|> (\es e -> Expression_ImpredicativeApplication emptyRange e es) <$> pList1 (pTILDE *> pE)
%%]
%%[9
           <|> (\es e -> Expression_ImplicitApplication emptyRange e es) <$> pList1 (pImpls' pContextedExpression)
           where pContextedExpression = (\e c r -> ContextedExpression_Contexted r e c) <$> pExpression <* pLTCOLON <*> pContextItem
                 pContextedExpression :: HSParser (Range -> ContextedExpression)
%%]

%%[1
pExpressionLayout :: HSParser Expression
pExpressionLayout
  =   pExpressionApp
%%]
%%[5
  <|> (Expression_Case . mkRange1) <$> pCASE <*> pExpression <* pOF <*> pAlternatives
%%]

%%[1
pExpressionOp :: HSParser Expression
pExpressionOp
  =   pChainr_ng
        ((\o l r -> Expression_InfixApplication (mkRange1 o) (Just l) (mkRngNm Expression_Variable o) (Just r)) <$> qop)
        pExpressionLayout
%%]

%%[1
pExpression :: HSParser Expression
pExpression
  = pE <??> ((\c t e -> Expression_Typed (mkRange1 c) e t) <$> pDCOLON <*> pType)
  where pE =   pExpressionPrefix <*> pE
           <|> pExpressionOp
%%]

%%[1.pExprPrefix
pExpressionPrefix :: HSParser (Expression -> Expression)
pExpressionPrefix
  =   (Expression_Let . mkRange1) <$> pLET <*> pDeclarations <* pIN
  <|> (Expression_Negate . mkRange1) <$> pMINUS
%%]
%%[5.pExprPrefix
  <|> (Expression_If . mkRange1) <$> pIF <*> pExpression <* pTHEN <*> pExpression <* pELSE
%%]
%%[1.pExprPrefixLam
  <|> pLAM <**> pLamArgs
  where pLamArgs
          =   (\a1 a2 t e -> a1 t (a2 t e))
              <$> (   (\ps t e -> Expression_Lambda (mkRange1 t) ps e) <$> pList1 pPatternBase
%%]
%%[9
                  <|> (\ps t e -> Expression_ImplicitLambda (mkRange1 t) ps e) <$> pList1 (pImpls' pContextedPattern)
%%]
%%[1
                  )
              <*> pLamArgs
          <|> (\_ e -> e) <$ pRARROW
%%]
%%[9
        pContextedPattern = (\p c r -> ContextedPattern_Contexted r p c) <$> pPattern <* pLTCOLON <*> pContextItem
        pContextedPattern :: HSParser (Range -> ContextedPattern)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Alternatives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[5
pAlternative :: HSParser Alternative
pAlternative
  = Alternative_Alternative emptyRange <$> pPattern <*> pRhs pRARROW

pAlternatives :: HSParser Alternatives
pAlternatives
  = pBlock1 pOCURLY pSEMI pCCURLY pAlternative
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pattern
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.pPatternBase
pPatternBase :: HSParser Pattern
pPatternBase
  =   qvar <**> (   (\a p v -> Pattern_As (mkRange1 a) (mkQName v) p) <$> pAT <*> pPatternBase
                <|> pSucceed (mkRngNm Pattern_Variable)
                )
  <|> Pattern_Literal emptyRange <$> pLiteral
  <|> (Pattern_Negate . mkRange1) <$> pMINUS <*> pLiteralNumber
%%]
%%[5
  <|> pBracks' (flip Pattern_List <$> pListSep pCOMMA pPattern)
%%]
%%[8
  <|> (Pattern_Irrefutable . mkRange1) <$> pTILDE <*> pPatternBase
%%]
%%[1.pPatternBase.prod
  <|> pParens' pInParens
  where pInParens :: HSParser (Range -> Pattern)
        pInParens
          =   (pPattern
               <**> (   (\es e r -> Pattern_Tuple r (e:es))
                        <$>  pList1 (pComma *> pPattern)
                    <|> pSucceed (flip Pattern_Parenthesized)
              )     )
          <|> pSucceed (\r -> Pattern_Constructor r (hsnProd 0) [])
%%]
%%[7 -1.pPatternBase.prod
  <|>   pParenRow True pOROWREC pCROWREC pEQUAL False undefined undefined
          (Pattern_RowRecordEmpty,Pattern_Variable,RowRecordPatternBinding_Binding,Pattern_RowRecordBinding,Pattern_Parenthesized)
          pSelector pPattern
%%]

%%[1
pPatternApp :: HSParser Pattern
pPatternApp
  =   mkRngNm Pattern_Constructor <$> qconid <*> pList pPatternBase
  <|> pPatternBase
%%]

%%[1
pPatternOp :: HSParser Pattern
pPatternOp
  =   pChainr_ng
        ((\o l r -> mkRngNm Pattern_Constructor o [l,r]) <$> qconop)
        pPatternApp
%%]

%%[1.pPattern
pPattern :: HSParser Pattern
pPattern
  =   pPatternOp
%%]
%%[4.patExpr
      <??> ((\c t p -> Pattern_Typed (mkRange1 c) p t) <$> pDCOLON <*> pType)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Row
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[7
data RowFld a = FldSel Token a | FldNoSel a | FldUpd Token a
%%]

%%[7
pParenRow
  ::   Bool -> HSParser Token -> HSParser Token -> HSParser Token -> Bool -> HSParser Token -> (Range -> HsName -> e -> r)
       -> (Range -> e,Range -> HsName -> e,Range -> Maybe HsName -> e -> r,Range -> e -> [r] -> e,Range -> e -> e)
       -> HSParser Token -> HSParser e -> HSParser e
pParenRow singleAsIs pO pC pS allowUpd pSUpd' semUpd' sems pSel pE
  = pPacked' pO pC (pParenRow' singleAsIs pO pC pS allowUpd pSUpd' semUpd' sems pSel pE)
%%]

%%[7
pParenRow'
  ::   Bool -> HSParser Token -> HSParser Token -> HSParser Token -> Bool -> HSParser Token -> (Range -> HsName -> e -> r)
       -> (Range -> e,Range -> HsName -> e,Range -> Maybe HsName -> e -> r,Range -> e -> [r] -> e,Range -> e -> e)
       -> HSParser Token -> HSParser e -> HSParser (Range -> e)
pParenRow' singleAsIs pO pC pS allowUpd pSUpd' (semUpd' :: Range -> HsName -> e -> r) (semEmpty,semVar,semExt,semRow,semParens) pSel (pE :: HSParser e)
  = pRowFlds
  where pFld          = (pSel <**> pSep <|> pSucceed FldNoSel) <*> pE
        pFlds         = pListSep pComma pFld
        pExtFlds      = (\e fs rng -> mkR rng e fs)
                        <$> (   pPacked' pO pC pExtFlds
                            <|> mkRngNm semVar <$> qvarid
                            )
                        <* pVBAR <*> pFlds
        pRowFlds      =   pFld <**> mkS ((\fs f rng -> mkR rng (semEmpty rng) (f:fs)) <$ pComma <*> pFlds)
                      <|> pExtFlds <|> pSucceed semEmpty
                      where mkS = if singleAsIs
                                  then (\p -> p <|> pSucceed (\le rng -> case le of {FldNoSel e -> semParens rng e; _ -> mkR rng (semEmpty rng) [le]}))
                                  else id
        mkR rng e fs  = semRow rng e (mkE fs)
        mkE           = map (\f -> case f of 
                                     FldSel   l e -> semExt (mkRange1 l) (Just (mkQName l)) e
                                     FldNoSel   e -> semExt emptyRange Nothing e
                                     FldUpd   l e -> mkRngNm semUpd l e
                            )
        pSep          = if allowUpd then FldSel <$ pS <|> FldUpd <$ pSUpd' else FldSel <$ pS
        semUpd        = if allowUpd then semUpd'                           else \_ _ _ -> undefined
        pFld :: HSParser (RowFld e)
        pFlds :: HSParser [RowFld e]
        pSep :: HSParser (Token -> a -> RowFld a)
        pRowFlds, pExtFlds :: HSParser (Range -> e)
        semUpd :: Range -> HsName -> e -> r
%%]

%%[7
pRowRecordSelectionSuffix :: HSParser (Expression -> Expression)
pRowRecordSelectionSuffix
  = (\lbls e -> foldl (\e l -> Expression_RowRecordSelect (mkRange1 l) e (mkQName l)) e lbls)
    <$> pList1 (pRARROW *> pSelector)
%%]

%%[7
pSelector :: HSParser Token
pSelector
  =   qvarid <|> qconid <|> pIntegerTk
%%]
