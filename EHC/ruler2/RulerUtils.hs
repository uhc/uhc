-- $Id: EHTyFitsIn.chs 214 2005-05-28 17:52:29Z atze $

module RulerUtils where

import IO
import Data.Maybe
import Data.Char
import Data.List
import Data.Graph
import qualified Data.Set as Set
import qualified Data.Map as Map
import FPath
import Utils
import PPUtils
import UU.Pretty
import qualified UU.DData.Scc as Scc
import UU.Scanner.Position( noPos, Pos, Position(..) )
import UU.Scanner.GenToken
import UU.Parsing

import Debug.Trace

-------------------------------------------------------------------------
-- Symbol position
-------------------------------------------------------------------------

type SPos = (String,Pos)

emptySPos = ("",noPos)

-------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------

data Err
  = Err_UndefNm SPos String String [Nm]
  | Err_NoJdSc  SPos String [Nm]
  | Err_Match   SPos String PP_Doc PP_Doc
  deriving Show

ppErr :: Position pos => (String,pos) -> PP_Doc -> PP_Doc
ppErr (sym,pos) p
  = "*** ERROR ***"
    >-< ppPos pos >|< (if null sym then empty else ", at symbol '" >|< pp sym >|< "'") >|< ":"
    >-< indent 4 p

ppErrPPL :: PP a => [a] -> PP_Doc
ppErrPPL = vlist . map pp

instance PP Err where
  pp (Err_UndefNm pos cx knd nmL)
    = ppErr pos ("In" >#< cx >#< knd >|< "(s) are undefined:" >#< ppCommas nmL)
  pp (Err_NoJdSc pos cx nmL)
    = ppErr pos ("In" >#< cx >#< "no (tex) judgement scheme for:" >#< ppCommas nmL)
  pp (Err_Match pos cx given reqd)
    = ppErr pos ("In" >#< cx >#< "could not match"
                 >-< indent 2
                       (    "scheme judgement expr:" >#< reqd
                        >-< "given view expr      :" >#< given
                       )
                )

-------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------

parseToResMsgs :: (Symbol s,InputState inp s pos) => AnaParser inp Pair s pos a -> inp -> (a,[Message s pos])
parseToResMsgs p inp
  = (r,getMsgs steps)
  where steps = parse p inp
        (Pair r _) = evalSteps steps

instance (Eq s, Show s, Show p, Position p) => PP (Message s p) where
  pp (Msg expecting position action)  
    = ppErr ("",position)
            (   "Expecting  :" >#< (fillblock 120 . intersperse (pp " ") . map pp $ showExp)
                               >#< (if null omitExp then empty else pp "...")
            >-< "Repaired by:" >#< show action
            )
    where (showExp,omitExp) = splitAt 20 . words $ show expecting

instance Position p => Position (Maybe p) where
  line   = maybe (line   noPos) line
  column = maybe (column noPos) column
  file   = maybe (file   noPos) file

ppPos :: Position p => p -> PP_Doc
ppPos p
  = if l < 0 then empty else pp f >|< ppListSep "(" ")" "," [pp l,pp c]
  where l = line p
        c = column p
        f = file p

instance Position (GenToken k t v) where
  line   = line   . position
  column = column . position
  file   = file   . position

-------------------------------------------------------------------------
-- Graph for version/view dpd
-------------------------------------------------------------------------

data DpdGr n
  = DpdGr
      { vgDpd   :: [[n]]
      , vgGr    :: Graph
      , vgGrT   :: Graph
      , vgV2N   :: Vertex -> (n, [n])
      , vgK2V   :: n -> Maybe Vertex
      }

emptyVwDpdGr :: DpdGr Nm
emptyVwDpdGr = mkVwDpdGr [] 

vgVsToNs :: DpdGr n -> [Vertex] -> [n]
vgVsToNs g = map (\v -> fst (vgV2N g v))

mkDpdGr :: Ord n => [[n]] -> (Graph, Vertex -> (n, n, [n]), n -> Maybe Vertex)
mkDpdGr
  = graphFromEdges . mkEdges . foldr cmbChain Map.empty . map mkChain
  where mkChain = Map.fromList . fst . foldl (\(c,prev) n -> ((n,prev) : c,[n])) ([],[])
        cmbChain = Map.unionWith (++)
        mkEdges = map (\(n,ns) -> (n,n,ns)) . Map.toList

mkVwDpdGr :: [[Nm]] -> DpdGr Nm
mkVwDpdGr nLL
  = DpdGr nLL g (transposeG g) (\v -> let (n,_,ns) = n2 v in (n,ns)) v2
  where (g,n2,v2) = mkDpdGr nLL

vgTopSort :: DpdGr n -> [n]
vgTopSort g
  = vgVsToNs g . topSort . vgGr $ g

vgVertices :: Ord n => DpdGr n -> Set.Set n
vgVertices g
  = Set.fromList . vgVsToNs g . vertices . vgGr $ g

vgReachable :: Ord n => (DpdGr n -> Graph) -> DpdGr n -> n -> Set.Set n
vgReachable gOf g n
  = case vgK2V g n of
      Just n' -> Set.fromList . vgVsToNs g $ reachable (gOf g) n'
      Nothing -> Set.empty

vgReachableFrom :: Ord n => DpdGr n -> n -> Set.Set n
vgReachableFrom = vgReachable vgGr 

vgReachableTo :: Ord n => DpdGr n -> n -> Set.Set n
vgReachableTo = vgReachable vgGrT 

vgDpdsOn :: DpdGr n -> n -> [n]
vgDpdsOn g n
  = maybe [] (snd . vgV2N g) (vgK2V g n)

-------------------------------------------------------------------------
-- Kind of scheme
-------------------------------------------------------------------------

data ScKind
  = ScJudge | ScRelation
  deriving (Show,Eq,Ord)

instance PP ScKind where
  pp = text . show

-------------------------------------------------------------------------
-- Kind of Expr wrappers (for influencing latex pretty printing, colors)
-------------------------------------------------------------------------

data WrKind
  = WrIsChanged | WrIsSame | WrTop | WrNone
  deriving (Show,Eq,Ord)

instance PP WrKind where
  pp = text . show

-------------------------------------------------------------------------
-- Is Expr a complex (non variable expr)?
-------------------------------------------------------------------------

data ExprIsRw
  = ExprIsRw    Nm
  | ExprIsVar   Nm
  | ExprIsOther

-------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------

data Nm' s
  = Nm     { nmStr      :: s }
  | NmSel  { nmNm       :: Nm' s
           , nmMbSel    :: Maybe s
           }
  deriving (Eq,Ord)

type Nm = Nm' String

nmBase' :: Nm -> String
nmBase' (NmSel n _) = nmBase' n
nmBase' (Nm s)      = s

nmBase :: Nm -> Nm
nmBase = Nm . nmBase'

nmSetSuff :: Nm -> String -> Nm
nmSetSuff n s = NmSel (nmBase n) (Just s)

nmSetBase :: Nm -> String -> Nm
nmSetBase n s
  = nmFromL (Just s:nL)
  where (_:nL) = nmToMbL n

nmSetSel :: Nm' s -> s -> Nm' s
nmSetSel n s = NmSel n (Just s)

nmSel :: Nm -> String
nmSel = maybe "" id . nmMbSel

nmInit :: Nm -> Nm
nmInit (NmSel n _) = n
nmInit n           = n

nmToMbL :: Nm' s -> [Maybe s]
nmToMbL 
  = reverse . ns
  where ns (NmSel n s) = s : ns n
        ns (Nm s) = [Just s]

nmToL :: Nm -> [String]
nmToL = map (maybe "" id) . nmToMbL

nmFromL :: [Maybe s] -> Nm' s
nmFromL
  = n . reverse
  where n [Just s] = Nm s
        n (s:ss) = NmSel (n ss) s

nmApd :: Nm' s -> Nm' s -> Nm' s
nmApd n1 n2
  = nmFromL (l1 ++ l2)
  where l1 = nmToMbL n1
        l2 = nmToMbL n2

nmStrApd :: Nm -> Nm -> Nm
nmStrApd n1 n2
  = Nm (s1 ++ s2)
  where s1 = show n1
        s2 = show n2

nmShow' :: String -> Nm -> String
nmShow' sep = concat . intersperse sep . nmToL

nmShowAG :: Nm -> String
nmShowAG = nmShow' "_"

instance Show Nm where
  show = nmShow' "."

instance PP Nm where
  pp = ppListSep "" "" "." . nmToL

instance Functor Nm' where
  fmap f (Nm s) = Nm (f s)
  fmap f (NmSel n ms) = NmSel (fmap f n) (fmap f ms)

strVec = "_"
strLhs = "lhs"

nmVec, nmUnk, nmApp, nmWild, nmNone, nmEql, nmComma, nmOParen, nmCParen, nmLhs, nmAny :: Nm
nmVec     = Nm strVec
nmLhs     = Nm strLhs
nmWild    = nmVec
nmUnk     = Nm "??"
nmAny     = Nm "*"
nmEql     = Nm "="
nmApp     = Nm "$"
nmNone    = Nm ""
nmComma   = Nm ","
nmOParen  = Nm "("
nmCParen  = Nm ")"

nmUniq :: Int -> Nm
nmUniq u  = Nm ("uniq" ++ (if u > 0 then show u else ""))

nmCmdBegChng, nmCmdEndChng, nmCmdBegSame, nmCmdEndSame :: Nm
nmCmdBegChng = Nm "rulerChngBegMark"
nmCmdEndChng = Nm "rulerChngEndMark"
nmCmdBegSame = Nm "rulerSameBegMark"
nmCmdEndSame = Nm "rulerSameEndMark"

nmFunMkUniq :: Int -> Nm
nmFunMkUniq u = Nm ("rulerMk" ++ show u ++ "Uniq")

-------------------------------------------------------------------------
-- LaTeX
-------------------------------------------------------------------------

mkLaTeXNm :: String -> String
mkLaTeXNm = map (\c -> if isAlphaNum c then c else '-')

nmLaTeX :: Nm -> Nm
nmLaTeX = Nm . mkLaTeXNm . show

strLhs2TeXSafe :: String -> String
strLhs2TeXSafe = concat . map (\c -> if c == '|' then "||" else [c])

nmLhs2TeXSafe :: Nm -> Nm
nmLhs2TeXSafe = fmap strLhs2TeXSafe

mkMBox :: PP a => a -> PP_Doc
mkMBox p = "\\;\\mbox" >|< ppCurly p

mkRuleNm :: String -> String -> PP_Doc
mkRuleNm r v = "\\textsc" >|< ppCurly (mkLaTeXNm r) >|< (if null v then empty else "_" >|< ppCurly v)

mkVerb :: PP_Doc -> PP_Doc
mkVerb p = ppPacked "@" "@" p

switchLaTeXLhs :: PP a => a -> PP_Doc
switchLaTeXLhs p = ppVBar (" " >|< p >|< " ")

mkInLhs2Tex :: PP_Doc -> PP_Doc
mkInLhs2Tex p = ppVBar (p >|< " ")

ensureTeXMath :: PP_Doc -> PP_Doc
ensureTeXMath = mkTexCmdUse "ensuremath"

mkCmdNmDef :: (PP a, PP b) => a -> b -> PP_Doc
mkCmdNmDef = mkTexCmdDef "rulerCmdDef"

mkCmdNmUse :: PP a => a -> PP_Doc
mkCmdNmUse = mkTexCmdUse "rulerCmdUse"

ppNmLaTeX :: Nm -> PP_Doc
ppNmLaTeX n
  = ppSelLaTeX (== strVec) (fromJust base) (map (fmap (\s -> (s,pp s))) sels)
  where (base:sels) = nmToMbL n

ppSelLaTeX :: (PP base,PP sel) => (sel -> Bool) -> base -> [Maybe (sel,PP_Doc)] -> PP_Doc
ppSelLaTeX isVec base sels
  = p
  where sw = (" " >|<) . switchLaTeXLhs
        over n s = if isVec n then sw (text "\\overline{") else sw ("\\stackrel{" >|< sw s >|< "}{")
        p = case (pp base,sels) of
                 (x,[Nothing,Nothing,Just (n3,s3)])
                   -> over n3 s3
                      >|< x
                      >|< sw (text "}")
                 (x,[Nothing,Just (_,s2),Just (n3,s3)])
                   -> over n3 s3
                      >|< x
                      >|< sw ("^{" >|< sw s2 >|< "}}")
                 (x,[Just (_,s1),Nothing,Just (n3,s3)])
                   -> over n3 s3
                      >|< x
                      >|< sw ("_{" >|< sw s1 >|< "}}")
                 (x,[Just (_,s1),Just (_,s2),Just (n3,s3)])
                   -> over n3 s3
                      >|< x
                      >|< sw ("_{" >|< sw s1 >|< "}^{" >|< sw s2 >|< "}}")
                 (x,(Just (_,s1):Just (_,s2):_))
                   -> x >|< sw ("_{" >|< sw s1 >|< "}^{" >|< sw s2 >|< "}")
                 (x,(Nothing:Just (_,s2):_))
                   -> x >|< sw ("^{" >|< sw s2 >|< "}")
                 (x,(Just (_,s1):_))
                   -> x >|< sw ("_{" >|< sw s1 >|< "}")
                 (x,_)
                   -> x

-------------------------------------------------------------------------
-- Tracing
-------------------------------------------------------------------------

tr m s v = trace (m ++ show s) v
trp m s v = trace (m ++ "\n" ++ disp (m >|< ":" >#< s) 1000 "") v

-------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------

hdAndTl :: [a] -> (a,[a])
hdAndTl (x:xs) = (x,xs)

maybeHd :: r -> (a -> r) -> [a] -> r
maybeHd n f l = if null l then n else f (head l)

strWhite :: Int -> String
strWhite sz = replicate sz ' '

strPad :: String -> Int -> String
strPad s sz = s ++ strWhite (sz - length s)

panic m = error ("panic: " ++ m)


