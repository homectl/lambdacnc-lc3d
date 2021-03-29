-- Pattern match compilation
--
-- overview:
-- https://rawgit.com/BP-HUG/presentations/master/2016_april/pattern-match-compilation/patternMatchComp.html

{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE ViewPatterns              #-}
module LambdaCube.Compiler.Patterns
  ( compileGuardTree
  , compileGuardTrees
  , compileGuardTrees'
  , compilePatts
  , compileCase
  , GuardTrees
  , ParPat
  , pattern PVarSimp
  , noGuards
  , ParseCheck(..)
  , getPVars
  -- statements
  , cHCons
  , cHNil
  -- parser
  , ConsInfo
  , PatList
  , pattern PConSimp
  , cSucc
  , cZero
  , cList
  , cCons
  , cNil
  , cHList
  , cTrue
  , lLet
  , guardNode'
  , pattern PParens
  , pattern PatTypeSimp
  , pattern ParPat
  , pattern ViewPatSimp
  , pattern PWildcard
  , patLam
  ) where

import           Control.Arrow                       (Arrow (first, second, (***)),
                                                      ArrowChoice (left))
import           Control.Monad.Writer                (MonadWriter (tell),
                                                      Writer, runWriter)
import           Data.Maybe                          (maybeToList)
import qualified Data.Set                            as Set

import           LambdaCube.Compiler.DeBruijn        (DeBruijnify (..),
                                                      Rearrange (..), rSubst,
                                                      rUp, up)
import           LambdaCube.Compiler.DesugaredSource
import           LambdaCube.Compiler.Pretty          hiding (braces, parens)
import           LambdaCube.Compiler.Utils           (Void, iterateN)

---------------------------------

data ParseCheck
    = TrackedCode Range
    | Reachable Range
    | Uncovered' SIName [PatList]

type PatList = ([ParPat_ ()], [(ParPat_ (), SExp)])

type ConsInfo = Either ((SName{-case eliminator name-}, Int{-num of indices-}), [(SIName, Int)]{-constructors with arities-})
                       Int{-arity-}

-------------------------------------------------------------------------------- pattern representation

type Pat = Pat_ ConsInfo

data Pat_ c
    = PVar SIName
    | PCon_ SI (SIName, c) [ParPat_ c]
    | ViewPat_ SI SExp (ParPat_ c)
    | PatType_ SI (ParPat_ c) SExp

type ParPat = ParPat_ ConsInfo

-- parallel patterns like  v@(f -> [])@(Just x)
data ParPat_ c = ParPat_ SI [Pat_ c]

pattern ParPat :: [Pat_ a] -> ParPat_ a
pattern ParPat ps <- ParPat_ _ ps
  where ParPat ps =  ParPat_ (foldMap sourceInfo ps) ps

instance PShow (Pat_ a) where
    pShow = \case
        PVar sn         -> pShow sn
        PCon (sn, _) ps -> foldl DApp (pShow sn) (pShow <$> ps)
        ViewPat e p     -> DOp "->" (Infix (-1)) (pShow e) (pShow p)
        PatType p t     -> DAnn (pShow p) (pShow t)

instance PShow (ParPat_ a) where
    pShow = \case
        ParPat [] -> text "_"
        ParPat ps -> foldr1 (DOp "@" (InfixR 11)) $ pShow <$> ps

pattern PWildcard :: SI -> ParPat_ a
pattern PWildcard si = ParPat_ si []

pattern PCon :: (SIName, c) -> [ParPat_ c] -> Pat_ c
pattern PCon n pp <- PCon_ _ n pp
  where PCon n pp =  PCon_ (sourceInfo (fst n) <> sourceInfo pp) n pp

pattern ViewPat :: SExp -> ParPat_ c -> Pat_ c
pattern ViewPat e pp <- ViewPat_ _ e pp
  where ViewPat e pp =  ViewPat_ (sourceInfo e <> sourceInfo pp) e pp

pattern PatType :: ParPat_ c -> SExp -> Pat_ c
pattern PatType pp e <- PatType_ _ pp e
  where PatType pp e =  PatType_ (sourceInfo e <> sourceInfo pp) pp e
--pattern SimpPats ps <- (traverse simpleParPat -> Just ps)
--  where SimpPats ps =  ParPat . (:[]) <$> ps

--simpleParPat (ParPat [p]) = Just p
--simpleParPat _ = Nothing

pattern PVarSimp :: SIName -> ParPat
pattern PVarSimp    n    = ParPat [PVar    n]

pattern PConSimp :: (SIName, c) -> [ParPat_ c] -> ParPat_ c
pattern PConSimp    n ps = ParPat [PCon    n ps]
--pattern PConSimp    n ps = PCon    n (SimpPats ps)

pattern ViewPatSimp :: SExp -> ParPat -> ParPat
pattern ViewPatSimp e p  = ParPat [ViewPat e p]

pattern PatTypeSimp :: ParPat -> SExp -> ParPat
pattern PatTypeSimp p t  = ParPat [PatType p t]

pBuiltin :: FNameTag -> Either ((SName,Int),[(FNameTag,Int)]) Int -> [ParPat] -> ParPat
pBuiltin n ci ps = PConSimp (Tag n, left (second $ map $ first Tag) ci) ps

cTrue, cZero, cNil, cHNil :: ParPat
cTrue = pBuiltin FTrue (Left ((CaseName "'Bool", 0), [(FFalse, 0), (FTrue, 0)])) []
cZero = pBuiltin FZero (Left ((CaseName "'Nat", 0), [(FZero, 0), (FSucc, 1)])) []
cNil  = pBuiltin FNil  (Left ((CaseName "'List", 0), [(FNil, 0), (FCons, 2)])) []
cHNil = pBuiltin FHNil (Left (("hlistNilCase", -1), [(FHNil, 0)])) []

cList, cHList, cSucc :: ParPat -> ParPat
cList  a = pBuiltin F'List (Right 1) [a]
cHList a = pBuiltin F'HList (Right 1) [a]
cSucc  a = pBuiltin FSucc (Left ((CaseName "'Nat", 0), [(FZero, 0), (FSucc, 1)])) [a]

cCons, cHCons :: ParPat -> ParPat -> ParPat
cCons  a b = pBuiltin FCons (Left ((CaseName "'List", 0), [(FNil, 0), (FCons, 2)])) [a, b]
cHCons a b = pBuiltin FHCons (Left (("hlistConsCase", -1), [(FHCons, 2)])) [a, b]

pattern PParens :: ParPat -> ParPat
pattern PParens p = ViewPatSimp (SBuiltin Fparens) p

mapP :: (Int -> SExp -> SExp) -> Int -> Pat -> Pat
mapP f i = \case
    PVar n          -> PVar n
    PCon_ si n ps   -> PCon_ si n (upPats (mapPP f) i ps)
    ViewPat_ si e p -> ViewPat_ si (f i e) (mapPP f i p)
    PatType_ si p t -> PatType_ si (mapPP f i p) (f i t)

mapPP :: (Int -> SExp -> SExp) -> Int -> ParPat_ ConsInfo -> ParPat_ ConsInfo
mapPP f i = \case
    ParPat_ si ps -> ParPat_ si $ upPats (mapP f) i ps

--upPats :: _
upPats g k []      = []
upPats g k (p: ps) = g k p: upPats g (k + patVars p) ps

instance Rearrange Pat where
    rearrange k f = mapP (`rearrange` f) k

instance Rearrange ParPat where
    rearrange k f = mapPP (`rearrange` f) k

instance DeBruijnify SIName ParPat where
    deBruijnify_ l ns = mapPP (`deBruijnify_` ns) l

-- pattern variables
class PatVars a where getPVars :: a -> [SIName]

instance PatVars Pat
  where
    getPVars = \case
        PVar n      -> [n]
        PCon _ ps   -> foldMap getPVars ps
        ViewPat e p -> getPVars p
        PatType p t -> getPVars p

instance PatVars ParPat where getPVars (ParPat ps) = foldMap getPVars ps

instance PatVars a => PatVars [a] where getPVars = foldMap getPVars

-- number of pattern variables
patVars :: PatVars a => a -> Int
patVars = length . getPVars

instance SourceInfo (ParPat_ c) where
    sourceInfo (ParPat_ si _) = si

instance SetSourceInfo (ParPat_ c) where
    setSI si (ParPat_ _ ps) = ParPat_ si ps

instance SourceInfo (Pat_ c) where
    sourceInfo = \case
        PVar sn         -> sourceInfo sn
        PCon_ si _ _    -> si
        ViewPat_ si _ _ -> si
        PatType_ si _ _ -> si

instance SetSourceInfo (Pat_ c) where
    setSI si = \case
        PVar sn         -> PVar $ setSI si sn
        PCon_ _  a b    -> PCon_ si a b
        ViewPat_ _  a b -> ViewPat_ si a b
        PatType_ _  a b -> PatType_ si a b

-------------------------------------------------------------------------------- pattern match compilation

-- pattern match compilation monad
type PMC = Writer ([CasePath], [Range])

type CasePath = [Maybe (SIName, Int, SExp)]

runPMC :: MonadWriter [ParseCheck] m => Maybe SIName -> [(Visibility, SExp)] -> PMC a -> m a
runPMC si vt m = do
    tell $ Reachable <$> rs
    case si of
        Nothing -> return ()
        Just si -> tell [Uncovered' si [mkPatt_ (zip [0 :: Int ..] $ reverse p) $ reverse [0.. length vt - 1] | Just p <- sequence <$> ps]]
    return a
  where
    (a, (ps, rs)) = runWriter m

    --mkPatt_ :: _
    mkPatt_ ps_ is = (ps, mkGuards 0 ps_)
      where
        (mconcat -> qs, ps) = unzip $ map (mkPatt 0 ps_) is

        --mkGuards :: _
        mkGuards k [] = []
        mkGuards k ((q, (cn, n, e)): ps) = [(PConSimp (cn, ()) $ replicate n $ PWildcard mempty, e) | q `Set.notMember` qs] ++ mkGuards (k + n) ps

        --mkPatt :: _
        mkPatt k ((q, (cn, n, SVar _ j)): ps) i | j == (i + k)
            = (Set.singleton q <>) . mconcat *** PConSimp (cn, ()) $ unzip [mkPatt 0 ps l | l <- [n-1, n-2..0]]
        mkPatt k ((q, (cn, n, _)): ps) i = mkPatt (k + n) ps i
        mkPatt k [] i = (mempty, PWildcard mempty)
--        mkPatt k ps' i = error $ "mkPatt " ++ show i_ ++ ":  " ++ maybe "" showSI si ++ "\n" ++ show ps' ++ "\n" ++ show ps_

data Lets a
    = LLet SIName SExp (Lets a)
    | LTypeAnn SExp (Lets a)        -- TODO: eliminate if not used
    | In a

lLet :: Rearrange a => SIName -> SExp' Void -> Lets a -> Lets a
lLet sn (SVar sn' i) l = rSubst 0 i l
lLet sn e l            = LLet sn e l

foldLets :: (a -> b) -> Lets a -> b
foldLets f = \case
    In e         -> f e
    LLet sn e x  -> foldLets f x
    LTypeAnn e x -> foldLets f x

--mapLets :: _
mapLets f h l = \case
    In e         -> In $ h l e
    LLet sn e x  -> LLet sn (f l e) $ mapLets f h (l+1) x
    LTypeAnn e x -> LTypeAnn (f l e) $ mapLets f h l x

instance Rearrange a => Rearrange (Lets a) where
    rearrange l f = mapLets (`rearrange` f) (`rearrange` f) l

instance DeBruijnify SIName a => DeBruijnify SIName (Lets a) where
    deBruijnify_ l ns = mapLets (`deBruijnify_` ns) (`deBruijnify_` ns) l

data GuardTree
    = GuardNode SExp (SIName, ConsInfo) [SIName] GuardTrees GuardTrees
    | GTSuccess SExp
    | GTFailure

instance DeBruijnify SIName GuardTree where
    deBruijnify_ l ns = mapGT (`deBruijnify_` ns) (`deBruijnify_` ns) l

type GuardTrees = Lets GuardTree

instance Monoid GuardTrees where
    mempty = In GTFailure
#if !MIN_VERSION_base(4,11,0)
    LLet sn e x `mappend` y = LLet sn e $ x `mappend` rUp 1 0 y
    LTypeAnn t x `mappend` y = LTypeAnn t $ x `mappend` y
    In (GuardNode e n ps t ts) `mappend` y = In $ GuardNode e n ps t (ts `mappend` y)
    In GTFailure `mappend` y = y
    x@(In GTSuccess{}) `mappend` _ = x
#else
instance Semigroup GuardTrees where
    LLet sn e x <> y                = LLet sn e $ x <> rUp 1 0 y
    LTypeAnn t x <> y               = LTypeAnn t $ x <> y
    In (GuardNode e n ps t ts) <> y = In $ GuardNode e n ps t (ts <> y)
    In GTFailure <> y               = y
    x@(In GTSuccess{}) <> _         = x
#endif

noGuards :: SExp -> GuardTrees
noGuards = In . GTSuccess

mapGT :: (Int -> ParPat -> ParPat) -> (Int -> SExp -> SExp) -> Int -> GuardTree -> GuardTree
mapGT f h k = \case
    GuardNode e c pps gt el -> GuardNode (h k e) c pps (mapGTs f h (k + length pps) gt) (mapGTs f h k el)
    GTSuccess e -> GTSuccess $ h k e
    GTFailure -> GTFailure

mapGTs :: (Int -> ParPat -> ParPat) -> (Int -> SExp -> SExp) -> Int -> GuardTrees -> GuardTrees
mapGTs f h = mapLets h (mapGT f h)
{-
foldGT f = \case
    GuardNode e c pps gt el -> GuardNode (h k e) c pps (mapGTs f h (k + length pps) gt) (mapGTs f h k el)
    GTSuccess e -> f e
    GTFailure -> mempty
-}
instance Rearrange GuardTree where
    rearrange l f = mapGT (`rearrange` f) (`rearrange` f) l

pattern Otherwise :: SExp' Void
pattern Otherwise = SBuiltin Fotherwise

guardNode :: Pat -> SExp -> GuardTrees -> GuardTrees
guardNode (PCon (sName -> "True", _) []) Otherwise gt = gt
guardNode (PCon (sName -> "False", _) []) Otherwise gt = In GTFailure
guardNode (PVar sn) e gt = lLet sn e gt
guardNode (ViewPat f p) e gt = guardNode' p (f `SAppV` e) gt
guardNode (PatType p t) e gt = guardNode' p (SAnn e t) gt
guardNode (PCon sn ps) e gt = In $ GuardNode e sn (replicate n $ dummyName "gn") (buildNode guardNode' n ps [n-1, n-2..0] gt) mempty
  where
    n = length ps

guardNode' :: ParPat_ ConsInfo -> SExp -> GuardTrees -> GuardTrees
guardNode' (PParens p) e gt = guardNode' p e gt
guardNode' (ParPat_ si ps) e gt = case ps of
    []  -> gt
    [p] -> guardNode p e gt
    ps  -> lLet (SIName si "gtc") e $ buildNode guardNode 1 ps [0..] gt

--buildNode :: _
buildNode guardNode n ps is gt
    = foldr f (rUp n (patVars ps) gt) $ zip3 ps is $ scanl (+) 0 $ map patVars ps
  where
    f (p, i, d) = guardNode (rUp n d p) (sVar "gn" $ i + d)

compilePatts :: [ParPat] -> GuardTrees -> GuardTrees
compilePatts ps = buildNode guardNode' n ps [n-1, n-2..0]
  where
    n = length ps

compileGuardTree :: MonadWriter [ParseCheck] m => (SExp -> SExp) -> (SExp -> SExp) -> Maybe SIName -> [(Visibility, SExp)] -> GuardTrees -> m SExp
compileGuardTree ulend lend si vt = fmap (\e -> foldr (uncurry SLam) e vt) . runPMC si vt . guardTreeToCases []
  where
    guardTreeToCases :: CasePath -> GuardTrees -> PMC SExp
    guardTreeToCases path = \case
        LLet sn e g -> SLet sn e <$> guardTreeToCases (Nothing: path){-TODO-} g
        LTypeAnn t g -> SAnn <$> guardTreeToCases (Nothing: path){-TODO-} g <*> pure t
        In GTFailure -> do
            tell ([path], mempty)
            return $ ulend $ SBuiltin Fundefined
        In (GTSuccess e) -> do
            tell $ (,) mempty $ maybeToList $ getRange $ sourceInfo e
            return $ lend e
        ts@(In (GuardNode f (s, cn) _ _ _)) -> case cn of
            Left ((casename, inum), cns) -> do
                cf <- sequence [ iterateN n SLamV <$> guardTreeToCases (Just (cn, n, f): path) (filterGuardTree (up n f) cn 0 n $ rUp n 0 ts)
                               | (cn, n) <- cns ]
                return $
                    foldl SAppV
                        (SGlobal (SIName mempty casename) `SAppV` iterateN (1 + inum) SLamV (Wildcard SType))
                        cf
                    `SAppV` f
            Right n -> do
                g1 <- guardTreeToCases (Nothing: path){-TODO-} $ filterGuardTree (up n f) s 0 n $ rUp n 0 ts
                g2 <- guardTreeToCases (Nothing: path){-TODO-} $ filterGuardTree' f s ts
                return $ SGlobal (SIName mempty $ MatchName $ sName s)
                 `SAppV` SLamV (Wildcard SType)
                 `SAppV` iterateN n SLamV g1
                 `SAppV` f
                 `SAppV` g2

    filterGuardTree' :: SExp -> SIName{-constr.-} -> GuardTrees -> GuardTrees
    filterGuardTree' f s = \case
        LLet sn e gt -> LLet sn e $ filterGuardTree' (up 1 f) s gt
        LTypeAnn e gt -> LTypeAnn e $ filterGuardTree' f s gt
        In (GuardNode f' s' ps gs (filterGuardTree' f s -> el))
            | f /= f' || s /= fst s' -> In $ GuardNode f' s' ps (filterGuardTree' (up (length ps) f) s gs) el
            | otherwise -> el
        In x -> In x

    filterGuardTree :: SExp -> SIName{-constr.-} -> Int -> Int{-number of constr. params-} -> GuardTrees -> GuardTrees
    filterGuardTree f s k ns = \case
        LLet sn e gt -> LLet sn e $ filterGuardTree (up 1 f) s (k + 1) ns gt
        LTypeAnn e gt -> LTypeAnn e $ filterGuardTree f s k ns gt
        In (GuardNode f' s' ps gs (filterGuardTree f s k ns -> el))
            | f /= f'   -> In $ GuardNode f' s' ps (filterGuardTree (up su f) s (su + k) ns gs) el
            | s == fst s' -> filterGuardTree f s k ns $ foldr (rSubst 0) gs (replicate su $ k+ns-1) <> el
            | otherwise -> el
          where
            su = length ps
        In x -> In x

compileGuardTrees :: MonadWriter [ParseCheck] m => (SExp -> SExp) -> Maybe SIName -> [(Visibility, SExp)] -> [GuardTrees] -> m SExp
compileGuardTrees ulend si vt = compileGuardTree ulend SRHS si vt . mconcat

compileGuardTrees' :: (Traversable t, MonadWriter [ParseCheck] m) => a -> [(Visibility, SExp)] -> t GuardTrees -> m (SExp' Void)
compileGuardTrees' si vt = fmap (foldr1 $ SAppV2 $ SBuiltin FparEval `SAppV` Wildcard SType) . mapM (compileGuardTrees id Nothing vt . (:[]))

compileCase :: MonadWriter [ParseCheck] m => SExp' Void -> [(ParPat, GuardTrees)] -> m (SExp' Void)
compileCase x cs
    = (`SAppV` x) <$> compileGuardTree id id (Just $ SIName (sourceInfo x) "") [(Visible, Wildcard SType)] (mconcat [compilePatts [p] e | (p, e) <- cs])

patLam :: MonadWriter [ParseCheck] m => (SExp -> SExp) -> (Visibility, SExp) -> ParPat -> SExp -> m SExp
patLam f vt p e = compileGuardTree f f (Just $ SIName (sourceInfo p) "") [vt] (compilePatts [p] $ noGuards e)

