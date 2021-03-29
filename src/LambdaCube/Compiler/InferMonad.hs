{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}  -- TODO: remove
{-# OPTIONS_GHC -fno-warn-unused-binds #-}  -- TODO: remove
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module LambdaCube.Compiler.InferMonad where

import qualified Data.List                           as List
import qualified Data.Map                            as Map
import qualified Data.Maybe                          as Maybe
import qualified Data.Set                            as Set

import           Control.Arrow                       (Arrow (first, second))
import           Control.Monad.Except                (ExceptT,
                                                      MonadError (throwError),
                                                      MonadPlus (mplus))
import           Control.Monad.Reader                (MonadReader (local),
                                                      ReaderT, asks)
import           Control.Monad.Writer                (MonadWriter (tell),
                                                      WriterT)

--import LambdaCube.Compiler.Utils
import           LambdaCube.Compiler.Core
import           LambdaCube.Compiler.DeBruijn        (HasFreeVars (getFreeVars),
                                                      freeVars, up)
import           LambdaCube.Compiler.DesugaredSource hiding (getList)
import           LambdaCube.Compiler.Parser          (ParseWarning (..))
import           LambdaCube.Compiler.Pretty          hiding (braces, parens)

-------------------------------------------------------------------------------- error messages

data ErrorMsg
    = ErrorMsg Doc
    | ECantFind SName SI
    | ETypeError Doc SI
    | ERedefined SName SI SI

errorRange_ :: ErrorMsg -> [SI]
errorRange_ = \case
    ErrorMsg s          -> []
    ECantFind s si      -> [si]
    ETypeError msg si   -> [si]
    ERedefined s si si' -> [si, si']

instance PShow ErrorMsg where
    pShow = \case
        ErrorMsg s -> s
        ECantFind s si -> "can't find:" <+> text s <+> "in" <+> pShow si
        ETypeError msg si -> "type error:" <+> msg <$$> "in" <+> pShow si
        ERedefined s si si' -> "already defined" <+> text s <+> "at" <+> pShow si <$$> "and at" <+> pShow si'


-------------------------------------------------------------------------------- infos

data Info
    = Info Range Doc
    | IType SIName Exp
    | ITrace String String
    | IError ErrorMsg
    | ParseWarning ParseWarning

instance PShow Info where
    pShow = \case
        Info r s       -> nest 4 $ shortForm (pShow r) <$$> s
        IType a b      -> shAnn (pShow a) (pShow b)
        ITrace i s     -> text i <> ": " <+> text s
        IError e       -> "!" <> pShow e
        ParseWarning w -> pShow w

errorRange :: Infos -> [Range]
errorRange is = [r | IError e <- is, RangeSI r <- errorRange_ e ]

type Infos = [Info]

throwError' :: (MonadWriter Infos m, MonadError ErrorMsg m) => ErrorMsg -> m a
throwError' e = tell [IError e] >> throwError e

mkInfoItem :: SI -> Doc -> Infos
mkInfoItem (RangeSI r) i = [Info r i]
mkInfoItem _ _           = mempty

listAllInfos :: Maybe FileInfo -> Infos -> [Doc]
listAllInfos f m
    = h "trace"  (listTraceInfos m) ++ listAllInfos' f m
  where
    h x [] = []
    h x xs = ("------------" <+> x) : xs

listAllInfos' :: Maybe FileInfo -> Infos -> [Doc]
listAllInfos' f m
    =   h "tooltips" [ nest 4 $ shortForm $ showRangeWithoutFileName r <$$> hsep (List.intersperse "|" is)
                     | (r, is) <- listTypeInfos m, Just (rangeFile r) == f ]
    ++  h "warnings" [ pShow w | ParseWarning w <- m ]
  where
    h x [] = []
    h x xs = ("------------" <+> x) : xs

listTraceInfos :: Infos -> [Doc]
listTraceInfos m = [DResetFreshNames $ pShow i | i <- m, case i of Info{} -> False; ParseWarning{} -> False; _ -> True]

listTypeInfos :: Infos -> [(Range, [Doc])]
listTypeInfos m = Map.toList $ Map.unionsWith (<>) [Map.singleton r [DResetFreshNames i] | Info r i <- m]

listErrors :: Infos -> [(Range, [Doc])]
listErrors m = Map.toList $ Map.unionsWith (<>) [Map.singleton r [DResetFreshNames (pShow e)] | IError e <- m, RangeSI r <- errorRange_ e]

listWarnings :: Infos -> [(Range, [Doc])]
listWarnings m = Map.toList $ Map.unionsWith (<>) [Map.singleton r [DResetFreshNames msg] | ParseWarning (getRangeAndMsg -> Just (r, msg)) <- m]
  where
    getRangeAndMsg = \case
        Unreachable r -> Just (r, "Unreachable")
        w@(Uncovered (getRange . sourceInfo -> Just r) _) -> Just (r, pShow w)
        _ -> Nothing

sortInfos :: [(Range, a)] -> [(Range, a)]
sortInfos = List.sortBy smallestRange
  where
    smallestRange :: (Range, a) -> (Range, a) -> Ordering
    smallestRange (Range{rangeStart=SPos lsa csa, rangeStop=SPos lea cea}, _)
                  (Range{rangeStart=SPos lsb csb, rangeStop=SPos leb ceb}, _)
        | lea - lsa < leb - lsb = LT
        | lea - lsa > leb - lsb = GT
        | cea - csa < ceb - csb = LT
        | cea - csa > ceb - csb = GT
        | lsa < lsb = LT
        | lsa > lsb = GT
        | csa < csb = LT
        | csa > csb = GT
        | otherwise = EQ

tellType :: (MonadWriter Infos m, SourceInfo si) => si -> Type -> m ()
tellType si t = tell $ mkInfoItem (sourceInfo si) $ DTypeNamespace True $ pShow t

filterInRange :: Int -> Int -> [(Range, a)] -> [(Range, a)]
filterInRange l c = filter inRange
  where
    inRange :: (Range, a) -> Bool
    inRange (Range{rangeStart=SPos ls cs, rangeStop=SPos le ce}, _) =
        ls <= l && cs <= c && le >= l && ce >= c

getMostRelevant :: Int -> Int -> [(Range, [Doc])] -> Maybe (Range, Doc)
getMostRelevant l c = Maybe.listToMaybe . filterInRange l c . firstNonEmpty

firstNonEmpty :: [(Range, [a])] -> [(Range, a)]
firstNonEmpty = Maybe.mapMaybe maybeHead

maybeHead :: (a, [b]) -> Maybe (a, b)
maybeHead (a, b:_) = Just (a, b)
maybeHead _        = Nothing

-------------------------------------------------------------------------------- global env

type GlobalEnv = Map.Map SName (Exp, Type, SI)

initEnv :: GlobalEnv
initEnv = Map.fromList
    [ (,) "'Type" (TType, TType, debugSI "source-of-Type")
    ]

-- inference monad
type IM m = ExceptT ErrorMsg (ReaderT (Extensions, GlobalEnv) (WriterT Infos m))

expAndType :: p -> (Exp, Type, c) -> ExpType
expAndType s (e, t, si) = ET e t

-- todo: do only if NoTypeNamespace extension is not on
lookupName :: SName -> Map.Map SName (Exp, Type, c) -> Maybe ExpType
lookupName s@(Ticked s') m = expAndType s <$> (Map.lookup s m `mplus` Map.lookup s' m)
lookupName s m             = expAndType s <$> Map.lookup s m

getDef te si s = do
    nv <- asks snd
    maybe (throwError' $ ECantFind s si) return (lookupName s nv)

addToEnv :: Monad m => SIName -> ExpType -> IM m GlobalEnv
addToEnv sn@(SIName si s) (ET x t) = do
--    maybe (pure ()) throwError_ $ ambiguityCheck s t      -- TODO
--    b <- asks $ (TraceTypeCheck `elem`) . fst
    tell [IType sn t]
    v <- asks $ Map.lookup s . snd
    case v of
      Nothing          -> return $ Map.singleton s (x, t, si)
      Just (_, _, si') -> throwError' $ ERedefined s si si'


removeHiddenUnit :: Exp -> Exp
removeHiddenUnit (Pi Hidden (hnf -> Unit) (down 0 -> Just t)) = removeHiddenUnit t
removeHiddenUnit (Pi h a b) = Pi h a $ removeHiddenUnit b
removeHiddenUnit t = t

addParams :: Foldable t => t (Visibility, Exp) -> Exp -> Exp
addParams ps t = foldr (uncurry Pi) t ps

addLams :: Foldable t => t b -> Exp -> Exp
addLams ps t = foldr (const Lam) t ps

lamify :: Exp -> ([Exp] -> Exp) -> Exp
lamify t x = addLams (fst $ getParams t) $ x $ downTo 0 $ arity t

lamify' :: Exp -> ([Exp] -> Exp) -> Exp
lamify' t x = addLams (fst $ getParams t) $ x $ downTo' 0 $ arity t

arity :: Exp -> Int
arity = length . fst . getParams

downTo, downTo' :: Int -> Int -> [Exp]
downTo n m = map Var [n+m-1, n+m-2..n]
downTo' n m = map Var [n, n+1..n+m-1]

withEnv :: (MonadReader (d, b) m, Semigroup b) => b -> m a -> m a
withEnv e = local $ second (<> e)

lamPi :: Visibility -> Exp -> ExpType -> ExpType
lamPi h t (ET x y) = ET (Lam x) (Pi h t y)

-- Ambiguous: (Int ~ F a) => Int
-- Not ambiguous: (Show a, a ~ F b) => b
ambiguityCheck :: String -> Exp -> Maybe String
ambiguityCheck s ty = case ambigVars ty of
    [] -> Nothing
    err -> Just $ s ++ " has ambiguous type:\n" ++ ppShow ty ++ "\nproblematic vars:\n" ++ ppShow err

ambigVars :: Exp -> [(Int, Exp)]
ambigVars ty = [(n, c) | (n, c) <- hid, not $ any (`Set.member` defined) $ Set.insert n $ free c]
  where
    (defined, hid, _i) = compDefined False ty

floatLetMeta :: Exp -> Bool
floatLetMeta ty = (i-1) `Set.member` defined
  where
    (defined, hid, i) = compDefined True ty

compDefined :: Bool -> Exp -> (Set.Set Int, [(Int, Exp)], Int)
compDefined b ty = (defined, hid, i)
  where
    defined = dependentVars hid $ Set.map (if b then (+i) else id) $ free ty

    i = length hid_
    hid = zipWith (\k t -> (k, up (k+1) t)) (reverse [0..i-1]) hid_
    (hid_, ty') = hiddenVars ty

-- TODO: remove
free :: HasFreeVars a => a -> Set.Set Int
free = Set.fromList . freeVars . getFreeVars

hiddenVars :: Exp -> ([Exp], Exp)
hiddenVars (Pi Hidden a b) = first (a:) $ hiddenVars b
hiddenVars t               = ([], t)

-- compute dependent type vars in constraints
-- Example:  dependentVars [(a, b) ~ F b c, d ~ F e] [c] == [a,b,c]
dependentVars :: [(Int, Exp)] -> Set.Set Int -> Set.Set Int
dependentVars ie = cycle mempty
  where
    freeVars = free

    cycle acc s
        | Set.null s = acc
        | otherwise = cycle (acc <> s) (grow s Set.\\ acc)

    grow = flip foldMap ie $ \case
      (n, t) -> (Set.singleton n <-> freeVars t) <> case t of
        (hnf -> CW (hnf -> CstrT _{-todo-} ty f)) -> freeVars ty <-> freeVars f
        (hnf -> CSplit a b c) -> freeVars a <-> (freeVars b <> freeVars c)
        _ -> mempty
      where
        a --> b = \s -> if Set.null $ a `Set.intersection` s then mempty else b
        a <-> b = (a --> b) <> (b --> a)


closeGlobalEnv :: GlobalEnv -> GlobalEnv
closeGlobalEnv = fmap (\(exp, type_, si) -> (closeExp exp , closeExp type_, si)) -- HINT: type should be finite
