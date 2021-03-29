{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE ViewPatterns              #-}
--{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}  -- TODO: remove
--{-# OPTIONS_GHC -fno-warn-unused-binds #-}  -- TODO: remove
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module LambdaCube.Compiler.Core where

import           Text.Printf                         (printf)
--import Debug.Trace
import           Data.Binary                         (Binary (get, put), Get,
                                                      Word8)
import           GHC.Generics                        (Generic)

import           Control.Arrow                       (Arrow (first))
import           Data.Function                       (on)
import           Data.List                           (intercalate)

import           LambdaCube.Compiler.DeBruijn
import           LambdaCube.Compiler.DesugaredSource
import           LambdaCube.Compiler.Pretty          hiding (braces, parens)
import           LambdaCube.Compiler.Utils           (foldlrev)

-------------------------------------------------------------------------------- names with infos

data ConName       = ConName       FName Int{-ordinal number, e.g. Zero:0, Succ:1-} Type
  deriving Generic

data TyConName     = TyConName     FName Int{-num of indices-} Type [(ConName, Type)]{-constructors-} CaseFunName
  deriving Generic

data FunName       = FunName       FName Int{-num of global vars-} FunDef Type

data CaseFunName   = CaseFunName   FName Type Int{-num of parameters-}
  deriving Generic

data TyCaseFunName = TyCaseFunName FName Type
  deriving Generic

data FunDef
    = DeltaDef !Int{-arity-} (FreeVars -> [Exp]{-args in reversed order-} -> Exp)
    | NoDef
    | ExpDef Exp

class HasFName a where getFName :: a -> FName

instance HasFName ConName       where getFName (ConName n _ _) = n
instance HasFName TyConName     where getFName (TyConName n _ _ _ _) = n
instance HasFName FunName       where getFName (FunName n _ _ _) = n
instance HasFName CaseFunName   where getFName (CaseFunName n _ _) = n
instance HasFName TyCaseFunName where getFName (TyCaseFunName n _) = n

instance Eq ConName       where (==) = (==) `on` getFName
instance Eq TyConName     where (==) = (==) `on` getFName
instance Eq FunName       where (==) = (==) `on` getFName
instance Eq CaseFunName   where (==) = (==) `on` getFName
instance Eq TyCaseFunName where (==) = (==) `on` getFName

instance Show  ConName       where show  (ConName n _ _) = show n
instance PShow ConName       where pShow (ConName n _ _) = pShow n
instance Show  TyConName     where show  (TyConName n _ _ _ _) = show n
instance PShow TyConName     where pShow (TyConName n _ _ _ _) = pShow n
instance Show  FunName       where show  (FunName n _ _ _) = show n
instance PShow FunName       where pShow (FunName n _ _ _) = pShow n
instance Show  CaseFunName   where show  (CaseFunName n _ _) = CaseName $ show n
instance PShow CaseFunName   where pShow (CaseFunName n _ _) = text $ CaseName $ show n
instance Show  TyCaseFunName where show  (TyCaseFunName n _) = MatchName $ show n
instance PShow TyCaseFunName where pShow (TyCaseFunName n _) = text $ MatchName $ show n

-------------------------------------------------------------------------------- core expression representation

data Freq = CompileTime | RunTime       -- TODO
  deriving (Eq, Generic, Show)

data Exp
    = ELit   Lit
    | TType_ Freq
    | Lam_   FreeVars Exp
    | Con_   FreeVars ConName !Int{-number of ereased arguments applied-} [Exp]{-args reversed-}
    | TyCon_ FreeVars TyConName [Exp]{-args reversed-}
    | Pi_    FreeVars Visibility Exp Exp
    | Neut   Neutral
    | RHS    Exp{-always in hnf-}
    | Let_   FreeVars ExpType Exp
    | Up_    FreeVars [Int] Exp
    | STOP   String
    deriving (Generic, Show)

data Neutral
    = Var_        !Int{-De Bruijn index-}
    | App__       FreeVars Neutral Exp
    | CaseFun__   FreeVars CaseFunName   [Exp] Neutral
    | TyCaseFun__ FreeVars TyCaseFunName [Exp] Neutral
    | Fun_        FreeVars FunName [Exp]{-given parameters, reversed-} Exp{-unfolded expression, in hnf-}
    | UpN_        FreeVars [Int] Neutral
    deriving (Generic, Show)

-------------------------------------------------------------------------------- auxiliary functions and patterns

type Type = Exp

data ExpType = ET {expr :: Exp, ty :: Type}
    deriving (Eq, Generic, Show)
{-
pattern ET a b <- ET_ a b
  where ET a b =  ET_ a (hnf b)
-}
instance Rearrange ExpType where
    rearrange l f (ET e t) = ET (rearrange l f e) (rearrange l f t)

instance HasFreeVars ExpType where
    getFreeVars (ET a b) = getFreeVars a <> getFreeVars b

instance PShow ExpType where pShow = mkDoc (False, False)

type SExp2 = SExp' ExpType

setMaxDB db = \case
    Neut (Fun_ _ a b c) -> Neut $ Fun_ db a b c

pattern TType = TType_ CompileTime

infixl 2 `App`, `app_`
infixr 1 :~>

pattern NoRHS <- (isRHS -> False)

isRHS RHS{} = True
isRHS _     = False

pattern Fun f xs n          <- Fun_ _ f xs n
  where Fun f xs n          =  Fun_ (foldMap getFreeVars xs) f xs n
pattern CaseFun_ a b c      <- CaseFun__ _ a b c
  where CaseFun_ a b c      =  CaseFun__ (getFreeVars c <> foldMap getFreeVars b) a b c
pattern TyCaseFun_ a b c    <- TyCaseFun__ _ a b c
  where TyCaseFun_ a b c    =  TyCaseFun__ (foldMap getFreeVars b <> getFreeVars c) a b c
pattern App_ a b            <- App__ _ a b
  where App_ a b            =  App__ (getFreeVars a <> getFreeVars b) a b
pattern Con x n y           <- Con_ _ x n y
  where Con x n y           =  Con_ (foldMap getFreeVars y) x n y
pattern TyCon x y           <- TyCon_ _ x y
  where TyCon x y           =  TyCon_ (foldMap getFreeVars y) x y
pattern Lam y               <- Lam_ _ y
  where Lam y               =  Lam_ (lowerFreeVars (getFreeVars y)) y
pattern Pi v x y            <- Pi_ _ v x y
  where Pi v x y            =  Pi_ (getFreeVars x <> lowerFreeVars (getFreeVars y)) v x y
pattern Let x y             <- Let_ _ x y
  where Let x y             =  Let_ (getFreeVars x <> lowerFreeVars (getFreeVars y)) x y

pattern SubstLet x <- (substLet -> Just x)

substLet (Let x y) = Just $ subst 0 (expr x) y
substLet _         = Nothing

pattern CaseFun a b c   = Neut (CaseFun_ a b c)
pattern TyCaseFun a b c = Neut (TyCaseFun_ a b c)
pattern Var a           = Neut (Var_ a)
pattern App a b        <- Neut (App_ (Neut -> a) b)
pattern DFun a b        = Neut (DFunN a b)
pattern DFun_ s a b        = Neut (DFunN_ s a b)

-- unreducable function application
pattern UFun a b <- Neut (Fun (FunName (FTag a) _ _ _) b NoRHS)

-- saturated function application
pattern DFunN a xs <- Fun (FunName (FTag a) _ _ _) xs _
  where DFunN a xs =  Fun (mkFunDef' (FTag a)) xs delta

pattern DFunN_ s a xs <- Fun_ s (FunName (FTag a) _ _ _) xs _
  where DFunN_ s a xs =  Fun_ s (mkFunDef' (FTag a)) xs delta

mkFunDef' a@(FTag f) = mkFunDef a $ funTy f

funTy = \case
    F'EqCT      -> TType :~> Var 0 :~> Var 1 :~> TConstraint
    Fcoe        -> TType :~> TType :~> CW (CstrT TType (Var 1) (Var 0)) :~> Var 2 :~> Var 2
    FparEval    -> TType :~> Var 0 :~> Var 1 :~> Var 2
    F'T2        -> TConstraint :~> TConstraint :~> TConstraint
    F'CW        -> TConstraint :~> TType
    Ft2C        -> Unit :~> Unit :~> Unit

conParams (conTypeName -> TyConName _ _ _ _ (CaseFunName _ _ pars)) = pars
mkConPars n (snd . getParams . hnf -> TyCon (TyConName _ _ _ _ (CaseFunName _ _ pars)) xs) = take (min n pars) $ reverse xs
mkConPars n x@Neut{} = error $ "mkConPars!: " ++ ppShow x
mkConPars n x = error $ "mkConPars: " ++ ppShow (n, x)

pattern ConN s a   <- Con (ConName (FTag s) _ _) _ a
tCon s i t a        = Con (ConName (FTag s) i t) 0 a
tCon_ k s i t a     = Con (ConName (FTag s) i t) k a
pattern TyConN s a <- TyCon (TyConName s _ _ _ _) a

pattern TTyCon0 s  <- TyCon (TyConName (FTag s) _ _ _ _) []
tTyCon0 s cs = TyCon (TyConName (FTag s) 0 TType (map ((,) (error "tTyCon0")) cs) $ CaseFunName (FTag s) (STOP "TTyCon0-B") 0) []

pattern a :~> b = Pi Visible a b

delta = ELit (LString "<<delta function>>") -- TODO: build an error call

pattern TConstraint <- TTyCon0 F'Constraint where TConstraint = tTyCon0 F'Constraint []
pattern Unit        <- TTyCon0 F'Unit      where Unit        = tTyCon0 F'Unit [Unit]
pattern TInt        <- TTyCon0 F'Int       where TInt        = tTyCon0 F'Int []
pattern TNat        <- TTyCon0 F'Nat       where TNat        = tTyCon0 F'Nat []
pattern TBool       <- TTyCon0 F'Bool      where TBool       = tTyCon0 F'Bool []
pattern TFloat      <- TTyCon0 F'Float     where TFloat      = tTyCon0 F'Float []
pattern TString     <- TTyCon0 F'String    where TString     = tTyCon0 F'String []
pattern TChar       <- TTyCon0 F'Char      where TChar       = tTyCon0 F'Char []
pattern TOrdering   <- TTyCon0 F'Ordering  where TOrdering   = tTyCon0 F'Ordering []
pattern TVec a b    <- TyConN (FTag F'VecS) [a, b]

pattern Empty s    <- TyCon (TyConName (FTag F'Empty) _ _ _ _) (HString{-hnf?-} s: _)
  where Empty s     = TyCon (TyConName (FTag F'Empty) (error "todo: inum2_") (TString :~> TType) (error "todo: tcn cons 3_") $ error "Empty") [HString s]

pattern TT          <- Con _ _ _
  where TT          =  tCon FTT 0 Unit []

pattern CUnit       <- ConN FCUnit _
  where CUnit       =  tCon FCUnit 0 TConstraint []
pattern CEmpty s    <- ConN FCEmpty (HString s: _)
  where CEmpty s    =  tCon FCEmpty 1 (TString :~> TConstraint) [HString s]

pattern CstrT t a b     = Neut (CstrT' t a b)
pattern CstrT' t a b    = DFunN F'EqCT [b, a, t]
pattern Coe a b w x     = DFun Fcoe [x,w,b,a]
pattern ParEval t a b   = DFun FparEval [b, a, t]
pattern T2 s a b        = DFun_ s F'T2 [b, a]
pattern CW a            = DFun F'CW [a]
pattern CW_ s a         = DFun_ s F'CW [a]
pattern CSplit a b c   <- UFun F'Split [c, b, a]

pattern HLit a <- (hnf -> ELit a)
  where HLit = ELit
pattern HInt a      = HLit (LInt a)
pattern HFloat a    = HLit (LFloat a)
pattern HChar a     = HLit (LChar a)
pattern HString a   = HLit (LString a)

pattern EBool a <- (getEBool -> Just a)
  where EBool = \case
            False -> tCon FFalse 0 TBool []
            True  -> tCon FTrue  1 TBool []

getEBool (hnf -> ConN FFalse _) = Just False
getEBool (hnf -> ConN FTrue _)  = Just True
getEBool _                      = Nothing

pattern ENat n <- (fromNatE -> Just n)
  where ENat 0         = tCon FZero 0 TNat []
        ENat n | n > 0 = tCon FSucc 1 (TNat :~> TNat) [ENat (n-1)]

fromNatE :: Exp -> Maybe Int
fromNatE (hnf -> ConN FZero _)      = Just 0
fromNatE (hnf -> ConN FSucc (n: _)) = succ <$> fromNatE n
fromNatE _                          = Nothing

mkOrdering x = case x of
    LT -> tCon FLT 0 TOrdering []
    EQ -> tCon FEQ 1 TOrdering []
    GT -> tCon FGT 2 TOrdering []

conTypeName :: ConName -> TyConName
conTypeName (ConName _ _ t) = case snd $ getParams t of TyCon n _ -> n ; STOP m -> error $ "STOP " ++ m

mkFun_ md (FunName _ _ (DeltaDef ar f) _) as _ | length as == ar = f md as
mkFun_ md f@(FunName _ _ (ExpDef e) _) xs _ = Neut $ Fun_ md f xs $ hnf $ foldlrev app_ e xs
mkFun_ md f xs y = Neut $ Fun_ md f xs $ hnf y

mkFun :: FunName -> [Exp] -> Exp -> Exp
mkFun f xs e = mkFun_ (foldMap getFreeVars xs) f xs e

pattern ReducedN y <- Fun _ _ (RHS y)
pattern Reduced y <- Neut (ReducedN y)
{-
-- TODO: too much hnf call
reduce (Neut (ReducedN y)) = Just $ hnf y
reduce (SubstLet x) = Just $ hnf x
reduce _ = Nothing
-}
hnf (Reduced y) = hnf y  -- TODO: review hnf call here
hnf a           = a

outputType = tTyCon0 F'Output []

-- TODO: remove
boolType = TBool
-- TODO: remove
trueExp = EBool True

-------------------------------------------------------------------------------- low-level toolbox

class Subst b a where
    subst_ :: Int -> FreeVars -> b -> a -> a

--subst :: Subst b a => Int -> FreeVars -> b -> a -> a
subst i x a = subst_ i (getFreeVars x) x a

instance Subst Exp ExpType where
    subst_ i dx x (ET a b) = ET (subst_ i dx x a) (subst_ i dx x b)

instance Subst ExpType SExp2 where
    subst_ j _ x = mapS (\_ _ -> error "subst: TODO") (const . SGlobal) f2 0
      where
        f2 sn i k = case compare i (k + j) of
            GT -> SVar sn $ i - 1
            LT -> SVar sn i
            EQ -> STyped $ up k x

down :: (PShow a, Subst Exp a, HasFreeVars a{-usedVar-}) => Int -> a -> Maybe a
down t x | usedVar t x = Nothing
         | otherwise = Just $ subst_ t mempty (error $ "impossible: down" ++ ppShow (t,x, getFreeVars x) :: Exp) x

instance Eq Exp where
    Neut a == Neut a'         = a == a'         -- try to compare by structure before reduction
    Reduced a == a'           = a == a'
    a == Reduced a'           = a == a'
    Lam a == Lam a'           = a == a'
    Pi a b c == Pi a' b' c'   = (a, b, c) == (a', b', c')
    Con a n b == Con a' n' b' = (a, n, b) == (a', n', b')
    TyCon a b == TyCon a' b'  = (a, b) == (a', b')
    TType_ f == TType_ f'     = f == f'
    ELit l == ELit l'         = l == l'
    RHS a == RHS a'           = a == a'
    _ == _                    = False

instance Eq Neutral where
    Fun f a _ == Fun f' a' _                = (f, a) == (f', a')       -- try to compare by structure before reduction
    ReducedN a == a'                        = a == Neut a'
    a == ReducedN a'                        = Neut a == a'
    CaseFun_ a b c == CaseFun_ a' b' c'     = (a, b, c) == (a', b', c')
    TyCaseFun_ a b c == TyCaseFun_ a' b' c' = (a, b, c) == (a', b', c')
    App_ a b == App_ a' b'                  = (a, b) == (a', b')
    Var_ a == Var_ a'                       = a == a'
    _ == _                                  = False

instance Subst Exp Exp where
    subst_ i0 dx x = f i0
      where
        f i (Neut n) = substNeut n
          where
            substNeut e | dbGE i e = Neut e
            substNeut e = case e of
                Var_ k              -> case compare k i of GT -> Var $ k - 1; LT -> Var k; EQ -> up (i - i0) x
                CaseFun__ fs s as n -> evalCaseFun (adjustDB i fs) s (f i <$> as) (substNeut n)
                TyCaseFun__ fs s as n -> evalTyCaseFun_ (adjustDB i fs) s (f i <$> as) (substNeut n)
                App__ fs a b        -> app__ (adjustDB i fs) (substNeut a) (f i b)
                Fun_ md fn xs v     -> mkFun_ (adjustDB i md) fn (f i <$> xs) $ f i v
        f i e | dbGE i e = e
        f i e = case e of
            Lam_ md b      -> Lam_ (adjustDB i md) (f (i+1) b)
            Con_ md s n as -> Con_ (adjustDB i md) s n $ f i <$> as
            Pi_ md h a b   -> Pi_ (adjustDB i md) h (f i a) (f (i+1) b)
            TyCon_ md s as -> TyCon_ (adjustDB i md) s $ f i <$> as
            Let_ md a b    -> Let_ (adjustDB i md) (subst_ i dx x a) (f (i+1) b)
            RHS a          -> RHS $ hnf $ f i a
            x              -> x

        adjustDB i md = if usedVar i md then delVar i md <> shiftFreeVars (i-i0) dx else delVar i md

instance Rearrange Exp where
    rearrange i g = f i where
        f i e | dbGE i e = e
        f i e = case e of
            Lam_ md b       -> Lam_ (rearrangeFreeVars g i md) (f (i+1) b)
            Pi_ md h a b    -> Pi_ (rearrangeFreeVars g i md) h (f i a) (f (i+1) b)
            Con_ md s pn as -> Con_ (rearrangeFreeVars g i md) s pn $ map (f i) as
            TyCon_ md s as  -> TyCon_ (rearrangeFreeVars g i md) s $ map (f i) as
            Neut x          -> Neut $ rearrange i g x
            Let x y         -> Let (rearrange i g x) (rearrange (i+1) g y)
            RHS x           -> RHS $ rearrange i g x

instance Rearrange Neutral where
    rearrange i g = f i where
        f i e | dbGE i e = e
        f i e = case e of
            Var_ k -> Var_ $ if k >= i then rearrangeFun g (k-i) + i else k
            CaseFun__ md s as ne -> CaseFun__ (rearrangeFreeVars g i md) s (rearrange i g <$> as) (rearrange i g ne)
            TyCaseFun__ md s as ne -> TyCaseFun__ (rearrangeFreeVars g i md) s (rearrange i g <$> as) (rearrange i g ne)
            App__ md a b -> App__ (rearrangeFreeVars g i md) (rearrange i g a) (rearrange i g b)
            Fun_ md fn x y -> Fun_ (rearrangeFreeVars g i md) fn (rearrange i g <$> x) $ rearrange i g y

instance HasFreeVars Exp where
    getFreeVars = \case
        Lam_ c _     -> c
        Pi_ c _ _ _  -> c
        Con_ c _ _ _ -> c
        TyCon_ c _ _ -> c
        Let_ c _ _   -> c

        STOP{}       -> mempty
        TType_ _     -> mempty
        ELit{}       -> mempty
        Neut x       -> getFreeVars x
        RHS x        -> getFreeVars x

instance HasFreeVars Neutral where
    getFreeVars = \case
        Var_ k              -> freeVar k
        CaseFun__ c _ _ _   -> c
        TyCaseFun__ c _ _ _ -> c
        App__ c a b         -> c
        Fun_ c _ _ _        -> c

varType' :: Int -> [Exp] -> Exp
varType' i vs = vs !! i

-------------------------------------------------------------------------------- pretty print

instance PShow Exp where
    pShow = mkDoc (False, False)

instance PShow Neutral where
    pShow = mkDoc (False, False)

class MkDoc a where
    mkDoc :: (Bool {-print reduced-}, Bool{-print function bodies-}) -> a -> Doc

instance MkDoc ExpType where
    mkDoc pr (ET e TType) = mkDoc pr e
    mkDoc pr (ET e t)     = DAnn (mkDoc pr e) (mkDoc pr t)

instance MkDoc Exp where
    mkDoc pr@(reduce, body) = \case
        Lam b           -> shLam_ (usedVar' 0 b "") (BLam Visible) Nothing (mkDoc pr b)
        Pi h TType b    -> shLam_ (usedVar' 0 b "") (BPi h) Nothing (mkDoc pr b)
        Pi h a b        -> shLam (usedVar' 0 b "") (BPi h) (mkDoc pr a) (mkDoc pr b)
        ENat n          -> pShow n
        Con s@(ConName _ i _) _ _ | body -> text $ "<<" ++ showNth i ++ " constructor of " ++ show (conTypeName s) ++ ">>"
        ConN FHCons (xs: x: _{-2-}) -> foldl DApp (text "HCons") (mkDoc pr <$> [x, xs])
        Con s _ xs      -> foldlrev DApp (pShow s) (mkDoc pr <$> xs)
        TyCon s@(TyConName _ i _ cs _) _ | body
            -> text $ "<<type constructor with " ++ show i ++ " indices; constructors: " ++ intercalate ", " (show . fst <$> cs) ++ ">>"
        TyConN s xs     -> foldlrev DApp (pShow s) (mkDoc pr <$> xs)
        TType           -> text "Type"
        ELit l          -> pShow l
        Neut x          -> mkDoc pr x
        Let a b         -> shLet_ (pShow a) (pShow b)
        RHS x           -> text "_rhs" `DApp` mkDoc pr x
        STOP x          -> text $ "STOP " ++ x

pattern FFix f <- Fun (FunName (FTag FprimFix) _ _ _) [f, _] _

getFixLam (Lam (Neut (Fun s@(FunName _ loc _ _) xs _)))
    | loc > 0
    , (h, v) <- splitAt loc $ reverse xs
    , Neut (Var_ 0) <- last h
    = Just (s, v)
getFixLam _ = Nothing

instance MkDoc Neutral where
    mkDoc pr@(reduce, body) = \case
        CstrT' t a b        -> shCstr (mkDoc pr a) (mkDoc pr (ET b t))
        Fun (FunName _ _ (ExpDef d) _) xs _ | body -> mkDoc (reduce, False) (foldlrev app_ d xs)
        FFix (getFixLam -> Just (s, xs)) | not body -> foldl DApp (pShow s) $ mkDoc pr <$> xs
        FFix f -> foldl DApp "primFix" [{-pShow t -}"_", mkDoc pr f]
        Fun (FunName _ _ (DeltaDef n _) _) _ _ | body -> text $ "<<delta function with arity " ++ show n ++ ">>"
        Fun (FunName _ _ NoDef _) _ _ | body -> "<<builtin>>"
        ReducedN a | reduce -> mkDoc pr a
        Fun s@(FunName _ loc _ _) xs _ -> foldl DApp ({-foldl DHApp (-}pShow s{-) h-}) v
          where (_h, v) = splitAt loc $ mkDoc pr <$> reverse xs
        Var_ k              -> shVar k
        App_ a b            -> mkDoc pr a `DApp` mkDoc pr b
        CaseFun_ s@(CaseFunName _ _ p) _ n | body -> text $ "<<case function of a type with " ++ show p ++ " parameters>>"
        CaseFun_ s xs n     -> foldl DApp (pShow s) (map (mkDoc pr) $ xs ++ [Neut n])
        TyCaseFun_ _ _ _ | body -> text "<<type case function>>"
        TyCaseFun_ s [m, t, f] n  -> foldl DApp (pShow s) (mkDoc pr <$> [m, t, Neut n, f])
        TyCaseFun_ s _ n -> error $ "mkDoc TyCaseFun"
        _ -> "()"

-------------------------------------------------------------------------------- reduction

{- todo: generate
    DFun n@(FunName "natElim" _) [a, z, s, Succ x] -> let      -- todo: replace let with better abstraction
                sx = s `app_` x
            in sx `app_` eval (DFun n [a, z, s, x])
    MT "natElim" [_, z, s, Zero] -> z
    DFun na@(FunName "finElim" _) [m, z, s, n, ConN "FSucc" [i, x]] -> let six = s `app_` i `app_` x-- todo: replace let with better abstraction
        in six `app_` eval (DFun na [m, z, s, i, x])
    MT "finElim" [m, z, s, n, ConN "FZero" [i]] -> z `app_` i
-}

mkFunDef :: FName -> Type -> FunName
mkFunDef a@(FTag FprimFix) t = fn
  where
    fn = FunName a 0 (DeltaDef 2 fx) t
    fx s xs@(f: _) = x where x = Neut $ Fun_ s fn xs $ RHS $ f `app_` x

mkFunDef a@(FTag tag) t = fn
  where
    fn = FunName a 0 (maybe NoDef (DeltaDef (length $ fst $ getParams t)) $ getFunDef tag) t

    getFunDef tag = case tag of
        F'EqCT             -> Just $ \s -> \case (b: a: t: _)   -> cstr t a b
        F'T2               -> Just $ \s -> \case (b: a: _)      -> t2_ s a b
        F'CW               -> Just $ \s -> \case (a: _)         -> cw_ s a
        Ft2C               -> Just $ \s -> \case (b: a: _)      -> t2C a b
        Fcoe               -> Just $ \s -> \case (d: t: b: a: _) -> evalCoe a b t d
        FparEval           -> Just $ \s -> \case (b: a: t: _)   -> parEval t a b
          where
            parEval _ x@RHS{} _ = x
            parEval _ _ x@RHS{} = x
            parEval t a b       = ParEval t a b

        FunsafeCoerce      -> Just $ \s -> \case xs@(x@(hnf -> NonNeut): _{-2-}) -> x; xs -> f s xs
        FreflCstr          -> Just $ \s -> \case _ -> TT
        FhlistNilCase      -> Just $ \s -> \case ((hnf -> Con n@(ConName _ 0 _) _ _): x: _{-1-}) -> x; xs -> f s xs
        FhlistConsCase     -> Just $ \s -> \case ((hnf -> Con n@(ConName _ 1 _) _ (b: a: _{-2-})): x: _{-3-}) -> x `app_` a `app_` b; xs -> f s xs

        -- general compiler primitives
        FprimAddInt        -> Just $ \s -> \case (HInt j: HInt i: _)    -> HInt (i + j); xs -> f s xs
        FprimSubInt        -> Just $ \s -> \case (HInt j: HInt i: _)    -> HInt (i - j); xs -> f s xs
        FprimModInt        -> Just $ \s -> \case (HInt j: HInt i: _)    -> HInt (i `mod` j); xs -> f s xs
        FprimSqrtFloat     -> Just $ \s -> \case (HFloat i: _)          -> HFloat $ sqrt i; xs -> f s xs
        FprimRound         -> Just $ \s -> \case (HFloat i: _)          -> HInt $ round i; xs -> f s xs
        FprimIntToFloat    -> Just $ \s -> \case (HInt i: _)            -> HFloat $ fromIntegral i; xs -> f s xs
        FprimIntToNat      -> Just $ \s -> \case (HInt i: _)            -> ENat $ fromIntegral i; xs -> f s xs
        FprimCompareInt    -> Just $ \s -> \case (HInt y: HInt x: _)    -> mkOrdering $ x `compare` y; xs -> f s xs
        FprimCompareFloat  -> Just $ \s -> \case (HFloat y: HFloat x: _) -> mkOrdering $ x `compare` y; xs -> f s xs
        FprimCompareChar   -> Just $ \s -> \case (HChar y: HChar x: _)  -> mkOrdering $ x `compare` y; xs -> f s xs
        FprimCompareString -> Just $ \s -> \case (HString y: HString x: _) -> mkOrdering $ x `compare` y; xs -> f s xs

        -- LambdaCube 3D specific primitives
        FPrimGreaterThan   -> Just $ \s -> \case (y: x: _{-7-}) | Just r <- twoOpBool (>) x y -> r; xs -> f s xs
        FPrimGreaterThanEqual
                           -> Just $ \s -> \case (y: x: _{-7-}) | Just r <- twoOpBool (>=) x y -> r; xs -> f s xs
        FPrimLessThan      -> Just $ \s -> \case (y: x: _{-7-}) | Just r <- twoOpBool (<)  x y -> r; xs -> f s xs
        FPrimLessThanEqual -> Just $ \s -> \case (y: x: _{-7-}) | Just r <- twoOpBool (<=) x y -> r; xs -> f s xs
        FPrimEqualV        -> Just $ \s -> \case (y: x: _{-7-}) | Just r <- twoOpBool (==) x y -> r; xs -> f s xs
        FPrimNotEqualV     -> Just $ \s -> \case (y: x: _{-7-}) | Just r <- twoOpBool (/=) x y -> r; xs -> f s xs
        FPrimEqual         -> Just $ \s -> \case (y: x: _{-3-}) | Just r <- twoOpBool (==) x y -> r; xs -> f s xs
        FPrimNotEqual      -> Just $ \s -> \case (y: x: _{-3-}) | Just r <- twoOpBool (/=) x y -> r; xs -> f s xs
        FPrimSubS          -> Just $ \s -> \case (y: x: _{-4-}) | Just r <- twoOp (-) x y -> r; xs -> f s xs
        FPrimSub           -> Just $ \s -> \case (y: x: _{-2-}) | Just r <- twoOp (-) x y -> r; xs -> f s xs
        FPrimAddS          -> Just $ \s -> \case (y: x: _{-4-}) | Just r <- twoOp (+) x y -> r; xs -> f s xs
        FPrimAdd           -> Just $ \s -> \case (y: x: _{-2-}) | Just r <- twoOp (+) x y -> r; xs -> f s xs
        FPrimMulS          -> Just $ \s -> \case (y: x: _{-4-}) | Just r <- twoOp (*) x y -> r; xs -> f s xs
        FPrimMul           -> Just $ \s -> \case (y: x: _{-2-}) | Just r <- twoOp (*) x y -> r; xs -> f s xs
        FPrimDivS          -> Just $ \s -> \case (y: x: _{-5-}) | Just r <- twoOp_ (/) div x y -> r; xs -> f s xs
        FPrimDiv           -> Just $ \s -> \case (y: x: _{-5-}) | Just r <- twoOp_ (/) div x y -> r; xs -> f s xs
        FPrimModS          -> Just $ \s -> \case (y: x: _{-5-}) | Just r <- twoOp_ modF mod x y -> r; xs -> f s xs
        FPrimMod           -> Just $ \s -> \case (y: x: _{-5-}) | Just r <- twoOp_ modF mod x y -> r; xs -> f s xs
        FPrimNeg           -> Just $ \s -> \case (x: _{-1-}) | Just r <- oneOp negate x -> r; xs -> f s xs
        FPrimAnd           -> Just $ \s -> \case (EBool y: EBool x: _) -> EBool (x && y); xs -> f s xs
        FPrimOr            -> Just $ \s -> \case (EBool y: EBool x: _) -> EBool (x || y); xs -> f s xs
        FPrimXor           -> Just $ \s -> \case (EBool y: EBool x: _) -> EBool (x /= y); xs -> f s xs
        FPrimNot           -> Just $ \s -> \case (EBool x: _: _: (hnf -> TNat): _) -> EBool $ not x; xs -> f s xs

        _ -> Nothing

    f s xs = Neut $ Fun_ s fn xs delta

    twoOpBool :: (forall a . Ord a => a -> a -> Bool) -> Exp -> Exp -> Maybe Exp
    twoOpBool f (HFloat x)  (HFloat y)  = Just $ EBool $ f x y
    twoOpBool f (HInt x)    (HInt y)    = Just $ EBool $ f x y
    twoOpBool f (HString x) (HString y) = Just $ EBool $ f x y
    twoOpBool f (HChar x)   (HChar y)   = Just $ EBool $ f x y
    twoOpBool f (ENat x)    (ENat y)    = Just $ EBool $ f x y
    twoOpBool _ _ _                     = Nothing

    oneOp :: (forall a . Num a => a -> a) -> Exp -> Maybe Exp
    oneOp f = oneOp_ f f

    oneOp_ f _ (HFloat x) = Just $ HFloat $ f x
    oneOp_ _ f (HInt x)   = Just $ HInt $ f x
    oneOp_ _ _ _          = Nothing

    twoOp :: (forall a . Num a => a -> a -> a) -> Exp -> Exp -> Maybe Exp
    twoOp f = twoOp_ f f

    twoOp_ f _ (HFloat x) (HFloat y) = Just $ HFloat $ f x y
    twoOp_ _ f (HInt x) (HInt y)     = Just $ HInt $ f x y
    twoOp_ _ _ _ _                   = Nothing

    modF x y = x - fromIntegral (floor (x / y)) * y

mkFunDef a t = FunName a 0 NoDef t


evalCaseFun _ a ps (Con n@(ConName _ i _) _ vs)
    | i /= (-1) = foldlrev app_ (ps !!! (i + 1)) vs
    | otherwise = error "evcf"
  where
    xs !!! i | i >= length xs = error $ "!!! " ++ ppShow a ++ " " ++ show i ++ " " ++ ppShow n ++ "\n" ++ ppShow ps
    xs !!! i = xs !! i
evalCaseFun fs a b (Reduced c) = evalCaseFun fs a b c
evalCaseFun fs a b (Neut c) = Neut $ CaseFun__ fs a b c
evalCaseFun _ a b x = error $ "evalCaseFun: " ++ ppShow (a, x)

evalCaseFun' a b c = evalCaseFun (getFreeVars c <> foldMap getFreeVars b) a b c

evalTyCaseFun a b c = evalTyCaseFun_ (foldMap getFreeVars b <> getFreeVars c) a b c

evalTyCaseFun_ s a b (Reduced c) = evalTyCaseFun_ s a b c
evalTyCaseFun_ s a b (Neut c) = Neut $ TyCaseFun__ s a b c
evalTyCaseFun_ _ (TyCaseFunName (FTag F'Type) ty) (_: t: f: _) TType = t
evalTyCaseFun_ _ (TyCaseFunName n ty) (_: t: f: _) (TyCon (TyConName n' _ _ _ _) vs) | n == n' = foldlrev app_ t vs
--evalTyCaseFun (TyCaseFunName n ty) [_, t, f] (DFun (FunName n' _) vs) | n == n' = foldl app_ t vs  -- hack
evalTyCaseFun_ _ (TyCaseFunName n ty) (_: t: f: _) _ = f

evalCoe a b (Reduced x) d = evalCoe a b x d
evalCoe a b TT d          = d
evalCoe a b t d           = Coe a b t d

cstr_ t a b = cw $ cstr t a b

cstr = f []
  where
    f z ty a a' = f_ z (hnf ty) (hnf a) (hnf a')

    f_ _ _ a a' | a == a' = CUnit
    f_ ns typ (RHS a) (RHS a') = f ns typ a a'
    f_ ns typ (Con a n xs) (Con a' n' xs') | a == a' && n == n' && length xs == length xs' =
        ff ns (foldl appTy (conType typ a) $ mkConPars n typ) $ reverse $ zip xs xs'
    f_ ns typ (TyCon a xs) (TyCon a' xs') | a == a' && length xs == length xs' =
        ff ns (nType a) $ reverse $ zip xs xs'
    f_ (_: ns) typ{-down?-} (down 0 -> Just a) (down 0 -> Just a') = f ns typ a a'
    f_ ns TType (Pi h a b) (Pi h' a' b') | h == h' = t2 (f ns TType a a') (f ((a, a'): ns) TType b b')

    f_ [] TType (UFun F'VecScalar [b, a]) (UFun F'VecScalar [b', a']) = t2 (f [] TNat a a') (f [] TType b b')
    f_ [] TType (UFun F'VecScalar [b, a]) (TVec a' b') = t2 (f [] TNat a a') (f [] TType b b')
    f_ [] TType (UFun F'VecScalar [b, a]) t@NonNeut = t2 (f [] TNat a (ENat 1)) (f [] TType b t)
    f_ [] TType (TVec a' b') (UFun F'VecScalar [b, a]) = t2 (f [] TNat a' a) (f [] TType b' b)
    f_ [] TType t@NonNeut (UFun F'VecScalar [b, a]) = t2 (f [] TNat a (ENat 1)) (f [] TType b t)

    f_ [] typ a@Neut{} a' = CstrT typ a a'
    f_ [] typ a a'@Neut{} = CstrT typ a a'
    f_ ns typ a a' = CEmpty $ simpleShow $ nest 2 ("can not unify" <$$> DTypeNamespace True (pShow a)) <$$> nest 2 ("with" <$$> DTypeNamespace True (pShow a'))

    ff _ _ [] = CUnit
    ff ns tt@(Pi v t _) ((t1, t2'): ts) = t2 (f ns t t1 t2') $ ff ns (appTy tt t1) ts
    ff ns t zs = error $ "ff: " -- ++ show (a, n, length xs', length $ mkConPars n typ) ++ "\n" ++ ppShow (nType a) ++ "\n" ++ ppShow (foldl appTy (nType a) $ mkConPars n typ) ++ "\n" ++ ppShow (zip xs xs') ++ "\n" ++ ppShow zs ++ "\n" ++ ppShow t

pattern NonNeut <- (nonNeut -> True)

nonNeut Neut{} = False
nonNeut _      = True

t2C (hnf -> TT) (hnf -> TT) = TT
t2C a b                     = DFun Ft2C [b, a]

cw_ _ (hnf -> CUnit)    = Unit
cw_ _ (hnf -> CEmpty a) = Empty a
cw_ s a                 = CW_ s a

cw a = cw_ (getFreeVars a) a

t2_ _ (hnf -> CUnit) a                    = a
t2_ _ a (hnf -> CUnit)                    = a
t2_ _ (hnf -> CEmpty a) (hnf -> CEmpty b) = CEmpty (a <> b)
t2_ _ (hnf -> CEmpty s) _                 = CEmpty s
t2_ _ _ (hnf -> CEmpty s)                 = CEmpty s
t2_ s a b                                 = T2 s a b

t2 a b = t2_ (getFreeVars a <> getFreeVars b) a b

app_ :: Exp -> Exp -> Exp
app_ a b = app__ (getFreeVars a <> getFreeVars b) a b

app__ _ (Lam x) a = subst 0 a x
app__ _ (Con s n xs) a = if n < conParams s then Con s (n+1) xs else Con s n (a: xs)
app__ _ (TyCon s xs) a = TyCon s (a: xs)
app__ _ (SubstLet f) a = app_ f a
app__ s (Neut f) a = neutApp f a
  where
    neutApp (ReducedN x) a = app_ x a
    neutApp (Fun_ db f xs (Lam e)) a = mkFun_ (db <> getFreeVars a) f (a: xs) (subst 0 a e)
    neutApp f a = Neut $ App__ s f a

conType (snd . getParams . hnf -> TyCon (TyConName _ _ _ cs _) _) (ConName _ n t) = t --snd $ cs !! n

appTy (Pi _ a b) x = subst 0 x b
appTy t x          = error $ "appTy: " ++ ppShow t

getParams :: Exp -> ([(Visibility, Exp)], Exp)
getParams (Pi h a b) = first ((h, a):) $ getParams b
getParams x          = ([], x)

--------------------------------------------------------- evident types

class NType a where nType :: a -> Type

instance NType FunName       where nType (FunName _ _ _ t) = t
instance NType TyConName     where nType (TyConName _ _ t _ _) = t
instance NType CaseFunName   where nType (CaseFunName _ t _) = t
instance NType TyCaseFunName where nType (TyCaseFunName _ t) = t

instance NType Lit where
    nType = \case
        LInt _    -> TInt
        LFloat _  -> TFloat
        LString _ -> TString
        LChar _   -> TChar

-------------------------------------------------------- exp serialization

trace :: String -> a -> a
trace _ = id

closeType_ msg fuel _ = STOP $ msg ++ " " ++ show fuel

-- TODO: check MkDoc
closeTyConName fuel (TyConName fname ints type_ cons caseFunName)
  = trace (printf "TyConName %s %d" (show fname) fuel) $ TyConName fname ints (closeType_ "TyConName" fuel type_) [{-(closeConName fuel n, closeType_ "TyConName con" fuel t) | (n,t) <- cons-}] (closeCaseFunName fuel caseFunName)

closeTyCaseFunName fuel (TyCaseFunName fname type_)
  = trace (printf "TyCaseFunName %s %d" (show fname) fuel) $ TyCaseFunName fname (closeType_ "TyCaseFunName" fuel type_)

closeConName fuel (ConName fname ordinal type_)
  = trace (printf "ConName %s %d" (show fname) fuel) $ ConName fname ordinal (closeType fuel type_) --(STOP "ConName")

closeCaseFunName fuel (CaseFunName fname type_ int)
  = trace (printf "CaseFunName %s %d" (show fname) fuel) $ CaseFunName fname (closeType_ "CaseFunName" fuel type_) int

closeFunName fuel (FunName fname int funDef type_)
  = trace (printf "FunName %s %d" (show fname) fuel) $ FunName fname int (closeFunDef funDef) (closeType_ "FunName" fuel type_)

closeFunDef = \case
  ExpDef exp -> ExpDef (closeExp exp)
  x          -> x

_maxFuel = 25 :: Int -- 6

closeType :: Int -> Exp -> Exp
--closeType fuel | fuel <= 0 = \_ -> STOP "out of fuel"
closeType fuel = \case
  Lam_   freeVars exp -> tr "Lam_" $ Lam_ freeVars (closeType_ "Lam_" _fuel exp)
  Con_   freeVars conName noErasedApplied argsReversed -> tr "Con_" $ Con_ freeVars (closeConName _fuel conName) noErasedApplied []
  TyCon_ freeVars tyConName argsReversed -> tr "TyCon_" $ TyCon_ freeVars (closeTyConName _fuel tyConName) []
  Pi_    freeVars visibility exp1 exp2 -> tr "Pi_" $ Pi_ freeVars visibility (closeType_ "Pi_ 1" _fuel exp1) (closeType _fuel exp2)
  Neut   neutral -> STOP "Neut"
  RHS    exp -> tr "RHS" $ RHS (closeType_ "RHS" _fuel exp)
  Let_   freeVars expType exp -> (STOP "Let_")
  Up_    freeVars ints exp -> tr "Up_" $ Up_ freeVars ints (closeType_ "Up_" _fuel exp)
  e -> e
 where
  _fuel = pred fuel
  tr :: String -> a -> a
  tr msg = trace (printf "%s %d" msg fuel)


closeExp :: Exp -> Exp
closeExp = \case
  Lam_   freeVars exp -> Lam_ freeVars $ closeExp exp
  Con_   freeVars conName noErasedApplied argsReversed -> Con_ freeVars (closeConName _maxFuel conName) noErasedApplied (map closeExp argsReversed)
  TyCon_ freeVars tyConName argsReversed -> TyCon_ freeVars (closeTyConName _maxFuel tyConName) (map closeExp argsReversed)
  Pi_    freeVars visibility exp1 exp2 -> Pi_ freeVars visibility (closeExp exp1) (closeExp exp2)
  Neut   neutral -> Neut $ closeNeutral neutral
  RHS    exp -> RHS $ closeExp exp
  Let_   freeVars expType exp -> Let_ freeVars (closeExpType expType) (closeExp exp)
  Up_    freeVars ints exp -> Up_ freeVars ints (closeExp exp)
  e -> e

closeNeutral :: Neutral -> Neutral
closeNeutral = \case
  App__       freeVars neutral exp -> App__ freeVars (closeNeutral neutral) (closeExp exp)
  CaseFun__   freeVars caseFunName   exps neutral -> CaseFun__ freeVars (closeCaseFunName _maxFuel caseFunName) (map closeExp exps) (closeNeutral neutral)
  TyCaseFun__ freeVars tyCaseFunName exps neutral -> TyCaseFun__ freeVars (closeTyCaseFunName _maxFuel tyCaseFunName) (map closeExp exps) (closeNeutral neutral)
  Fun_        freeVars funName exps exp -> Fun_ freeVars (closeFunName _maxFuel funName) (map closeExp exps) (STOP "Fun_") --(closeExp exp)
  UpN_        freeVars ints neutral -> UpN_ freeVars ints (closeNeutral neutral)
  n -> n

closeExpType :: ExpType -> ExpType
closeExpType (ET e t) = ET (closeExp e) (closeType_ "ExpType Type" _maxFuel t)

instance Binary ExpType
instance Binary Exp
instance Binary Neutral
instance Binary Freq
instance Binary ConName
instance Binary CaseFunName
instance Binary TyConName
instance Binary TyCaseFunName

instance Binary FunName where -- do FunName/FunDef instance together
  put (FunName fName int funDef type_) = do
    put fName
    put int
    put type_
    case funDef of
      NoDef      -> put (0 :: Word8)
      DeltaDef{} -> put (1 :: Word8)
      ExpDef exp -> put (2 :: Word8) >> put exp

  get = do
    fName <- get
    int <- get
    type_ <- get
    (get :: Get Word8) >>= \case
      0 -> pure $ FunName fName int NoDef type_
      1 -> pure $ mkFunDef fName type_
      2 -> (\exp -> FunName fName int (ExpDef exp) type_) <$> get
