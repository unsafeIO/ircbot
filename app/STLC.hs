{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module STLC where

import Control.Monad.Except
import Control.Monad.State
import Data.Coerce
import Prettyprinter

data Term
  = Lit Lit
  | Var Int
  | App Term Term
  | ArthAdd Term Term
  | ArthMul Term Term
  | If Term Term Term
  | Abs Type Term
  deriving (Eq, Show, Ord)

data Lit
  = LInt Int
  | LBool Bool
  deriving (Eq, Show, Ord)

data Type
  = TInt
  | TBool
  | TArr Type Type
  deriving (Eq, Show, Ord)

newtype Binding = VarBinding Type
  deriving (Eq, Show, Ord)

data TypeErr
  = Mismatch Type Type
  | NotFunction Type
  | NotInScope Int

-----------------------------------------------------------------------------

type Context = [Binding]

type TC = ExceptT TypeErr (State Context)

extend :: Binding -> TC ()
extend b = modify $ (:) b

getByIndex :: Int -> TC Binding
getByIndex n = do
  ctx <- get
  if length ctx > n
    then pure $ ctx !! n
    else throwError $ NotInScope n

-----------------------------------------------------------------------------

shiftTerm :: Int -> Term -> Term
shiftTerm d = go 0
  where
    go c (Var x)
      | x >= c = Var (x + d)
      | otherwise = Var x
    go c (Abs ty t) = Abs ty (go (c + 1) t)
    go c (App t1 t2) = App (go c t1) (go c t2)
    go c (ArthAdd t1 t2) = ArthAdd (go c t1) (go c t2)
    go c (ArthMul t1 t2) = ArthMul (go c t1) (go c t2)
    go c (If t1 t2 t3) = If (go c t1) (go c t2) (go c t3)
    go _ l@Lit {} = l

substTerm :: Int -> Term -> Term -> Term
substTerm j s = go 0
  where
    go c t@(Var x)
      | x == j + c = shiftTerm c s
      | otherwise = t
    go c (Abs ty t) = Abs ty $ go (c + 1) t
    go c (App t1 t2) = App (go c t1) (go c t2)
    go c (ArthAdd t1 t2) = ArthAdd (go c t1) (go c t2)
    go c (ArthMul t1 t2) = ArthMul (go c t1) (go c t2)
    go c (If t1 t2 t3) = If (go c t1) (go c t2) (go c t3)
    go _ l@Lit {} = l

apply :: Term -> Term -> Term
apply s t = shiftTerm (-1) $ substTerm 0 (shiftTerm 1 s) t

isValue :: Term -> Bool
isValue Lit {} = True
isValue Abs {} = True
isValue _ = False

eval :: Term -> Maybe Term
eval (App (Abs _ t1) v2)
  | isValue v2 = Just $ apply v2 t1
eval (App t1 t2)
  | isValue t1 = App <$> Just t1 <*> eval t2
  | otherwise = App <$> eval t1 <*> Just t2
eval (ArthAdd t1 t2)
  | Lit (LInt a) <- t1,
    Lit (LInt b) <- t2 =
    Just . Lit . LInt $ a + b
eval (ArthMul t1 t2)
  | Lit (LInt a) <- t1,
    Lit (LInt b) <- t2 =
    Just . Lit . LInt $ a * b
eval (If t1 t2 t3)
  | Lit (LBool c) <- t1 = Just $ if c then t2 else t3
eval _ = Nothing

eval' :: Term -> Term
eval' t = case eval t of
  Just x -> eval' x
  _ -> t

-----------------------------------------------------------------------------

check :: Term -> TC Type
check t = case t of
  (Var x) -> coerce <$> getByIndex x
  (Abs ty1 t2) -> do
    extend $ VarBinding ty1
    ty2 <- check t2
    pure $ TArr ty1 ty2
  (App t1 t2) -> do
    ty1 <- check t1
    ty2 <- check t2
    case ty1 of
      TArr t11 t12 ->
        if t11 == ty2
          then pure t12
          else throwError $ Mismatch t11 ty2
      _ -> throwError $ NotFunction ty1
  (ArthAdd t1 t2) -> do
    ty1 <- check t1
    ty2 <- check t2
    case ty1 of
      TInt -> case ty2 of
        TInt -> pure TInt
        _ -> throwError $ Mismatch ty2 TInt
      _ -> throwError $ Mismatch ty1 TInt
  (ArthMul t1 t2) -> do
    ty1 <- check t1
    ty2 <- check t2
    case ty1 of
      TInt -> case ty2 of
        TInt -> pure TInt
        _ -> throwError $ Mismatch ty2 TInt
      _ -> throwError $ Mismatch ty1 TInt
  (If t1 t2 t3) -> do
    ty1 <- check t1
    ty2 <- check t2
    ty3 <- check t3
    case ty1 of
      TBool -> if ty2 == ty3 then pure ty2 else throwError $ Mismatch ty2 ty3
      _ -> throwError $ Mismatch ty1 TBool
  (Lit LInt {}) -> pure TInt
  (Lit LBool {}) -> pure TBool

-----------------------------------------------------------------------------

prettyType :: Type -> Doc ann
prettyType TInt = "Int"
prettyType TBool = "Bool"
prettyType (TArr ty1 ty2) = prettyType ty1 <+> "->" <+> prettyType ty2

prettyLit :: Lit -> Doc ann
prettyLit (LBool b) = pretty b
prettyLit (LInt n) = pretty n

prettyVar :: Int -> Doc ann
prettyVar n = "#" <> pretty n

prettyAbs :: Type -> Term -> Doc ann
prettyAbs ty t = "(λ#" <+> colon <+> prettyType ty <> dot <+> prettyTerm t <> ")"

prettyApp :: Term -> Term -> Doc ann
prettyApp t1 t2 = "(" <> prettyTerm t1 <+> prettyTerm t2 <> ")"

prettyTerm :: Term -> Doc ann
prettyTerm (Lit x) = prettyLit x
prettyTerm (Var n) = prettyVar n
prettyTerm (App t1 t2) = prettyApp t1 t2
prettyTerm (Abs ty t) = prettyAbs ty t
prettyTerm (ArthAdd t1 t2) = prettyTerm t1 <+> "+" <+> prettyTerm t2
prettyTerm (ArthMul t1 t2) = prettyTerm t1 <+> "*" <+> prettyTerm t2
prettyTerm (If t1 t2 t3) =
  "if" <+> "(" <> prettyTerm t1 <> ")" <+> "then" <+> "(" <> prettyTerm t2 <> ")" <+> "else" <+> "(" <> prettyTerm t3 <> ")"

prettyErr :: TypeErr -> Doc ann
prettyErr (Mismatch ty1 ty2) = "Can not match" <+> prettyType ty1 <+> "with" <+> prettyType ty2
prettyErr (NotFunction ty) = "Not a function" <> colon <+> prettyType ty
prettyErr (NotInScope n) = "Not in scope" <> colon <+> prettyVar n

prettyResult :: Either TypeErr (Doc ann) -> Doc ann
prettyResult (Left err) = prettyErr err
prettyResult (Right x) = x

-----------------------------------------------------------------------------

runTC :: TC a -> Either TypeErr a
runTC m = let j = runExceptT m in evalState j []

run :: Term -> Doc ann
run t = prettyResult . runTC $ do
  ty <- check t
  let t' = eval' t
  pure $ prettyTerm t' <+> colon <+> prettyType ty

-----------------------------------------------------------------------------

int :: Int -> Term
int = Lit . LInt

bool :: Bool -> Term
bool = Lit . LBool

plus1 :: Term
plus1 = Abs TInt $ Abs TInt $ ArthAdd (Var 0) (Lit $ LInt 1)

-----------------------------------------------------------------------------
