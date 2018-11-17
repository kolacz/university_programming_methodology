-- Wymagamy, by moduł zawierał tylko bezpieczne funkcje
{-# LANGUAGE Safe #-}
-- Definiujemy moduł zawierający rozwiązanie.

module Eval (typecheck, eval) where

-- Importujemy moduły z definicją języka oraz typami potrzebnymi w zadaniu
import AST
import DataTypes

data Val  = VNum Integer | VBool Bool | VUnit | VPair Val Val | VCons Val Val | VNil
type Error p = (p, ErrKind)
data ErrKind = EUndefinedVariable Var | EUndefinedFunction FSym
             | ETypeMismatch Type Type | EBranchMismatch Type Type
             | EPairMismatch Type | EListMismatch Type
type Env a = [(Var, a)]
type IRes p = Either (Error p) Type

instance Show ErrKind where
  show (EUndefinedVariable x)  =
    "Undefined variable " ++ show x ++ "."
  show (EUndefinedFunction x)  =
    "Undefined function " ++ show x ++ "."
  show (ETypeMismatch t1 t2)   =
    "Type mismatch: expected " ++ show t1 ++ " but received " ++ show t2 ++ "."
  show (EBranchMismatch t1 t2) =
    "Type mismatch in the branches: " ++ show t1 ++ " and " ++ show t2 ++ "."
  show (EPairMismatch t)       =
    "Type mismatch: expected a pair, but received " ++ show t ++ "."
  show (EListMismatch t)       =
    "Type mismatch: expected a list, but received " ++ show t ++ "."

infixr 6 $>

($>) :: Maybe a -> Either a b -> Either a b
Just e  $> _ = Left e
Nothing $> e = e

inferType :: Env (Type, Type) -> Env Type -> Expr p -> IRes p
inferType φ γ (EVar p x) =
  case lookup x γ of
    Just t  -> Right t
    Nothing -> Left (p, EUndefinedVariable x)
inferType φ γ (ENum _ _)  = Right TInt
inferType φ γ (EBool _ _) = Right TBool
inferType φ γ (EUnary _ op e) =
  checkType φ γ e ta $>
  Right tr
  where (ta, tr) = uopType op
inferType φ γ (EBinary _ op e1 e2) =
  checkType φ γ e1 et1 $>
  checkType φ γ e2 et2 $>
  Right tr
  where (et1, et2, tr) = bopType op
inferType φ γ (ELet _ x ex eb) =
  case inferType φ γ ex of
    Left err -> Left err
    Right tx -> inferType φ ((x, tx) : γ) eb
inferType φ γ (EIf p ec et ef) =
  checkType φ γ ec TBool $>
  checkEqual p (inferType φ γ et) (inferType φ γ ef)
inferType φ γ (EApp p f e) =
  case lookup f φ of
    Just (ta, tr) -> checkType φ γ e ta $> Right tr
    Nothing -> Left (p, EUndefinedFunction f)
inferType φ γ (EUnit _) = Right TUnit
inferType φ γ (EPair _ e1 e2) =
  case (inferType φ γ e1, inferType φ γ e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right t1, Right t2) -> Right $ TPair t1 t2
inferType φ γ (EFst p e) =
  fmap fst . checkPair p $ inferType φ γ e
inferType φ γ (ESnd p e) =
  fmap snd . checkPair p $ inferType φ γ e
inferType φ γ (ENil p t) =
  fmap TList . checkList p $ Right t
inferType φ γ (ECons p eh et) =
  fmap TList $ checkEqual p (inferType φ γ eh) (checkList (getData et) $ inferType φ γ et)
inferType φ γ (EMatchL p e en (x, xs, ec)) =
  case checkList (getData e) $ inferType φ γ e of
    Right t  -> checkEqual p (inferType φ γ en) (inferType φ ((x, t):(xs, TList t):γ) ec)
    Left err -> Left err

checkPair :: p -> IRes p -> Either (Error p) (Type, Type)
checkPair _ (Right (TPair t1 t2)) = Right (t1, t2)
checkPair p (Right t)  = Left $ (p, EPairMismatch t)
checkPair _ (Left err) = Left err

checkList :: p -> IRes p -> IRes p
checkList _ (Right (TList t)) = Right t
checkList p (Right t)  = Left $ (p, EListMismatch t)
checkList _ (Left err) = Left err

checkEqual :: p -> IRes p -> IRes p -> IRes p
checkEqual _ (Left err) _ = Left err
checkEqual _ _ (Left err) = Left err
checkEqual p (Right t1) (Right t2) =
  if t1 == t2 then Right t1
  else Left (p, EBranchMismatch t1 t2)

checkType :: Env (Type, Type) -> Env Type -> Expr p -> Type -> Maybe (Error p)
checkType φ γ e t =
  case inferType φ γ e of
    Left err -> Just err
    Right t' -> if t == t' then Nothing else Just (getData e, ETypeMismatch t' t)

uopType :: UnaryOperator -> (Type, Type)
uopType UNot = (TBool, TBool)
uopType UNeg = (TInt,  TInt)

bopType e = case e of
  BAnd -> tbool
  BOr  -> tbool
  BEq  -> tcomp
  BNeq -> tcomp
  BLt  -> tcomp
  BLe  -> tcomp
  BGt  -> tcomp
  BGe  -> tcomp
  BAdd -> tarit
  BSub -> tarit
  BMul -> tarit
  BDiv -> tarit
  BMod -> tarit
  where tbool = (TBool, TBool, TBool)
        tcomp = (TInt,  TInt,  TBool)
        tarit = (TInt,  TInt,  TInt)

-- Funkcja sprawdzająca typy
-- Dla wywołania typecheck vars e zakładamy, że zmienne występujące
-- w vars są już zdefiniowane i mają typ int, i oczekujemy by wyrażenia e
-- miało typ int
typecheck :: [FunctionDef p] -> [Var] -> Expr p -> TypeCheckResult p
typecheck fs vs e =
  case maybe (checkType φ γ e TInt) Just $ foldl comb Nothing fs of
    Nothing  -> Ok
    Just (p, err) -> Error p $ show err
  where γ = map (\ x -> (x, TInt)) vs
        φ = map (\ f -> (funcName f, (funcArgType f, funcResType f))) fs
        checkFD f = checkType φ [(funcArg f, funcArgType f)] (funcBody f) (funcResType f)
        comb a f = maybe (checkFD f) Just a

ev :: Env (Var, Expr p) -> Env Val -> Expr p -> Maybe Val
ev φ σ (EVar _ x)  = lookup x σ
ev φ σ (ENum _ n)  = Just $ VNum n
ev φ σ (EBool _ b) = Just $ VBool b
ev φ σ (EUnary _ op e) =
  case ev φ σ e of
    Just v -> evUOp op v
    Nothing -> Nothing
ev φ σ (EBinary _ op e1 e2) =
  case (ev φ σ e1, ev φ σ e2) of
    (Just v1, Just v2) -> evBOp op v1 v2
    _ -> Nothing
ev φ σ (ELet _ x ex eb) =
  case ev φ σ ex of
    Just v -> ev φ ((x, v) : σ) eb
    Nothing -> Nothing
ev φ σ (EIf _ ec et ef) =
  case ev φ σ ec of
    Just (VBool True)  -> ev φ σ et
    Just (VBool False) -> ev φ σ ef
    _ -> Nothing
ev φ σ (EApp _ f e) =
  let Just (x, e') = lookup f φ
  in case ev φ σ e of
    Just v -> ev φ [(x, v)] e'
    Nothing -> Nothing
ev φ σ (EUnit _) = Just VUnit
ev φ σ (EPair _ e1 e2) =
  case (ev φ σ e1, ev φ σ e2) of
    (Just v1, Just v2) -> Just $ VPair v1 v2
    _ -> Nothing
ev φ σ (EFst _ e) =
  case ev φ σ e of
    Just (VPair v _) -> Just v
    Nothing -> Nothing
ev φ σ (ESnd _ e) =
  case ev φ σ e of
    Just (VPair _ v) -> Just v
    Nothing -> Nothing
ev φ σ (ENil _ _) = Just VNil
ev φ σ (ECons _ eh et) =
  case (ev φ σ eh, ev φ σ et) of
    (Just vh, Just vt) -> Just $ VCons vh vt
    _ -> Nothing
ev φ σ (EMatchL _ e en (x, xs, ec)) =
  case ev φ σ e of
    Just VNil -> ev φ σ en
    Just (VCons vh vt) -> ev φ ((x, vh):(xs, vt):σ) ec
    Nothing -> Nothing

evUOp UNot (VBool b) = Just . VBool $ not b
evUOp UNeg (VNum n)  = Just . VNum $ -n

evBOp BAnd (VBool b1) (VBool b2) = Just . VBool $ b1 && b2
evBOp BOr  (VBool b1) (VBool b2) = Just . VBool $ b1 || b2
evBOp BEq  (VNum n1)  (VNum n2)  = Just . VBool $ n1 == n2
evBOp BNeq (VNum n1)  (VNum n2)  = Just . VBool $ n1 /= n2
evBOp BLt  (VNum n1)  (VNum n2)  = Just . VBool $ n1 <  n2
evBOp BLe  (VNum n1)  (VNum n2)  = Just . VBool $ n1 <= n2
evBOp BGt  (VNum n1)  (VNum n2)  = Just . VBool $ n1 >  n2
evBOp BGe  (VNum n1)  (VNum n2)  = Just . VBool $ n1 >= n2
evBOp BAdd (VNum n1)  (VNum n2)  = Just . VNum  $ n1 + n2
evBOp BSub (VNum n1)  (VNum n2)  = Just . VNum  $ n1 - n2
evBOp BMul (VNum n1)  (VNum n2)  = Just . VNum  $ n1 * n2
evBOp BDiv (VNum n1)  (VNum n2)
  | n2 == 0   = Nothing
  | otherwise = Just . VNum  $ n1 `div` n2
evBOp BMod (VNum n1)  (VNum n2)
  | n2 == 0   = Nothing
  | otherwise = Just . VNum  $ n1 `mod` n2

-- Funkcja obliczająca wyrażenia
-- Dla wywołania eval input e przyjmujemy, że dla każdej pary (x, v)
-- znajdującej się w input, wartość zmiennej x wynosi v.
-- Możemy założyć, że wyrażenie e jest dobrze typowane, tzn.
-- typecheck (map fst input) e = Ok
eval :: [FunctionDef p] -> [(Var,Integer)] -> Expr p -> EvalResult
eval fs args e =
  case ev φ σ e of
    Just (VNum n) -> Value n
    Just _        -> undefined -- impossible (tc)
    Nothing       -> RuntimeError
  where σ = map (\(x, n) -> (x, VNum n)) args
        φ = map (\f -> (funcName f, (funcArg f, funcBody f))) fs