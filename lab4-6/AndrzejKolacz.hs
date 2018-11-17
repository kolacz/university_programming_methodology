-- Wymagamy, by moduł zawierał tylko bezpieczne funkcje
{-# LANGUAGE Safe #-}
-- Definiujemy moduł zawierający rozwiązanie.

module AndrzejKolacz (typecheck, eval) where

-- Importujemy moduły z definicją języka oraz typami potrzebnymi w zadaniu
import AST
import DataTypes
import Data.Map (Map)
import Data.List
import qualified Data.Map as M

---------------------------------------------------------
-------- SPRAWDZANIE TYPÓW ------------------------------
---------------------------------------------------------

-- Sposób reprezentowania środowiska
type Env a   = Map Var a 

-- Typ reprezentujący błąd
type Error p = TypeCheckResult p

-- Funkcja definiująca środowisko początkowe
env_funs :: [FunctionDef p] -> Env Type 
env_funs list = aux (M.empty) list where
                    aux map [] = map
                    aux map ((FunctionDef _ f _ t1 t2 _):xs) = 
                        aux (M.insert f (TArrow t1 t2) map) xs  

-- Funkcja wzbogacająca środowisko z funkcjami o zmienne wejściowe
env_vars :: Env Type  -> [Var] -> Env Type 
env_vars env list = aux env list where
                    aux map [] = map
                    aux map (x:xs) = aux (M.insert x TInt map) xs

-- Funkcja rozszerzająca środowisko o zmienne
env_extend :: Env a -> Var -> a -> Env a -- w sprawdzaniu typów a = Type, w interpretacji a = Val p
env_extend map x t = M.insert x t map 

-- Funkcje do obsługi wartości typu Either z Data.Either.Unwrap
-- Funkcje "typecheck" oraz "eval" zostały napisane tak, by uniemożliwić wywołanie "error"

fromRight           :: Either a b -> b
fromRight (Left _)  = error "Either.Unwrap.fromRight: Argument takes form 'Left _'"
fromRight (Right x) = x

fromLeft            :: Either a b -> a
fromLeft (Right _) = error "Either.Unwrap.fromLeft: Argument takes form 'Right _'" 
fromLeft (Left x)  = x

-- Funkcje sprawdzające czy operator należy do konkretnej grupy operatorów
isArithmOp :: BinaryOperator -> Bool
isArithmOp     op = elem op [BAdd,BSub,BMul,BDiv,BMod]

isComparisonOp :: BinaryOperator -> Bool
isComparisonOp op = elem op [BEq,BNeq,BLt,BGt,BLe,BGe]

isBooleanOp :: BinaryOperator -> Bool
isBooleanOp    op = elem op [BAnd,BOr]

-- Funkcja wnioskująca typ wyrażenia
infer_type :: Eq p => Env Type -> Expr p -> Either (Error p) Type
infer_type env expr = case expr of
        EVar p x            -> case M.lookup x env of
                                  Just t  -> Right t
                                  Nothing -> Left (Error p "Undefined variable.")

        ENum p n            -> Right TInt
        EBool p b           -> Right TBool

        EUnary p UNeg e     -> if (infer_type env e == Right TInt) 
                               then Right TInt 
                               else Left (Error p "Expected type of the expression: Int.")

        EUnary p UNot e     -> if (infer_type env e == Right TBool) 
                               then Right TBool 
                               else Left (Error p "Expected type of the expression: Bool.")
                                  
        EBinary p op e1 e2  -> if isArithmOp op then
                                  if (infer_type env e1 == Right TInt)
                                  then 
                                     if (infer_type env e2 == Right TInt)
                                     then Right TInt 
                                     else Left (Error p "Expected type of the second expression: Int.")
                                  else Left (Error p "Expected type of the first expression: Int.")
                               else
                               if isComparisonOp op then
                                  if (infer_type env e1 == Right TInt)
                                  then 
                                     if (infer_type env e2 == Right TInt)
                                     then Right TBool 
                                     else Left (Error p "Expected type of the second expression: Int.")
                                  else Left (Error p "Expected type of the first expression: Int.")
                               else   
                               -- isBooleanOp op == True
                               if (infer_type env e1 == Right TBool)
                               then 
                                  if (infer_type env e2 == Right TBool)
                                  then Right TBool 
                                  else Left (Error p "Expected type of the second expression: Bool.")
                               else Left (Error p "Expected type of the first expression: Bool.")

        ELet p x e1 e2      -> let  t1 = (infer_type env e1) in
                               case t1 of
                                  Right _ -> infer_type (env_extend env x (fromRight t1)) e2 -- rozszerzanie środowiska
                                  Left  _ -> t1

        EIf p e1 e2 e3      -> let  t1 = (infer_type env e1) in
                               case t1 of
                                    Right TBool ->  let  t2 = (infer_type env e2) in
                                                    if  (t2 == infer_type env e3)
                                                    then t2 
                                                    else Left (Error p "Types of True-value and False-value differ.")
                                    Right _     -> Left (Error p "Condition should evaluate to Boolean value.")
                                    Left  _     -> t1

----------------------------------------------------

        EFn p x t1 e        -> let  t2 = (infer_type (env_extend env x t1) e) in
                               case t2 of
                                   Right _ -> Right (TArrow t1 (fromRight t2)) 
                                   Left  _ -> t2 -- błąd podczas wnioskowania typu wyrażenia e

        EApp p e1 e2        -> let  arg_type = (infer_type env e2) in
                               case arg_type of
                               Right _ -> case e1 of
                                     EVar _ x -> -- aplikujemy do zdefiniowanej zmiennej/funkcji
                                        case M.lookup x env of
                                             Just (TArrow t2 t1) -> 
                                                 if  (fromRight arg_type == t2)
                                                 then Right t1
                                                 else Left (Error p "Incorrect type of the argument.")
                                             Just _              -> 
                                                 Left (Error p "Arguments cannot be applied to variables.")
                                             Nothing             -> Left (Error (getData e1) "Undefined function.")

                                     _        -> -- aplikujemy do innego wyrażenia
                                        let  t = (infer_type env e1) in
                                        case t of
                                             Right (TArrow t2 t1) -> 
                                                 if  (fromRight arg_type == t2)
                                                 then Right t1
                                                 else Left (Error p "Incorrect type of the argument.")
                                             Right _              -> 
                                                 Left (Error p "First expression is not a function.")
                                             Left  _              ->  t -- błąd podczas wnioskowania typu wyrażenia

                               Left  _ -> arg_type -- błąd podczas wnioskowania typu argumentu

----------------------------------------------------
                                                   
        EUnit p             -> Right TUnit

        EPair p e1 e2       -> let  t1 = (infer_type env e1)
                                    t2 = (infer_type env e2)
                               in 
                               case t1 of
                                    Right _ -> 
                                        case t2 of
                                            Right _ -> Right (TPair (fromRight t1) (fromRight t2))
                                            Left  _ -> t2
                                    Left  _ -> t1
                                         
        EFst p e            -> let  t = (infer_type env e) in
                               case t of
                                  Right (TPair t1 _) -> Right t1
                                  Left _             -> t
                                  _                  -> Left (Error p "The expression is not a pair.")

        ESnd p e            -> let  t = (infer_type env e) in
                               case t of
                                  Right (TPair _ t2) -> Right t2
                                  Left _             -> t 
                                  _                  -> Left (Error p "The expression is not a pair.")

        ENil p t            -> Right (TList t) 
        ECons p e1 e2       -> let  t1 = (infer_type env e1)
                                    t2 = (infer_type env e2) 
                               in
                               case t1 of
                                  Right _ -> 
                                      case t2 of
                                        Right (TList listtype) -> 
                                            if ((fromRight t1) == listtype) 
                                            then Right (TList (fromRight t1))
                                            else 
                                                 Left (Error p "The head and the tail are not of the same type.")
                                        Left _ -> t2 
                                        _      -> Left (Error p "The second expression is not a list.")
                                  Left  _ -> t1

        EMatchL p e e1 (x,y,e2) 
                            -> let  t1 = (infer_type env e)
                                    t2 = (infer_type env e1)
                               in
                               case t1 of
                                    Right (TList listtype) -> 
                                          let env2 = (env_extend env x listtype) in
                                            if  (t2 == (infer_type (env_extend env2 y (fromRight t1)) e2))
                                            then t2
                                            else Left (Error p "Types of NilClause result and ConsClause result differ.")
                                    Left _ -> t1                                          
                                    _      -> Left (Error p "The first expression is not a list.")

-- Funkcja podająca wynik sprawdzania zgodności typów
typecheck :: Eq p => [FunctionDef p] -> [Var] -> Expr p -> TypeCheckResult p
typecheck funs vars e = let res = (infer_type (env_vars (env_funs funs) vars) e) in
                   case res of
                      Right TInt -> Ok
                      Right _    -> (Error (getData e) "Ultimate type of expression is not Int.")
                      otherwise  -> fromLeft res

---------------------------------------------------------
---------- INTERPRETACJA---------------------------------
---------------------------------------------------------

-- Wartości
data Val p 
    = VNum Integer 
    | VBool Bool 
    | VUnit ()
    | VPair (Val p,Val p)
    | VList [Val p]

    -- FUNKCJA ANONIMOWA
    | VFn   ( Env (Val p)      , (Var, Expr p)      )
    --        środowisko       , funkcja anonimowa (pamiętana jako para (arg, body))

    -- FUNKCJA GLOBALNA
    | VFun  ([FunctionDef p]   , (Var, Var, Expr p) )
    --  ciąg definicji funkcji , funkcja globalna  (pamiętana jako trójka (id, arg, body))
    deriving (Show, Eq)

-- Funkcja tworząca środowisko z definicji funkcji
env_from_funs :: [FunctionDef p] -> Env (Val p)
env_from_funs list = aux (M.empty) list where
                     aux map [] = map
                     aux map ((FunctionDef _ f x _ _ e):xs) =
                         aux (M.insert f (VFun (list,(f,x,e)) ) map) xs

-- Funkcja rozszerzająca środowisko o zmienne wejściowe 
add_vars :: Env (Val p) -> [(Var, Integer)] -> Env (Val p) 
add_vars env vars = aux env vars where
                    aux map [] = map
                    aux map ((v,t):vs) =
                        aux (M.insert v (VNum t) env) vs 

-- Funkcja obliczająca wyrażenia
evaluate :: Env (Val p) -> Expr p -> Either EvalResult (Val p)
evaluate env expr = case expr of
        EVar p x            -> let  val = M.lookup x env in
                               case val of
                                    Just value -> Right value
        
        ENum p n            -> Right (VNum n)
        EBool p b           -> Right (VBool b)
        
        EUnary p UNeg e     -> let VNum v = fromRight (evaluate env e) in 
                               Right (VNum (-v))
        
        EUnary p UNot e     -> let VBool v = fromRight (evaluate env e) in
                               case v of
                                    True  -> Right (VBool False)
                                    False -> Right (VBool True) 
        
        EBinary p op e1 e2  -> 
                if (isArithmOp op || isComparisonOp op) then
                    let VNum v1 = fromRight (evaluate env e1)
                        VNum v2 = fromRight (evaluate env e2)
                    in
                    case op of
                        BAdd -> Right (VNum (v1+v2))
                        BSub -> Right (VNum (v1-v2))     
                        BMul -> Right (VNum (v1*v2))      
                        BDiv -> case v2 of
                                0         -> Left RuntimeError -- dzielenie przez 0
                                otherwise -> Right (VNum (v1 `div` v2))
                        BMod -> case v2 of
                                0         -> Left RuntimeError -- dzielenie przez 0
                                otherwise -> Right (VNum (v1 `mod` v2))
                        BEq  -> Right (VBool (v1 == v2))
                        BNeq -> Right (VBool (v1 /= v2))
                        BLt  -> Right (VBool (v1 <  v2))
                        BGt  -> Right (VBool (v1 >  v2))
                        BLe  -> Right (VBool (v1 <= v2))
                        BGe  -> Right (VBool (v1 >= v2))
                else
                    let VBool v1 = fromRight (evaluate env e1)
                        VBool v2 = fromRight (evaluate env e2)
                    in
                    case op of
                        BAnd -> Right (VBool (v1 && v2))
                        BOr  -> Right (VBool (v1 || v2))
        
        ELet p x e1 e2      -> let v1 = fromRight (evaluate env e1) in 
                               evaluate (env_extend env x v1) e2 -- rozszerzamy środowisko

        EIf p e1 e2 e3      -> let v1 = fromRight (evaluate env e1) in
                               case v1 of
                                  VBool True  -> evaluate env e2
                                  VBool False -> evaluate env e3

----------------------------------------------------

        EFn p v t e         -> Right ( VFn ( env , (v,e) ) )
       
        EApp p e1 e2        -> let  fun = fromRight (evaluate env e1) 
                                    v  =  fromRight (evaluate env e2)
                               in
                               case fun of
                                    VFn  (envprim, (x,eprim))   -> 
                                        evaluate (env_extend envprim x v) eprim

                                    VFun (fundefs, (f,x,eprim)) ->
                                        evaluate (env_extend (env_from_funs fundefs) x v) eprim

----------------------------------------------------

        EUnit p             -> Right (VUnit ())

        EPair p e1 e2       -> let v1 = fromRight (evaluate env e1)
                                   v2 = fromRight (evaluate env e2)
                               in
                               Right (VPair (v1,v2))

        EFst p e            -> let VPair (v1,v2) = fromRight(evaluate env e)
                               in
                               Right v1

        ESnd p e            -> let VPair (v1,v2) = fromRight(evaluate env e)
                               in
                               Right v2

        ENil p t            -> Right (VList []) 
        ECons p e1 e2       -> let v0       = fromRight (evaluate env e1)
                                   VList v1 = fromRight (evaluate env e2)
                               in
                               Right (VList (v0:v1))
                                   
        EMatchL p e e1 (x,y,e2) 
                            -> let list = fromRight (evaluate env e) 
                               in
                               case list of
                                  VList []     -> evaluate env e1
                                  VList (z:zs) -> let env2 = (env_extend env x z) in
                                                  evaluate (env_extend env2 y (VList zs)) e2  
                               

-- Funkcja podająca wynik obliczonej wartości wyrażenia
eval :: [FunctionDef p] -> [(Var,Integer)] -> Expr p -> EvalResult
eval funs vars e = let  res = (evaluate (add_vars (env_from_funs funs) vars) e) in -- inicjujemy środowiska
                   case res of
                      Left RuntimeError  -> RuntimeError
                      Right (VNum n)     -> Value n