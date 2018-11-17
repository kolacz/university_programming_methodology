{-# LANGUAGE Safe #-}
-- Definiujemy moduł zawierający rozwiązanie.
-- Należy zmienić nazwę modułu na {Imie}{Nazwisko}Compiler gdzie
-- za {Imie} i {Nazwisko} należy podstawić odpowiednio swoje imię
-- i nazwisko zaczynające się wielką literą oraz bez znaków diakrytycznych.
module AndrzejKolaczCompiler(compile) where

-- wersja 13pkt - kompilator języka opisanego w treści pracowni 4.

import AST
import MacroAsm
import Data.List

type Env a = (a,[(Var,a)])

-- Funkcja generująca makroinstrukcje w zależności od operatora binarnego.
-- "instr" pełni funkcję stylistyczną. Dla operatorów porównania wykonywane jest odejmowanie
-- pomiędzy argumentami i, w zależności od znaku wyniku, do akumulatora przekazywana jest
-- "prawda" bądź "fałsz" 
mOp :: Label -> Int -> BinaryOperator -> ([MInstr], (Label,Int))
mOp lab h op = let instr = [MConst 0, MJump (lab+1), MLabel lab, MConst (-1), MLabel (lab+1)] in
        case op of 
            BAdd -> ([MAdd],(lab,h-1)) -- (h-1), bo "MAdd" zdejmuje ze stosu
            BSub -> ([MSub],(lab,h-1)) -- itd.
            BMul -> ([MMul],(lab,h-1))
            BDiv -> ([MDiv],(lab,h-1))
            BMod -> ([MMod],(lab,h-1))
            BAnd -> ([MAnd],(lab,h-1))
            BOr  -> ([MOr], (lab,h-1))
            BEq  -> ([MSub, MBranch MC_Z  lab] ++ instr, (lab+2,h-1)) -- lab+2, bo w instr wykorzystaliśmy 2 etykiety
            BNeq -> ([MSub, MBranch MC_NZ lab] ++ instr, (lab+2,h-1)) 
            BLt  -> ([MSub, MBranch MC_N  lab] ++ instr, (lab+2,h-1))
            BGe  -> ([MSub, MBranch MC_NN lab] ++ instr, (lab+2,h-1))
            BLe  -> ([MSub, MBranch MC_NP lab] ++ instr, (lab+2,h-1))
            BGt  -> ([MSub, MBranch MC_P  lab] ++ instr, (lab+2,h-1))

-- Funkcja, która umieszcza odpowiedni warunek "cc" 
-- w makroinstrukcji "MBranch cc l"  w zależności od operatora porównania.
ccond :: BinaryOperator -> MCondition
ccond op = case op of
        BEq  -> MC_EQ
        BNeq -> MC_NE
        BLt  -> MC_LT
        BGe  -> MC_GE
        BLe  -> MC_LE
        BGt  -> MC_GT

-- Funkcja tworząca parę złożoną z: 
--   - liczby k - początkowa wysokość stosu - liczba zmiennych wejściowych
--   - listy par (zmienna, n), gdzie n - pozycja liczona od dołu stosu potrzebna przy 'MGetLocal'
varpos :: [Var] -> Env Int 
varpos vars = (h, zip vars (Data.List.unfoldr (\x -> if x==0 then Nothing else Just(x,x-1)) h)) -- nauka nie poszła w las:)
              where h = length vars

-- Główna funkcja generująca rekurencyjnie listy makroinstrukcji.
-- "lab" - podczas przekazywania do dowolnego wywołania "generate" 
-- jest kolejną etykietą, którą można wykorzystać.
generate :: Label -> Env Int -> Expr p -> ([MInstr], (Label,Int)) -- ([instr], (bieżąca_etykieta, wysokość_stosu))
generate lab (h,vars) (EVar _ v) = case lookup v vars of
                                   Just n -> ([MGetLocal (h-n)], (lab,h)) -- pobieramy z miejsca (wysokość_stosu [minus] pozycja od dołu stosu)

generate lab (h,vars) (EBool _ True)       = ([MConst (-1)], (lab,h)) -- 111...11
generate lab (h,vars) (EBool _ False)      = ([MConst 0]   , (lab,h)) -- 000...00
generate lab (h,vars) (ENum  _ n)          = ([MConst n]   , (lab,h)) 
generate lab (h,vars) (EUnary _ UNot e)    = (instr ++ [MNot], (lab1,h1)) 
                                              where (instr,(lab1,h1)) = generate lab (h,vars) e

generate lab (h,vars) (EUnary _ UNeg e)    = (instr ++ [MNeg], (lab1,h1))
                                              where (instr,(lab1,h1)) = generate lab (h,vars) e

generate lab (h,vars) (EBinary _ op e1 e2) = (instr1 ++ [MPush] ++ instr2 ++ instrOp, (labOp,hOp))
                                              where (instr1, (lab1, h1)) = generate lab (h,vars) e1
                                                    (instr2, (lab2, h2)) = generate lab1 (h1+1,vars) e2 -- (h1+1), bo push
                                                    (instrOp,(labOp,hOp))= mOp lab2 h2 op

generate lab (h,vars) (ELet _ x ex eb) = 
    (instrEx ++ [MPush] ++ instrEb ++ [MPopN 1], (labEb,hEb-1)) -- zdejmujemy x po obliczeniu wartości "eb"
        where (instrEx, (labEx,hEx)) = generate lab (h,vars) ex
              (instrEb, (labEb,hEb)) = generate lab (hEx+1,(x,hEx+1):vars) eb

-- Instrukcja warunkowa z operatorami logicznymi.
-- Dokonane zostały pewne usprawnienia tak, by np. w alternatywie nie sprawdzać drugiego warunku, 
-- jeśli pierwszy ma wartość "true", itp.

generate lab (h,vars) (EIf _ (EUnary _ UNot e) e1 e2) = 
    (instrE ++ [MBranch MC_Z labE] ++ instr2 ++ 
         [MJump lab2, MLabel labE] ++ instr1 ++ [MLabel lab2], (lab1,h1))
    where (instrE, (labE,hE)) = generate lab (h,vars) e
          (instr2, (lab2,h2)) = generate (labE+1) (hE,vars) e2  --(+1), bo branch
          (instr1, (lab1,h1)) = generate (lab2+1) (h2,vars) e1  --(+1), bo jump

generate lab (h,vars) (EIf _ (EBinary _ BAnd a b) e1 e2) =
    (instrA ++ [MBranch MC_Z labA] ++ instrB ++ [MBranch MC_Z labA] ++ instr1 ++ 
         [MJump lab1, MLabel labA] ++ instr2 ++ [MLabel lab1], (lab2,h2))
    where (instrA, (labA,hA)) = generate lab (h,vars) a
          (instrB, (labB,hB)) = generate (labA+1) (hA,vars) b -- (+1), gdyż branch
          (instr1, (lab1,h1)) = generate labB (hB,vars) e1    -- nie dodajemy 1, bo użyliśmy tej samej etykiety
          (instr2, (lab2,h2)) = generate (lab1+1) (h1,vars) e2

generate lab (h,vars) (EIf _ (EBinary _ BOr a b) e1 e2) = 
    (instrA ++ [MBranch MC_NZ labA] ++ instrB ++ [MBranch MC_NZ labA] ++ instr2 ++
          [MJump lab2, MLabel labA] ++ instr1 ++ [MLabel lab2], (lab1,h1))
    where (instrA, (labA,hA)) = generate lab (h,vars) a
          (instrB, (labB,hB)) = generate (labA+1) (hA,vars) b
          (instr2, (lab2,h2)) = generate labB (hB,vars) e2
          (instr1, (lab1,h1)) = generate (lab2+1) (h2,vars) e1

-- Instrukcja warunkowa z operatorami porównania
generate lab (h,vars) (EIf _ (EBinary _ op x y) e1 e2) =
    ( instrX ++ [MPush] ++ instrY ++ [MBranch cc labY] ++ instr2 ++ 
        [MJump lab2, MLabel labY] ++ instr1 ++ [MLabel lab2], (lab1,h1))
        where cc = ccond op  -- dla każdego z tych warunków "MBranch cc l" ściąga wartość ze stosu
              (instrX, (labX,hX)) = generate lab (h,vars) x
              (instrY, (labY,hY)) = generate labX (hX+1,vars) y -- push
              (instr2, (lab2,h2)) = generate (labY+1) (hY-1,vars) e2 -- branch
              (instr1, (lab1,h1)) = generate (lab2+1) (h2,vars) e1

-- Funkcja kompilująca program
-- Dla pracowni nr 4 należy zignorować pierwszy argument
-- UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jej definicje
compile :: [FunctionDef p] -> [Var] -> Expr p -> [MInstr]
compile fundefs vars expr = fst (generate 0 (varpos vars) expr) ++ [MRet]