-- Wymagamy, by moduł zawierał tylko bezpieczne funkcje
{-# LANGUAGE Safe #-}
-- Definiujemy moduł zawierający testy.
-- Należy zmienić nazwę modułu na {Imie}{Nazwisko}Tests gdzie za {Imie}
-- i {Nazwisko} należy podstawić odpowiednio swoje imię i nazwisko
-- zaczynające się wielką literą oraz bez znaków diakrytycznych.
module AndrzejKolaczTests(tests) where

-- Importujemy moduł zawierający typy danych potrzebne w zadaniu
import DataTypes

-- Lista testów do zadania
-- Należy uzupełnić jej definicję swoimi testami
tests :: [Test]
tests =
  [ Test "inc"               (SrcString "input x in x + 1") (Eval [42] (Value 43))
  , Test "undefVar"          (SrcString "x")                TypeError
  , Test "wrongArgType"      (SrcFile "wrong_arg_type.pp6") TypeError
  , Test "funSub17"          (SrcFile "fun_sub17.pp6") (Eval [10] (Value (-7)))
  , Test "composition"       (SrcFile "composition.pp6") (Eval [4] (Value 16))
  , Test "wrongComposition"  (SrcFile "wrong_composition.pp6") TypeError
  , Test "fn"                (SrcFile "fn.pp6") (Eval [100] (Value 101))
  , Test "fun_bool"          (SrcFile "fun_bool.pp6") (Eval [1] (Value 2))
  ]