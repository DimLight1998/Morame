module Test where

import AST
import qualified EvalType as T
import qualified EvalValue as V

adtMaybeInt = ADT "Maybe" [("Just", [TInt]), ("Nothing", [])]
adtEitherBoolIntOrMaybeIntx3 =  ADT "Either" [("Left", [TBool, TInt]), ("Right", [TData "Maybe", TData "Maybe", TData "Maybe"])]

adts = [adtMaybeInt, adtEitherBoolIntOrMaybeIntx3]

nothing = EVar "Nothing"
just1 = EApply (EVar "Just") (EIntLit 1)
just2 = EApply (EVar "Just") (EIntLit 2)
either = EApply (EApply (EApply (EVar "Right") just1) nothing) just2
wrongEither = EApply (EApply (EApply (EVar "Right") just1) (EIntLit 3)) just2

res1 = ECase just1 [(PData "Nothing" [], EBoolLit False), (PData "Just" [PVar "x"], EEq (EVar "x") (EIntLit 1))]
res2 = ECase just1 [(PData "Nothing" [], EBoolLit False)]
res3 = ECase just1 [(PVar "x", EBoolLit False), (PData "Nothing" [], EBoolLit False), (PData "Just" [PVar "x"], EEq (EVar "x") (EIntLit 1))]