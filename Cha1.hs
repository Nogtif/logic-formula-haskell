{-# OPTIONS_GHC -Wall #-}
{--
    LCPF : Projet
    Une princesse ou un tigre ?
    by Quentin Carpentier & Paul-Joseph Krogulec.
-}

module Cha1 (challenge1) where
import CPL

{- Porte 1 disant : "Il y a une princesse dans cette cellule et un tigre dans l'autre." -}
door1 :: Formula
door1 = And (Var "p1") (Var "t2")

{- Porte 2 disant : "Il y a une princesse dans une cellule et il y a un tigre dans une cellule." -}
door2 :: Formula
door2 = Or (And (Var "p1") (Var "t2")) (And (Var "t1") (Var "p2"))

{- Formule qui décrit le fait qu'il ne peut pas y avoir un tigre et une princesse (en même temps) dans chaque cellule. -}
constraint :: Formula
constraint = And (Eqv (Var "p1") (Not (Var "t1"))) (Eqv (Var "p2") (Not (Var "t2")))

{- Formule pour exprimer le fait que, dans la première épreuve au moins, l'une des portes dit la vérité et l'autre ment. -}
reglement :: Formula
reglement = Or (And (Not door1) door2) (And door1 (Not door2))

{- Fonction qui fait la conjonction de toutes les formules de la première épreuve. -}
challenge1 :: Formula
challenge1 = And (constraint) (reglement)