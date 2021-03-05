{-# OPTIONS_GHC -Wall #-}
{--
    LCPF : Projet
    Une princesse ou un tigre ?
    by Quentin Carpentier & Paul-Joseph Krogulec.
-}

module Cha3 (challenge3) where
import CPL

{- Porte 1 disant : "Il y a un tigre dans cette cellule ou il y a une princesse dans l'autre." -}
door1 :: Formula
door1 = Or (Var "t1") (Var "p2")

{- Porte 2 disant : "Il y a une princesse dans l'autre cellule." -}
door2 :: Formula
door2 = (Var "p1")

{- Formule qui décrit le fait qu'il ne peut pas y avoir un tigre et une princesse (en même temps) dans chaque cellule. -}
constraint :: Formula
constraint = And (Eqv (Var "p1") (Not (Var "t1"))) (Eqv (Var "p2") (Not (Var "t2")))

{- Formule pour exprimer le fait que : dans les deux portes disent toutes les deux la vérité, ou mentent. -}
reglement :: Formula
reglement = Or (And door1 door2) (And (Not door1) (Not door2))

{- Fonction qui fait la conjonction de toutes les formules de la troisième épreuve. -}
challenge3 :: Formula
challenge3 = And (constraint) (reglement)