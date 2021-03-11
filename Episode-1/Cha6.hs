{-# OPTIONS_GHC -Wall #-}
{--
    LCPF : Projet
    Une princesse ou un tigre ?
    by Quentin Carpentier & Paul-Joseph Krogulec.
-}

module Cha6 (challenge6) where
import CPL

{- Porte 1 disant : "Il y a un tigre ici." -}
door1 :: Formula
door1 = (Var "t1")

{- Porte 2 disant : "Cette cellule contient une princesse." -}
door2 :: Formula
door2 = (Var "p2")

{- Porte 2 disant : "Il y a un tigre dans la cellule 2" -}
door3 :: Formula
door3 = (Var "t2")

{- Formule qui décrit le fait qu'il ne peut pas y avoir un tigre et une princesse (en même temps) dans chaque cellule. -}
constraint :: Formula
constraint = And (And (Eqv (Var "p1") (Not (Var "t1"))) (Imp (Var "p1") (And (Var "t2") (Var "t3")))) (And (And (Eqv (Var "p2") (Not (Var "t2"))) (Imp (Var "p2") (And (Var "t1") (Var "t3")))) (And (Eqv (Var "p3") (Not (Var "t3"))) (Imp (Var "p3") (And (Var "t1") (Var "t2")))))

{- Formule pour exprimer le fait que, dans les deux portes disent toutes les deux la vérité, ou mentent. -}
reglement :: Formula
reglement = And (Imp (door1) (And (Not door2) (Not door3))) (And (Imp (door2) (And (Not door1) (Not door3))) (Imp (door3) (And (Not door1) (Not door2))))

{- Fonction qui fait la conjonction de toutes les formules de la sixième épreuve. -}
challenge6 :: Formula
challenge6 = And (constraint) (reglement)