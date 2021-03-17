{-# OPTIONS_GHC -Wall #-}
{--
    LCPF : Projet
    Une princesse ou un tigre ?
    by Quentin Carpentier & Paul-Joseph Krogulec.
-}

module Cha2 (challenge2) where
import CPL

{- Porte 1 disant : "Une au moins de deux cellules contient une princesse." -}
door1 :: Formula
door1 = Or 
        (Var "p1") 
        (Or 
            (Var "p2") 
            (And (Var "p1") (Var "p2"))
        )

{- Porte 2 disant : "Il y a un tigre dans l'autre cellule." -}
door2 :: Formula
door2 = (Var "t1")

{- Formule qui décrit le fait qu'il ne peut pas y avoir un tigre et une princesse (en même temps) dans chaque cellule. -}
constraint :: Formula
constraint = And 
            (Eqv (Var "p1") (Not (Var "t1"))) 
            (Eqv (Var "p2") (Not (Var "t2")))

{- Formule pour exprimer le fait que : dans les deux portes disent toutes les deux la vérité, ou mentent. -}
reglement :: Formula
reglement = Or 
            (And door1 door2) 
            (And (Not door1) (Not door2))

{- Fonction qui fait la conjonction de toutes les formules de la seconde épreuve. -}
challenge2 :: Formula
challenge2 = And (constraint) (reglement)