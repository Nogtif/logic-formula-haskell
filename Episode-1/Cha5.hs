{-# OPTIONS_GHC -Wall #-}
{--
    LCPF : Projet
    Une princesse ou un tigre ?
    by Quentin Carpentier & Paul-Joseph Krogulec.
-}

module Cha5 (challenge5) where
import CPL

{- Porte 1 disant : "Choisis bien ta cellule¸ça a de l'importance !" -}
door1 :: Formula
door1 = Or 
        (And (Var "p1") (Var "t2")) 
        (And (Var "t1") (Var "p2"))

{- Porte 2 disant : "Tu ferais mieux de choisir l'autre cellule !" -}
door2 :: Formula
door2 = (Var "p1")

{- Formule qui décrit le fait qu'il ne peut pas y avoir un tigre et une princesse (en même temps) dans chaque cellule. -}
constraint :: Formula
constraint = (
    And 
        (Eqv 
            (Var "p1") (Not (Var "t1"))
        ) 
        (Eqv 
            (Var "p2") (Not (Var "t2"))
        )
    )

{- Formule pour exprimer le fait que :
La cellule 1 dit la vérité quand il y a une princesse dans cette cellule et ment quand c'est un tigre. Pour la cellule 2 c'est exactement le contraire. -}
reglement :: Formula
reglement = (
    And 
        (And 
            (Imp (Var "p1") door1) 
            (Imp (Var "t1") (Not door1))
        ) 
        (And 
            (Imp (Var "t2") door2) 
            (Imp (Var "p2") (Not door2))
        )
    )

{- Fonction qui fait la conjonction de toutes les formules de la cinquième épreuve. -}
challenge5 :: Formula
challenge5 = And (constraint) (reglement)