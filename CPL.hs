{-# OPTIONS_GHC -Wall #-}

{----- 
    LCPF : Projet
    Une princesse ou un tigre ?
    by Quentin Carpentier & Paul-Joseph Krogulec.
-----}

module CPL where

{- Type structuré Formula pour représenter des formules booléennes (c.-à-d., des formules logiques).-}
data Formula = T | F | Var [Char]
    | Not   (Formula)
    | And   (Formula) (Formula)
    | Or    (Formula) (Formula)
    | Imp   (Formula) (Formula)
    | Eqv   (Formula) (Formula)
    deriving (Eq)

instance Show Formula where
    show T = "Vrai"
    show F = "Faux"
    show (Var s) = s
    show (Not f) = "~" ++ show f
    show (And f1 f2) = "(" ++ show f1 ++ " ∧ " ++ show f2 ++ ")"
    show (Or f1 f2) = "(" ++ show f1 ++ " ∨ " ++ show f2 ++ ")"
    show (Imp f1 f2) = "(" ++ show f1 ++ " ⇒ " ++ show f2 ++ ")"
    show (Eqv f1 f2) = "(" ++ show f1 ++ " ⇔ " ++ show f2 ++ ")"

door1 :: Formula
door1 = And (Var "p1") (Var "t2")

door2 :: Formula
door2 = Or (And (Var "p1") (Var "t2")) (And (Var "t1") (Var "p2"))

{- Formule qui décrit le fait qu’il ne peut pas y avoir un tigre et une princesse (en même temps) dans chaque cellule. -}
constraint :: Formula
constraint = And (Eqv (Var "p1") (Not (Var "t1"))) (Eqv (Var "p2") (Not (Var "t2")))

{- Formule pour exprimer le fait que, dans la première épreuve au moins, l'une des portes dit la vérité et l'autre ment. -}
reglement :: Formula
reglement = Or (And (Not door1) door2) (And door1 (Not door2))

{- Fonction qui fait la conjonction de toutes les formules de la première épreuve. -}
challenge1 :: Formula
challenge1 = And (constraint) (reglement)

{- Type World, pour représenter le type des mondes possibles. -}
type World = [[Char]]

isExist :: World -> [World] -> Bool
isExist n [] = False
isExist n (x:xs)
    | n == x = True
    | otherwise = isExist n xs

{- Fonction qui, pour une liste de noms de variables propositionnels (tel que ["p1", "p2", "t1", "t2"]), 
génère la liste de tous les mondes possibles pour ces variables. -}
genAllWorlds :: [[Char]] -> [World]
genAllWorlds _ = [["p1", "p2", "t1", "t2"]]
