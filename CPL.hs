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

{- Type World, pour représenter le type des mondes possibles. -}
type World = [[Char]]

{- Fonction qui, pour une liste de noms de variables propositionnels (tel que ["p1", "p2", "t1", "t2"]), 
génère la liste de tous les mondes possibles pour ces variables. -}
genAllWorlds :: [[Char]] -> [World]
genAllWorlds [] = []
genAllWorlds (x:xs) = [x] : map (x :) (genAllWorlds xs) ++ genAllWorlds xs

isExist :: [Char] -> World -> Bool
isExist _ [] = False
isExist n (x:xs)
    | n == x = True
    | otherwise = isExist n xs

sat :: World -> Formula -> Bool
sat _ T = True
sat _ F = True
sat w (Var a) 
    | (isExist a w) = True 
    | otherwise = False

