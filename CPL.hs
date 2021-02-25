{-# OPTIONS_GHC -Wall #-}

{----- 
    LCPF : Projet
    Une princesse ou un tigre ?
    by Quentin Carpentier & Paul-Joseph Krogulec.
-----}

module CPL where

{- Type structuré Formula pour représenter des formules booléennes (c.-à-d., des formules logiques).-}
data Formula a = T | F | Var [Char]
    | Not   (Formula a)
    | And   (Formula a) (Formula a)
    | Or    (Formula a) (Formula a)
    | Imp   (Formula a) (Formula a)
    | Eqv   (Formula a) (Formula a)
    deriving (Eq)

instance Show (Formula a) where
    show T = "Vrai"
    show F = "Faux"
    show (Var v) = v
    show (Not f) = "~" ++ show f
    show (And f1 f2) = "(" ++ show f1 ++ " ∧ " ++ show f2 ++ ")"
    show (Or f1 f2) = "(" ++ show f1 ++ " ∨ " ++ show f2 ++ ")"
    show (Imp f1 f2) = "(" ++ show f1 ++ " ⇒ " ++ show f2 ++ ")"
    show (Eqv f1 f2) = "(" ++ show f1 ++ " ⇔ " ++ show f2 ++ ")"


constraint :: Formula a
constraint = Or (And (Not (Var "p1")) (Var "t1")) (Or (Var "p2") (Not (Var "t2")))




{- Type World, pour représenter le type des mondes possibles. -}
type World = [[Char]]

{- Fonction qui, pour une liste de noms de variables propositionnels (tel que ["p1", "p2", "t1", "t2"]), 
génère la liste de tous les mondes possibles pour ces variables. -}
genAllWorlds :: [[Char]] -> [World]
genAllWorlds _ = [["p1", "p2", "t1", "t2"]]
