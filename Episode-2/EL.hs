{-# OPTIONS_GHC -Wall #-}
{--
    LCPF : Projet
    Les enfants bouées
    by Quentin Carpentier & Paul-Joseph Krogulec.
-}

module EL (
    Prop,
    Agent,
    World,
    EpiState,
    EpiFormula (..),
) where

-- Représente une proposition.
type Prop = [Char]

-- Représente un agent.
type Agent = [Char]

-- Représente un monde possible.
type World = Int

-- Type de tuple représentant des états épistémiques.
type EpiState = (Prop -> [World], Agent -> World -> [World], World)

{- Type structuré EpiFormula pour représenter des formules logique épis-témique.
(c.-à-d., des formules logiques, plus les deux opérateurs Knows et After). -}
data EpiFormula = T | F | Var [Char]
    | Not   (EpiFormula)
    | And   (EpiFormula) (EpiFormula)
    | Or    (EpiFormula) (EpiFormula)
    | Imp   (EpiFormula) (EpiFormula)
    | Eqv   (EpiFormula) (EpiFormula)
    | Knows [Char] (EpiFormula)
    | After (EpiFormula) (EpiFormula)
    deriving (Eq)