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
    epiSat, testEpiSat,
    testAll
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
    deriving (Show, Eq)

{- Fonction qui prend un état épistémiques et une formule phi en arguments,
et renvoie True si a satisfait phi, et False sinon -}
epiSat :: EpiState -> EpiFormula -> Bool
epiSat _ T = True
epiSat _ F = False
epiSat (interp, _, w) (Var p) = w `elem` (interp p)
epiSat s (Not phi) = not (epiSat s phi)
epiSat s (And phi psi) = (epiSat s phi) && (epiSat s psi)
epiSat s (Or phi psi)  = (epiSat s phi) || (epiSat s psi)
epiSat s (Imp phi psi) = (not (epiSat s phi)) || (epiSat s psi)
epiSat s (Eqv phi psi) = (epiSat s (Imp phi psi)) && (epiSat s (Imp psi phi))
epiSat (interp, indis, w) (Knows a phi) = (epiSat (interp, indis, w) phi) && null (indis a w)

testEpiSat :: [Bool]
testEpiSat = [True]

{- Fonction qui reçoit les résultats d’un test et qui retourne vrai si tous les résultats du test sont vrai et faux sinon. -}
test :: [Bool] -> Bool
test(tab)
    | head tab == last tab && head tab == True = True
    | head tab == False = False
    | otherwise = test (tail tab)

{- Fonction qui retourne la chaine de caractères "Success!" 
si tous les résultats des tests de toutes les fonctions sont vrais, sinon "Fail!". -}
testAll :: [Char]
testAll 
    | test(testEpiSat) = "Success"
    | otherwise = "Fail!"