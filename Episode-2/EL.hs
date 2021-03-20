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
    update, testUpdate,
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

{- Fonction interp pour permettre les test des fonctions testEpiSat & testUpdate. -}
testInterp :: Prop -> [World] -- Interp de test du problème 1
testInterp "as" = [10, 11]
testInterp "bs" = [01, 11]
testInterp _    = []

{- Fonction indis pour permettre les test des fonctions testEpiSat & testUpdate. -}
testIndis :: Agent -> World -> [World]  -- Indis de test du problème 1
testIndis "a" 00 = [00, 10]
testIndis "a" 01 = [01, 11]
testIndis "a" 10 = [10, 00]
testIndis "a" 11 = [11, 01]
testIndis "b" 00 = [00, 01]
testIndis "b" 01 = [01, 00]
testIndis "b" 10 = [10, 11]
testIndis "b" 11 = [11, 10]
testIndis _ _ = []

{- Fonction qui prend un état épistémique s et une formule phi en arguments,
et renvoie True si s satisfait phi, et False sinon -}
epiSat :: EpiState -> EpiFormula -> Bool
epiSat _ T = True
epiSat _ F = False
epiSat (interp, _, w) (Var p) = w `elem` (interp p)
epiSat s (Not phi) = not (epiSat s phi)
epiSat s (And phi psi) = (epiSat s phi) && (epiSat s psi)
epiSat s (Or phi psi)  = (epiSat s phi) || (epiSat s psi)
epiSat s (Imp phi psi) = (not (epiSat s phi)) || (epiSat s psi)
epiSat s (Eqv phi psi) = (epiSat s (Imp phi psi)) && (epiSat s (Imp psi phi))
epiSat (interp, indis, w) (Knows a phi) = all (\x->(epiSat (interp, indis, x) phi)) (indis a w)
epiSat (interp, indis, w) (After phi psi) = (epiSat (interp, indis, w) phi) && (epiSat(update(interp, indis, w) phi) psi)

testEpiSat :: [Bool] -- Fonction de test
testEpiSat = [
        epiSat (testInterp, testIndis, 00) T == True,
        epiSat (testInterp, testIndis, 10) F == False,
        epiSat (testInterp, testIndis, 11) (After (Not (Knows "a" (Var "as"))) (Not (Knows "b" (Var "bs")))) == True
    ]

{- Fonction qui prend un état épistémique s et une formule phi ,
et renvoie un nouvel état épistémique correspondant à lamise à jour de s par phi. -}
update  ::  EpiState -> EpiFormula -> EpiState
update (interp, indis, w) phi =
    let interp2 p = filter (\x-> (epiSat (interp, indis, x) phi)) (interp p)
        indis2 a w2 = filter(\x-> (epiSat (interp, indis, x) phi)) (indis a w2)
    in (interp2, indis2, w)

testUpdate :: [Bool] -- Fonction de test
testUpdate = [
        epiSat (update (testInterp, testIndis, 00) F) T == True
    ]

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
    | test(testEpiSat) && test(testUpdate) = "Success"
    | otherwise = "Fail!"