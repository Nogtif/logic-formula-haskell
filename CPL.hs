{-# OPTIONS_GHC -Wall #-}
{--
    LCPF : Projet
    Une princesse ou un tigre ?
    by Quentin Carpentier & Paul-Joseph Krogulec.
-}

module CPL (
    Formula (..),
    World,
    genAllWorlds, testGenWorlds,
    sat, testSat,
    findWorlds, testFindWorlds,
    testAll
) where
 
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

testGenWorlds :: [Bool] -- Fonction de test
testGenWorlds = [
    genAllWorlds ["p1", "p2", "t1", "t2"] == [
        ["p1"], ["p1", "p2"], ["p1","p2","t1"], ["p1","p2","t1","t2"],["p1","p2","t2"],
        ["p1","t1"],["p1","t1","t2"],["p1","t2"], ["p2"],["p2","t1"], ["p2","t1","t2"], ["p2","t2"], ["t1"], ["t1","t2"], ["t2"]] 
    ]


{- Fonction qui, pour un monde possible w et une formule phi passés en arguments, vérifie si w satisfait phi. -}
sat :: World -> Formula -> Bool
sat _ T = True
sat _ F = False
sat w (Var s) = s `elem` w
sat w (Not phi) = not (sat w phi)
sat w (And phi psi) = (sat w phi) && (sat w psi)
sat w (Or phi psi)  = (sat w phi) || (sat w psi)
sat w (Imp phi psi) = (not (sat w phi)) || (sat w psi)
sat w (Eqv phi psi) = (sat w (Imp phi psi)) && (sat w (Imp psi phi))

testSat :: [Bool] -- Fonction de test
testSat = [
    sat ["p1", "t2"] T == True,
    sat ["p1", "p2", "t2"] (And (Var "p1") (Var "p2")) == True,
    sat ["p1", "p2"] (And (Var "p1") (Var "p2")) == True,
    sat ["p1", "t2"] (And (Eqv (Var "p1") (Not (Var "t1"))) (Eqv (Var "p2") (Not (Var "t2")))) == True ]

{- Fonction qui qui, pour une formule phi renvoie la liste de tous mondes possibles qui satisfont phi. -}
extractVars :: Formula -> [[Char]]
extractVars T = []
extractVars F = []
extractVars (Var v) = [v]
extractVars (Not f) = extractVars f
extractVars (And f1 f2) = extractVars f1 ++ extractVars f2
extractVars (Or f1 f2) = extractVars f1 ++ extractVars f2
extractVars (Imp f1 f2) = extractVars f1 ++ extractVars f2
extractVars (Eqv f1 f2) = extractVars f1 ++ extractVars f2

supDoublons :: [[Char]] -> [[Char]]
supDoublons [] = []
supDoublons (x:xs)   
    | x `elem` xs = supDoublons xs
    | otherwise = x : supDoublons xs

findFromWorlds :: [World] -> Formula -> [World]
findFromWorlds [] _ = []
findFromWorlds (x:xs) f
    | (sat x f) == True = x : findFromWorlds xs f
    | otherwise = (findFromWorlds xs f)

findWorlds :: Formula -> [World]
findWorlds f = (findFromWorlds (genAllWorlds (supDoublons(extractVars f))) f)

testFindWorlds :: [Bool] -- Fonction de test
testFindWorlds = [
    findWorlds (And (Var "p1") (Var "t2")) == [["p1","t2"]] ]


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
    | test(testGenWorlds) && test(testSat) && test(testFindWorlds) = "Success"
    | otherwise = "Fail!"
