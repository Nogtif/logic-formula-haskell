{-# OPTIONS_GHC -Wall #-}
{--
    LCPF : Projet
    Les enfants bouées
    by Quentin Carpentier & Paul-Joseph Krogulec.
-}

module Problem1 where
import EL

{- Fonction qui prend une proposition enargument 
et renvoie la liste de mondes possibles où la proposition est vraie. -}
interp :: Prop -> [World]
interp "as" = [10, 11]  -- Alice est sale dans les mondes 10 et 11.
interp "bs" = [01, 11]  -- Bob est sale dans les mondes 01 et 11.
interp _    = []        -- Toutes les autres propositions sont fausses.

{- Fonction qui prend un agent i et un monde possible w en arguments,
et renvoie la liste de mondes possibles qui sont indiscernables du monde w pour l'agent i. -}
indis :: Agent -> World -> [World]
-- Monde indiscernable d'Alice :
indis "a" 00 = [00, 10]
indis "a" 01 = [01, 11]
indis "a" 10 = [10, 00]
indis "a" 11 = [11, 01]
-- Monde indiscernable de Bob :
indis "b" 00 = [00, 01]
indis "b" 01 = [01, 00]
indis "b" 10 = [10, 11]
indis "b" 11 = [11, 10]
-- Autres...
indis _ _ = []

{- Définition complète de l'état épistémique initial du problème. -}
s0 :: EpiState
s0 = (interp, indis, 01)

{- Exprime l'annonce du père, c.-à-d., « Au moins un des deux a le visage sale. » -}
fatherAnn :: EpiFormula
fatherAnn = Or (Var "as") (Var "bs")

{- Exprime l'ignorance d'Alice sur son état, « Alice ne sait pas que le visage d'Alice est sale, 
et Alice ne sait pas que le visage d'Alice n’est pas sale. » -}
aliceIgn :: EpiFormula
aliceIgn = (
    And 
        (Not (Knows "a" (Var "as"))) 
        (Not (Knows "a" (Not (Var "as"))))
    )

{- Exprime l'ignorance de Bob sur son état, « Bob ne sait pas que le visage de Bob est sale, 
et Bob ne sait pas que le visage de Bob n’est pas sale. » -}
bobIgn :: EpiFormula
bobIgn = (
    And 
        (Not (Knows "b" (Var "bs"))) 
        (Not (Knows "b" (Not (Var "bs"))))
    )

{- Exprime le problème 1 dans sa totalité. -}
problem1 :: EpiFormula
problem1 = (
    And 
        (And aliceIgn bobIgn) 
        (After fatherAnn (And 
                            (And aliceIgn (Not bobIgn))
                            (After (Not bobIgn) (Not aliceIgn)) 
                        ) 
        )
    )