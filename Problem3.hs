{-# OPTIONS_GHC -Wall #-}
{--
    LCPF : Projet
    Les enfants bouées
    by Quentin Carpentier & Paul-Joseph Krogulec.
-}

module Problem3 where
import EL

{- Fonction qui prend une proposition en argument,
et renvoie la liste de mondes possibles où la proposition est vraie. -}
interp :: Prop -> [World]
interp "as" = [100, 101, 110, 111]
interp "bs" = [010, 011, 110, 111]
interp "cs" = [001, 011, 101, 111]
interp _    = []

{- Fonction qui prend un agent i et un monde possible w en arguments,
et renvoie la liste de mondes possibles qui sont indiscernables du monde w pour l'agent i. -}
indis :: Agent -> World -> [World]
-- Mondes indiscernables d'Alice :
indis "a" 000 = [000, 100]
indis "a" 001 = [001, 101]
indis "a" 010 = [010, 110]
indis "a" 011 = [011, 111]
indis "a" 100 = [100, 000]
indis "a" 101 = [101, 001]
indis "a" 110 = [110, 010]
indis "a" 111 = [111, 011]
-- Mondes indiscernables de Bob :
indis "b" 000 = [000, 010]
indis "b" 001 = [001, 011]
indis "b" 010 = [010, 000]
indis "b" 011 = [011, 001]
indis "b" 100 = [100, 110]
indis "b" 101 = [101, 111]
indis "b" 110 = [110, 100]
indis "b" 111 = [111, 101]
-- Mondes indiscernables de Caroline :
indis "c" 000 = [000, 001]
indis "c" 001 = [001, 000]
indis "c" 010 = [010, 011]
indis "c" 011 = [011, 010]
indis "c" 100 = [100, 101]
indis "c" 101 = [101, 100]
indis "c" 110 = [110, 111]
indis "c" 111 = [111, 110]
-- Autres...
indis _ _ = []

{- Définition complète de l'état épistémique initial du problème. -}
s0 :: EpiState
s0 = (interp, indis, 111)

{- Exprime l'annonce du père, c.-à-d., « Au moins un des trois a le visage sale. » -}
fatherAnn :: EpiFormula
fatherAnn = Or (Var "as") (Or (Var "bs") (Var "cs"))

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

{- Exprime l'ignorance de Caroline sur son état, « Caroline ne sait pas que le visage de Caroline est sale, 
et Caroline ne sait pas que le visage de Caroline n’est pas sale. » -}
carolineIgn :: EpiFormula
carolineIgn = (And 
        (Not (Knows "c" (Var "cs"))) 
        (Not (Knows "c" (Not (Var "cs")))))

{- Exprime le problème 3 dans sa totalité. -}
problem3 :: EpiFormula
problem3 = (
    And 
        (And aliceIgn (And bobIgn carolineIgn)) 
        (After fatherAnn (After
                            (And aliceIgn (And bobIgn carolineIgn)) 
                                (After 
                                        (And aliceIgn (And bobIgn carolineIgn)) 
                                        (And 
                                            (Not aliceIgn) 
                                            (And (Not bobIgn) (Not carolineIgn))
                                        )
                                )
                        )
        )
    )