{-# OPTIONS_GHC -Wall #-}
{--
    LCPF : Projet
    Les enfants bouées
    by Quentin Carpentier & Paul-Joseph Krogulec.
-}

module Problem4 where
import EL

{- Fonction qui prend une proposition en argument,0
et renvoie la liste de mondes possibles où la proposition est vraie. -}
interp :: Prop -> [World]
-- Mondes possibles d'Anne :
interp "a0" = [01]
interp "a1" = [10, 12]
interp "a2" = [21, 23]
interp "a3" = [32, 34]
interp "a4" = [43]
-- Mondes possibles de Bill :
interp "b0" = [10]
interp "b1" = [01, 21]
interp "b2" = [12, 32]
interp "b3" = [23, 43]
interp "b4" = [34]
interp _    = []

{- Fonction qui prend un agent i et un monde possible w en arguments,
et renvoie la liste de mondes possibles qui sont indiscernables du monde w pour l'agent i. -}
indis :: Agent -> World -> [World]
-- Mondes indiscernables d'Anne :
indis "a" 01 = [01]
indis "a" 10 = [10, 12]
indis "a" 12 = [12, 10]
indis "a" 21 = [21, 23]
indis "a" 23 = [23, 21]
indis "a" 32 = [32, 34]
indis "a" 34 = [34, 32]
indis "a" 43 = [43]
-- Mondes indiscernables de Bill :
indis "b" 01 = [01, 21]
indis "b" 10 = [10]
indis "b" 12 = [12, 32]
indis "b" 21 = [21, 01]
indis "b" 23 = [23, 43]
indis "b" 32 = [32, 12]
indis "b" 34 = [34]
indis "b" 43 = [43, 23]
-- Autres...
indis _ _ = []

{- Définition complète de l'état épistémique initial du problème avec le monde réel 12. -}
s0 :: EpiState
s0 = (interp, indis, 12)

{- Définition complète de l'état épistémique initial du problème avec le monde réel 32. -}
s1 :: EpiState
s1 = (interp, indis, 32)

{- Exprime l'ignorance d'Anne, « Anne ne connaît pas le nombre de Bill. » -}
anneIgn :: EpiFormula
anneIgn = (
    Or 
        (Or 
            (And 
                (Not (Knows "a" (Var "b0"))) 
                (Not (Knows "a" (Not (Var "b0"))))
            ) 
            (And 
                (Not (Knows "a" (Var "b1"))) 
                (Not (Knows "a" (Not (Var "b1"))))
            )
        )
        (Or 
            (And 
                (Not (Knows "a" (Var "b2"))) 
                (Not (Knows "a" (Not (Var "b2"))))
            ) 
            (Or 
                (And 
                    (Not (Knows "a" (Var "b3"))) 
                    (Not (Knows "a" (Not (Var "b3"))))
                )
                (And 
                    (Not (Knows "a" (Var "b4"))) 
                    (Not (Knows "a" (Not (Var "b4"))))

                )
            )
        )
    )

{- Exprime l'ignorance de Bill, « Bill ne sait pas que le visage de Bill est sale, 
et Bill ne sait pas que le visage de Bill n’est pas sale. » -}
billIgnorance :: EpiFormula
billIgnorance = (
    Or  
        (Or 
            (And 
                (Not (Knows "b" (Var "a0"))) 
                (Not (Knows "b" (Not (Var "a0"))))
            ) 
            (And 
                (Not (Knows "b" (Var "a1"))) 
                (Not (Knows "b" (Not (Var "a1"))))
            )
        )
        (Or 
            (And 
                (Not (Knows "b" (Var "a2"))) 
                (Not (Knows "b" (Not (Var "a2"))))
            ) 
            (Or 
                (And 
                    (Not (Knows "a" (Var "a3"))) 
                    (Not (Knows "a" (Not (Var "a3"))))
                )
                (And 
                    (Not (Knows "b" (Var "a4"))) 
                    (Not (Knows "b" (Not (Var "a4"))))

                )
            )
        )
    )

{- Exprime le problème 4 dans sa totalité :
« Anne ne connaît pas le nombre de Bill, et Bill ne connaît pas le nombre de Anne, 
et Après que Anne annonce son ignorance, et après que Bill annonce son ignorance, 
Anne connaît le nombre de Bill. » -}
problem4 :: EpiFormula
problem4 = (
    And 
        (And anneIgn billIgnorance) 
        (After 
            anneIgn 
            (After billIgnorance (Not anneIgn))
        )
    )
          