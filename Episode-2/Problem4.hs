{-# OPTIONS_GHC -Wall #-}
{--
    LCPF : Projet
    Les enfants bouées
    by Quentin Carpentier & Paul-Joseph Krogulec.
-}

module Problem4 where
import EL

{- Fonction qui prend une proposition en argument,
et renvoie la liste de mondes possibles où la proposition est vraie. -}
interp :: Prop -> [World]
interp "an" = [10, 21, 32, 43]
interp "bn" = [01, 12, 23, 34]
interp _    = []

{- Fonction qui prend un agent i et un monde possible w en arguments,
et renvoie la liste de mondes possibles qui sont indiscernables du monde w pour l'agent i. -}
indis :: Agent -> World -> [World]
indis "a" 10 = [10, 12]
indis "a" 21 = [21, 23]
indis "a" 32 = [32, 34]
indis "a" 43 = [43]

indis "b" 01 = [01, 21]
indis "b" 12 = [12, 32]
indis "b" 23 = [23, 43]
indis "b" 34 = [34]
indis _ _ = []

{- Définition complète de l'état épistémique initial du problème. -}
s0 :: EpiState
s0 = (interp, indis, 12)

{- Exprime l'ignorance d'Anne, « Anne ne connaît pas le nombre de Bill. » -}
anneIgn :: EpiFormula
anneIgn = And 
            (Not (Knows "a" (Var "bn"))) 
            (Not (Knows "a" (Not (Var "bn"))))

{- Exprime l'ignorance de Bill, « Bill ne sait pas que le visage de Bill est sale, 
et Bill ne sait pas que le visage de Bill n’est pas sale. » -}
billIgn :: EpiFormula
billIgn = And 
        (Not (Knows "b" (Var "an"))) 
        (Not (Knows "b" (Not (Var "an"))))

{- Exprime le problème 4 dans sa totalité :
« Anne ne connaît pas le nombre de Bill, 
et Bill ne connaît pas le nombre de Anne, et Après que Anne annonce son ignorance,
et après que Bill annonce son ignorance, Anne connaît le nombre de Bill. » -}
problem4 :: EpiFormula
problem4 = And 
            (And anneIgn billIgn)
            (After 
                anneIgn 
                (After billIgn (Not anneIgn))
            )