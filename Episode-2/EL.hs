{-# OPTIONS_GHC -Wall #-}
{--
    LCPF : Projet
    Les enfants bouées
    by Quentin Carpentier & Paul-Joseph Krogulec.
-}

type Prop = [[Char]]
type Agent = [[Char]]
type World = Int

type EpiState = (Prop -> [World], Agent -> World -> [World], World)