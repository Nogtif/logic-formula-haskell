{-# OPTIONS_GHC -Wall #-}
{--
    LCPF : Projet
    Les enfants bouées
    by Quentin Carpentier & Paul-Joseph Krogulec.
-}

module Problem1 where
import EL

interp :: Prop -> [World]
interp "as" = [10 ,11]
interp "bs" = [01 ,11]
interp _    = []

indis :: Agent  -> World  -> [World]
indis "a" 00 = [00 ,10]
indis "b" 00 = [00 ,01]