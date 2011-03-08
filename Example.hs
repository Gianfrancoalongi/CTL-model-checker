
{-      *****************************************************
        Example module
        Gianfranco Alongi       AKA zenon
        gianfranco.alongi@gmail.com
        20071020
        *****************************************************
-}
--------------------------------------------------------------------------
module Example where

import CTL
import Prelude  as P
import Data.Set as S
import Data.Map as M

-- 1: List of states in model
-- 2: Transitions
-- 3: Valid atoms in each state
parse :: [State] -> [(State,State)] -> [(State,[Char])] -> Model
parse s trans lbls =
   Model
   (S.fromList s)
   (S.fromList trans)
   (M.fromList$P.map(\(x,y)->(x,S.fromList$P.map(\x'->Atm x')y))lbls)
   

--------------------------------------------------------------------------
example = 
    parse ["S0","S1","S2","S3"] -- States
          [("S0","S3"),         -- Transitions
           ("S0","S1"),
           ("S3","S0"),
           ("S2","S3"),
           ("S1","S1"),
           ("S1","S2")]
          [("S0","pq"),         -- Labeling function
           ("S1","r"),
           ("S2","pt"),
           ("S3","qr")]

-- Example formulas
phi_a = U EG (Atm 'q')                    -- EG q 
phi_b = U EG (Neg (Atm 'r') )             -- EG (~ r)
phi_c = U EG (B Or (Atm 'p') (Atm 'q'))   -- EG (p v q )
phi_d = U EG (Atm 'r')                    -- EG r
--------------------------------------------------------------------------
{-      To run the example model with the example formulas.
        Load into hugs or ghci. Then type
        > sat example phi_a
        And the program produces the set of states satisfying the formula
        phi_a. The same goes for phi_b, phi_c, phi_d.
-}
