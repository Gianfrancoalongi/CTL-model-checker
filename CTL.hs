{-      
        ********************************************************
        Gianfranco Alongi AKA zenon                     
        gianfranco.alongi@gmail.com                     20071020
        ********************************************************
-}
module CTL where
--------------------------------------------------------------------------
import Prelude  as P
import Data.Set as S
import Data.Map as M
import Data.Maybe
-- Grammar of CTL
data Sentence   = Neg Sentence  | Atm Char 
                | U Mod Sentence| B Con Sentence Sentence
                deriving(Show,Eq,Ord)
-- Connectives
data Con        = And | Or | Implies
                deriving(Show,Eq,Ord)
-- Modalities
data Mod        = AX | AF | AG | EX | EF | EG 
                deriving(Show,Eq,Ord)
-- Model
type State      = String
data Model      = Model { set   ::(Set State),
                         trans  ::(Set (State, State)),
                         label  ::(Map State (Set Sentence))}
                deriving(Show,Eq)
--------------------------------------------------------------------------
-- The SAT algorithm
sat :: Model -> Sentence -> Set State
sat m (Neg s)   = (set m) S.\\ sat m s
sat m (Atm c)   = S.fromList [s'| s' <- S.toList $ set m,
                S.member (Atm c) (fromJust $ M.lookup s' (label m) ) ]
sat m (B c s s')= case c of
                And     -> sat m s `S.intersection` sat m s'
                Or      -> sat m s `S.union` sat m s'
                Implies -> sat m (B Or (Neg s) s')
sat m (U t s')  = case t of
                AF -> fxPnt m (sat m s') preA S.union
                AG -> fxPnt m (sat m s') preA S.intersection
                EF -> fxPnt m (sat m s') preE S.union
                EG -> fxPnt m (sat m s') preE S.intersection
                AX -> preA m $ sat m s'
                EX -> preE m $ sat m s'

-- The preE algorithm, return all states which go to the set of
-- states in Set State
preE :: Model -> Set State -> Set State
preE m s = S.fromList [s''| s' <- S.toList s,
                    (s'',s''') <- S.toList $ trans m,s'''==s']

-- The preA algorithm, return all states which ONLY go to a state
-- in the set of states.
preA :: Model -> Set State -> Set State
preA m s = S.fromList[s''|s' <- S.toList $ s,
			  s''<- S.toList $ set m,
                     let (y,_)= S.partition (\(x,x')->x==s'')$trans m in
                       and (P.map (\(x,x')->S.member x' s )$ S.toList y)]

-- Fix point algorithm.
fxPnt :: Model -> Set State -> (Model -> Set State -> Set State) ->
	(Set State -> Set State -> Set State) -> Set State
fxPnt m s pre sFun = let s' =  s `sFun` (pre m s) in 
                     if  s  == s' then s' else fxPnt m s' pre sFun
-- -----------------------------------------------------------------------
