module Verification where 

import CTL
import Kripke
import SExpr

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe (fromJust)

-- Single-state verifier for a CTL formula with a given model and state.
isValid :: KripkeModel -> State -> Set (State, CTL) -> CTL -> Bool
isValid m s v TTrue      = True
isValid m s v TFalse     = False
isValid m s v (TAtom p)  = p `holdsIn` (m, s) 
isValid m s v (TNeg p)   = not $ isValid m s v p
isValid m s v (TAnd p q) = (isValid m s v p) && (isValid m s v q)
isValid m s v (TOr p q)  = (isValid m s v p) || (isValid m s v q)
isValid m s v (TEx p)    = any (\s' -> isValid m s' v p) (neighbors m s)
isValid m s v (TAx p)    = all (\s' -> isValid m s' v p) (neighbors m s)
isValid m s v (TEf p)
  | Set.member (s, (TEf p)) v = False
  | otherwise = isValid m s (Set.insert (s, TEf p) v) (p `TOr` (TEx . TEf $ p))
isValid m s v (TEg p)
  | Set.member (s, (TEg p)) v = True
  | otherwise = isValid m s (Set.insert (s, TEg p) v) (p `TAnd` (TEx . TEg $ p))


data Problem = Problem KripkeModel State CTL
-- Parses an S-Expression into a verification problem instance
-- A prolem S-Expression is of the form
-- P := (problem MOD (state n) (formula CTL))
-- where MOD is a valid model S-Expression (as defined in the Kripke module),
-- CTL is a valid CTL formula S-Expression (as defined in the CTL module)
-- and n is an integer for the state to verify the formula from
parseProblem :: SExpr -> Problem
parseProblem 
  (List [Sym "problem", 
          mod@(List (Sym "model":_)),
          List [Sym "state", Num n],
          List [Sym "formula", f]]) = Problem (parseKripke mod) n (parseCTL f)

-- Solves an instance of a verification problem instance
solveProblem :: Problem -> Bool
solveProblem (Problem m s p) = isValid m s Set.empty p

-- Parses a problem from a string and solves it
verifyString :: String -> Bool
verifyString = solveProblem . parseProblem . fromJust . parseSExpr

-- Parses a problem form a file of the given file path and solves it
verifyFile :: FilePath -> IO Bool
verifyFile s = verifyString <$> readFile s
