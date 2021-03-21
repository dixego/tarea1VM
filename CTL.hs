module CTL where

import SExpr

-- Type representing CTL formulas.
data CTL
  = TTrue
  | TFalse
  | TAtom String
  | TNeg CTL
  | TAnd CTL CTL
  | TOr CTL CTL
  | TEx CTL
  | TAx CTL
  | TEf CTL
  | TEg CTL
  deriving (Show, Ord, Eq)

-- S-Expression parser for CTL formulas.
-- A CTL S-Expression is one of the following:
-- CTLS := 
-- | true
-- | false
-- | p | q | r |...|
-- (any symbol other than `true` or `false` will be parsed as a propositional variable)
-- | (and CTLS CTLS ...) (n-ary conjunction e.g. (and p q r s))
-- | (or CTLS CTLS ...) (n-ary disjunction)
-- | (not CTLS)
-- | (ex CTLS)
-- | (ax CTLS)
-- | (ef CTLS)
-- | (eg CTLS)
parseCTL :: SExpr -> CTL
parseCTL (Sym "true") = TTrue
parseCTL (Sym "false") = TTrue
parseCTL (Sym p) = TAtom p
parseCTL (List (Sym "and":cdr)) = foldl1 TAnd $ (map parseCTL cdr)
parseCTL (List (Sym "or" :cdr)) = foldl1 TOr $ (map parseCTL cdr)
parseCTL (List [Sym "not", p]) = TNeg $ parseCTL p
parseCTL (List [Sym "ex", p]) = TEx $ parseCTL p
parseCTL (List [Sym "ax", p]) = TAx $ parseCTL p
parseCTL (List [Sym "ef", p]) = TEf $ parseCTL p
parseCTL (List [Sym "eg", p]) = TEg $ parseCTL p
parseCTL _ = error "Not a valid CTL formula."
