module Kripke where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import SExpr 


type State = Integer
data KripkeModel = KripkeModel
  { graph  :: Map State [State]
  , values :: Map State (Set String)
  } deriving (Show)

parseKripke :: SExpr -> KripkeModel
parseKripke (List (Sym "model":states)) = KripkeModel { graph = pStates, values = pVals }
  where
    (pStates, pVals) = foldl parseState (Map.empty, Map.empty) states
    parseState (g, v) (List [Num n, List vals, List ns]) = (g', v')
      where g' = Map.insert n (map (\(Num m) -> m) ns) g
            v' = Map.insert n (Set.fromList $ map (\(Sym s) -> s) vals) v
    parseState _ _ = error "Not a valid Kripke model."

parseKripke _ = error "Not a valid Kripke model."


holdsIn :: String -> (KripkeModel, State) -> Bool
holdsIn p (k, s) = Set.member p ((values k) Map.! s)

neighbors :: KripkeModel -> State -> [State]
neighbors m s = (graph m) Map.! s

--
