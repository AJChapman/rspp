module RSPP (
    Pledge(Pledge), pledgeClauses, pledgeLimit,
    PledgeClause(FixedPledge, RationalPledge), rpAbove, rpPerUnit, rpUnit,
    solve,
    evalClause, evalPledge, evalPledges,
    pledgeMax, pledgeMin, maxPledges, minPledges,
) where

import Data.Foldable as F

-- The 'c' typeclass is for your currency datatype, ie. 'Centi'

data PledgeClause c = FixedPledge c
                    | RationalPledge { rpAbove :: c, rpPerUnit :: c, rpUnit :: c }
                    deriving (Eq, Read, Show)

data Pledge c = Pledge { pledgeClauses :: [PledgeClause c], pledgeLimit :: c } deriving (Eq, Read, Show)

evalClause :: (Ord c, Fractional c) => c -> PledgeClause c -> c
evalClause     _ (FixedPledge x)                  = x
evalClause total (RationalPledge above perUnit unit) = max (fromInteger 0) $ ((total - above) * perUnit) / unit

mapSum :: (Foldable t, Num c) => (a -> c) -> t a -> c
mapSum f p = F.foldr step (fromInteger 0) p
    where step a sum = f a + sum

-- Note: a pledge is evaluated as though its own contribution has already been added to the total!
evalPledge :: (Ord c, Fractional c) => c -> Pledge c -> c
evalPledge total (Pledge clauses limit) =
    min limit (mapSum (evalClause total) clauses)

pledgeMax :: Pledge c -> c
pledgeMax = pledgeLimit

maxPledges :: (Foldable t, Num c) => t (Pledge c) -> c
maxPledges = mapSum pledgeMax

pledgeMin :: (Ord c, Fractional c) => Pledge c -> c
pledgeMin = evalPledge $ fromInteger 0

minPledges :: (Foldable t, Ord c, Fractional c) => t (Pledge c) -> c
minPledges = mapSum pledgeMin

evalPledges :: (Foldable t, Ord c, Fractional c) => c -> t (Pledge c) -> c
evalPledges total pledges = mapSum (evalPledge total) pledges

solveInRange :: (Foldable t, Ord c, Fractional c) => c -> c -> t (Pledge c) -> c
solveInRange l h pledges
    | h == l    = h
    | otherwise =
        let mid = l + ((h - l) / 2)
        in  if l == mid
               then l
               else let totalMid = evalPledges mid pledges
                    in  if totalMid < mid then solveInRange l mid pledges
                                          else solveInRange mid h pledges

solve :: (Foldable t, Ord c, Fractional c) => t (Pledge c) -> c
solve pledges = solveInRange (minPledges pledges) (maxPledges pledges) pledges
