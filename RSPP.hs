module RSPP (
  -- * Types
  -- ** Pledge
  Pledge(Pledge, pledgeClauses, pledgeLimit),
  -- ** PledgeClause
  PledgeClause(FixedPledge, RationalPledge, rpAbove, rpPerUnit, rpUnit),
  -- * Functions
  solve,
  evalClause, evalPledge, evalPledges,
  pledgeMax, pledgeMin, maxPledges, minPledges,
) where

import Data.Foldable as F

-- | A single pledge. Collect these in a Foldable (ie. List), and calculate the total with 'solve'.
-- The 'c' type parameter is for your currency datatype, ie. 'Centi' from Data.Fixed.
data Pledge c =
    Pledge { -- | A list of clauses in this pledge
             pledgeClauses :: [PledgeClause c],
             -- | This pledge may never exceed this amount
             pledgeLimit :: c
           } deriving (Eq, Read, Show)

-- | A clause within a pledge, describing its behaviour.
-- The clauses within the pledge are added together, but may never exceed the 'pledgeLimit'.
data PledgeClause c
    -- | Simply contribute a fixed amount
    = FixedPledge c

    -- | Contribute 'rpPerUnit' for every 'rpUnit' raised above 'rpAbove'
    | RationalPledge { rpAbove :: c, rpPerUnit :: c, rpUnit :: c}

    deriving (Eq, Read, Show)

-- | The key function: it does a binary search to find the maximum total which satisfies all pledges.
solve :: (Foldable t, Ord c, Fractional c) => t (Pledge c) -- ^ All pledges
                                           -> c            -- ^ The maximum consistent total
solve pledges = solveInRange (minPledges pledges) (maxPledges pledges) pledges

-- A helper function for solve
solveInRange :: (Foldable t, Ord c, Fractional c) => c -> c -> t (Pledge c) -> c
solveInRange l h pledges
    | h == l    = h -- We have converged on the solution
    | otherwise =
        let mid = l + ((h - l) / 2) -- Find the midpoint between 'h' and 'l'
        in  if l == mid
               then l -- We have converged on the solution
               else let totalMid = evalPledges mid pledges
                    in  if totalMid < mid then solveInRange l mid pledges -- The solution is in the lower half of the range
                                          else solveInRange mid h pledges -- The solution is in the upper half of the range

-- | Given a particular total, what is the total of all the pledges?
-- Note: This may not give a valid result! Use 'solve' for that.
evalPledges :: (Foldable t, Ord c, Fractional c) => c            -- ^ The total to evaluate against
                                                 -> t (Pledge c) -- ^ All pledges
                                                 -> c            -- ^ The total of all pledges evaluated against the given total
evalPledges total = mapSum (evalPledge total)

-- | Given a particular total, how much of it was this pledge?
-- Note: the given total includes the total of this pledge!
evalPledge :: (Ord c, Fractional c) => c        -- ^ The total raised, including this pledge
                                    -> Pledge c -- ^ The pledge to evaluate
                                    -> c        -- ^ What this pledge contributes to the given total
evalPledge total (Pledge clauses limit) =
    min limit (mapSum (evalClause total) clauses)

-- | Given a particular total, how much did this clause contribute to that total?
evalClause :: (Ord c, Fractional c) => c              -- ^ The total raised, including this clause
                                    -> PledgeClause c -- ^ The clause to evaluate
                                    -> c              -- ^ What this clause contributes to the given total
evalClause     _ (FixedPledge x)                  = x
evalClause total (RationalPledge above perUnit unit) = max 0 $ ((total - above) * perUnit) / unit

-- | The highest conceivable total that these pledges may contribute, based on their limits.
-- Note that it may not be possible to actually reach this total.
maxPledges :: (Foldable t, Num c) => t (Pledge c) -> c
maxPledges = mapSum pledgeMax

-- | The most this pledge could contribute.
-- This currently just uses the pledge's limit, though this may not always be quite right.
-- Consider the following pledge:
--     Pledge [FixedPledge 10] 100
-- The limit of 100 will certainly never be reached.
pledgeMax :: Pledge c -> c
pledgeMax = pledgeLimit

-- | The lowest conceivable total that these pledges may contribute.
minPledges :: (Foldable t, Ord c, Fractional c) => t (Pledge c) -> c
minPledges = mapSum pledgeMin

-- | The least this pledge may contribute. May be above 0 if it contains a FixedPledge clause.
pledgeMin :: (Ord c, Fractional c) => Pledge c -> c
pledgeMin = evalPledge 0

-- A utility function: map a function over a 't a', and then sum the results
-- TODO: Is this actually better than using fmap and foldr?
mapSum :: (Foldable t, Num c) => (a -> c) -> t a -> c
mapSum f = F.foldr step 0
    where step a accum = f a + accum
