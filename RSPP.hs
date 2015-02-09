{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RSPP where

-- The 'c' typeclass is for your currency datatype, ie. 'Centi'

data PledgeClause c = FixedPledge c
                  | PledgeAbove { paAbove :: c, paPerUnit :: c, paUnit :: c }
                  deriving (Read, Show, Eq)

data Pledge c = Pledge { pClauses :: [PledgeClause c], pLimit :: c } deriving (Read, Show, Eq)

evalClause :: (Ord c, Fractional c) => c -> PledgeClause c -> c
evalClause     _ (FixedPledge x)                  = x
evalClause total (PledgeAbove above perUnit unit) = max (fromInteger 0) $ ((total - above) * perUnit) / unit

-- Note: a pledge is evaluated as though its own contribution has already been added to the total!
evalPledge :: (Ord c, Fractional c) => c -> Pledge c -> c
evalPledge total (Pledge clauses limit) = min limit (sum $ map (evalClause total) clauses)

maxPledge :: Pledge c -> c
maxPledge = pLimit

maxPledges :: Num c => [Pledge c] -> c
maxPledges = sum . map maxPledge

minPledge :: (Ord c, Fractional c) => Pledge c -> c
minPledge = evalPledge $ fromInteger 0

minPledges :: (Ord c, Fractional c) => [Pledge c] -> c
minPledges = sum . map minPledge

evalAllPledges :: (Ord c, Fractional c) => c -> [Pledge c] -> c
evalAllPledges total pledges = sum $ map (evalPledge total) pledges

solveInRange :: (Ord c, Fractional c) => c -> c -> [Pledge c] -> c
solveInRange l h pledges
    | h == l    = h
    | otherwise =
        let mid = l + ((h - l) / 2)
        in  if l == mid
               then l
               else let totalMid = evalAllPledges mid pledges
                    in  if totalMid < mid then solveInRange l mid pledges
                                          else solveInRange mid h pledges

solve :: (Ord c, Fractional c) => [Pledge c] -> c
solve pledges = solveInRange (minPledges pledges) (maxPledges pledges) pledges
