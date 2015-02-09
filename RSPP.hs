{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RSPP where

import Data.Fixed

newtype Money = Money { unMoney :: Centi } deriving (Read, Show, Eq, Ord, Num, Fractional)

data PledgeClause = FixedPledge Money
                  | PledgeAbove { paAbove :: Money, paPerUnit :: Money, paUnit :: Money }
                  deriving (Read, Show, Eq)

data Pledge = Pledge { pClauses :: [PledgeClause], pLimit :: Money } deriving (Read, Show, Eq)

evalClause :: Money -> PledgeClause -> Money
evalClause     _ (FixedPledge x)                  = x
evalClause total (PledgeAbove above perUnit unit) = max (Money 0.00) $ ((total - above) * perUnit) / unit

-- Note: a pledge is evaluated as though its own contribution has already been added to the total!
evalPledge :: Money -> Pledge -> Money
evalPledge total (Pledge clauses limit) = min limit (sum $ map (evalClause total) clauses)

maxPledge :: Pledge -> Money
maxPledge = pLimit

maxPledges :: [Pledge] -> Money
maxPledges = sum . map maxPledge

minPledge :: Pledge -> Money
minPledge = evalPledge $ Money 0.00

minPledges :: [Pledge] -> Money
minPledges = sum . map minPledge

evalAllPledges :: Money -> [Pledge] -> Money
evalAllPledges total pledges = sum $ map (evalPledge total) pledges

solveInRange :: Money -> Money -> [Pledge] -> Money
solveInRange l h pledges = if (h - l) <= (Money 0.01) then h
                           else let mid      = l + ((h - l) / 2)
                                    totalMid = evalAllPledges mid pledges
                                in if totalMid < mid then solveInRange l mid pledges
                                   else solveInRange mid h pledges

solve :: [Pledge] -> Money
solve pledges = solveInRange (minPledges pledges) (maxPledges pledges) pledges
