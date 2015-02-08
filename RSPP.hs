{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RSPP where

import Data.Fixed

newtype Money = Money { unMoney :: Centi } deriving (Read, Show, Eq, Ord, Num, Fractional)

data PledgeClause = FixedPledge Money
                  | PledgeAbove { paAbove :: Money, paPerUnit :: Money, paUnit :: Money }

data Pledge = Pledge { pClauses :: [PledgeClause], pLimit :: Money }

evalClause :: Money -> PledgeClause -> Money
evalClause     _ (FixedPledge x)                  = x
evalClause total (PledgeAbove above perUnit unit) = ((total - above) * perUnit) / unit

evalPledge :: Money -> Pledge -> Money
evalPledge total (Pledge clauses limit) = min limit (sum $ map (evalClause total) clauses)

maxAllPledges :: [Pledge] -> Money
maxAllPledges = sum . map pLimit

evalAllPledges :: Money -> [Pledge] -> Money
evalAllPledges total pledges = sum $ map (evalPledge total) pledges

solveInRange :: Money -> Money -> [Pledge] -> Money
solveInRange l h pledges = if l == h then h
                           else let mid      = l + ((h - l) / 2)
                                    totalMid = evalAllPledges mid pledges
                                in if totalMid < mid then solveInRange l mid pledges
                                   else solveInRange mid h pledges

solve :: [Pledge] -> Money
solve pledges = solveInRange 0.00 (maxAllPledges pledges) pledges

{-
-- A type wrapping an underlying money type, with a phantom type
newtype Money t m = Money { unMoney :: m }

-- Phantom types for use with money
data FixedAmount
data AmountAbove
data AmountPerUnit
data Unit
data Limit
data PledgeClauseTotal
data PledgeTotal
data TotalRaised

data PledgeClause m = FixedPledge (Money FixedAmount m)
                    | PledgeAbove (Money AmountAbove m) (Money AmountPerUnit m) (Money Unit m)

data Pledge m = Pledge [PledgeClause m] (Money Limit m)

evalClause :: Fractional m => Money TotalRaised m -> PledgeClause m -> Money PledgeClauseTotal m
evalClause _ (FixedPledge x) = Money . unMoney $ x
evalClause (Money t) (PledgeAbove (Money above) (Money perUnit) (Money unit))
  = Money $ ((t - above) * perUnit) / unit

evalPledge :: Pledge m -> Money TotalRaised m -> Money PledgeTotal m
evalPledge (Pledge pcs (Money limit)) t = Money $ min (sum $ map (evalClause t) pcs) limit
-}
