import RSPP
import Data.Fixed -- for our money type

type Money = Centi

pledges :: [Pledge Money]
pledges =
  [ Pledge [RationalPledge 100.00 5.00 1.00] 250.00                   -- Pledge $1 for every $5 raised above $100, up to a limit of $250
  , Pledge [FixedPledge 50.00, RationalPledge 25.00 2.00 1.00] 500.00 -- Pledge $50, plus $1 for every $2 raised above $25, up to a limit of $500
  , Pledge [FixedPledge 200.00] 200.00                                -- Pledge $200
  ]

total :: Money
total = solve pledges -- 700.00

pledgeAmounts :: [Money]
pledgeAmounts = fmap (evalPledge total) pledges -- [0.00, 500.00, 200.00]

