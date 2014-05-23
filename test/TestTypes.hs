{-# OPTIONS_GHC -fno-warn-orphans #-}
module TestTypes where

import Control.Monad (liftM, ap)
import Test.QuickCheck
import Types

instance Arbitrary Exp where
  arbitrary =
    resize 10 $
      frequency [ (50, Number `liftM` arbitrary)
                , (25, Sum    `liftM` arbitrary `ap` arbitrary)
                , (25, Prod   `liftM` arbitrary `ap` arbitrary)
                ]
