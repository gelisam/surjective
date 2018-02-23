module Main where

import Test.DocTest

-- $setup
-- >>> :set -XLambdaCase
-- >>> :set -XTemplateHaskell
-- >>> :set -Wall
-- >>> import Surjective

-- |
-- >>> const () $$(surjective [||\covers -> \case { "true" -> covers $ \(Just True) -> Just True; _ -> covers $ \Nothing -> Nothing } ||])
-- ...
-- ...Just False...
-- ...
--
-- >>> $$(surjective [||\covers -> [ covers $ \(Just True) -> Just True, covers $ \Nothing -> Nothing] ||])
-- ...
-- ...Just False...
-- ...


main :: IO ()
main = doctest ["test/doctests.hs"]
