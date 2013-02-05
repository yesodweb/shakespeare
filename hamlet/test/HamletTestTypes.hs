module HamletTestTypes where

-- This record is defined outside of the HamletTest module
-- because record wildcards use 'reify' and reify doesn't
-- see types defined in the local module.

data ARecord = ARecord { field1 :: Int, field2 :: Bool }
