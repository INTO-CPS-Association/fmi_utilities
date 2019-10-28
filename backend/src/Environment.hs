-- For aeson generic. See https://artyom.me/aeson#records-and-json-generics
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Environment where
import Data.Aeson
import GHC.Generics

data Environment = Environment {
    sizeLimitGB :: Integer,
    port :: Int
} deriving (Generic, FromJSON)