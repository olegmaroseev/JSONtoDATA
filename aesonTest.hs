{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Maybe
import Data.HashMap.Strict as StrHash
import Data.Text
import Data.Generics

decodedSimpJSON = fromJust $ decode $ "{\"name\":\"Oleg\",\"age\": 20 }" :: Maybe Value

decodedCompJSON = fromJust $ decode $ "{\"name\":\"Joe\",\"age\":{\"foo\": 123}}" :: Maybe Value

toHashMap (Object x) = x

--lookup "age" y :: [(String, Object)]

-- Creating data with Haskell Template?

--Cast [(String, Object)]

xxx = StrHash.lookup (pack "age") $ toHashMap $ fromJust $ decodedSimpJSON

--typeOf

--nub Hash
