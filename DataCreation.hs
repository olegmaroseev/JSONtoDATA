{-# LANGUAGE OverloadedStrings #-}

module DataCreation where

import Language.Haskell.TH
import Data.Aeson
import Data.Maybe
import Data.HashMap.Strict as StrHash
import Data.Text
import Data.Char
import Data.Generics

personJSON =  fromJust $ decode $ "{\"name\":\"Joe\"}" :: Maybe Value

toHashMap :: Value -> Object
toHashMap (Object obj) = obj

getMapKeys :: Maybe Value -> [Text]
getMapKeys map' = foldlWithKey' (\list' key' val' -> key' : list') [] (toHashMap $ fromJust $ map')

firstLetterToUpper :: String -> String
firstLetterToUpper fieldName = (Data.Char.toUpper $ Prelude.head fieldName) : (Prelude.tail fieldName)

getDataFromJSON::DecsQ
getDataFromJSON = do
  return $ 
        [DataD 
                [] 
                (mkName "Person") 
                [] 
                [
                        NormalC (mkName $ firstLetterToUpper $ Data.Text.unpack $Prelude.head $ getMapKeys personJSON) 
                                [(NotStrict, ConT (mkName "String"))]
                ] 
                [mkName "Show"]
        ]
