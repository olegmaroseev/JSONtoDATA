{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

import Language.Haskell.TH
import DataCreation
import Data.Aeson

$(getDataFromJSON)

instance ToJSON JSONData where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON JSONData

--dataPerson = JSONData {name = "Oleg", age = 12, avg = 12, arra = Arra {fg = "Oleg"}}
dataPerson = JSONData {name = "Oleg", age = 12, avg = 12, arr = [1,2,3]}

encodeJSON = encode dataPerson
