{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import DataCreation

data Arra = Arra {fg::String}
   deriving (Show,Eq)

$(getDataFromJSON)

dataPerson = JSONData {name = "Oleg", age = 12, avg = 12, arra = Arra {fg = "Oleg"}}

main = print dataPerson
