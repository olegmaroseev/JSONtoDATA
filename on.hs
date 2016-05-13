{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import DataCreation

data Arra = Arra {fg::String}
   deriving (Show,Eq)

$(getDataFromJSON)

--dataPerson = JSONData {name = "Oleg", age = 12, avg = 12, arra = Arra {fg = "Oleg"}}
dataPerson = JSONData {name = "Oleg", age = 12, avg = 12, arr = [1,2,3]}

main = print dataPerson
