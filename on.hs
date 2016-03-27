{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import DataCreation

$(getDataFromJSON)

dataPerson = JSON {name = "Oleg", arr = [1,2,3], age = 12, avg = 12}

main = print dataPerson

