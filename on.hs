{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import DataCreation

--Person = Name String
$(getDataFromJSON)

dataPerson = Name {age = "Oleg"}

main = print dataPerson

