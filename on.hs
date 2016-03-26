{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import DataCreation

--Person = Name String
$(getDataFromJSON)

dataPerson = Name "Oleg"

main = print dataPerson

