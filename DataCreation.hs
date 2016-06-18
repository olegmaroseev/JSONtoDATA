{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module DataCreation where

import Language.Haskell.TH
import Data.Aeson
import Data.Maybe
import qualified Data.HashMap.Strict as StrHash
import Data.Text
import Data.Char
import Data.Generics
import qualified Data.Vector      as V
import Language.Haskell.TH.Syntax
import Data.Vector
import Control.Monad.State        as MState
import qualified Data.Foldable    as FB
import qualified Data.Map         as M
import qualified Data.List        as DList

extraSimple = fromJust $ decode $ "{\"name\":\"Joe\"}" :: Value

personJSON =  fromJust $ decode $ "{\"name\":\"Joe\",\"age\":25,\"avg\":4,\"arr\" : [1,2,3]}" :: Value

personJSON2 =  fromJust $ decode $ "{\"name\":\"Joe\",\"age\":25,\"avg\":4,\"arra\" : {\"fg\" : \"qwerty\"}}" :: Value

arrJs = fromJust $ decode $ "{\"fg\" : \"qwerty\"}" ::Value

compJSON = fromJust $ decode $ "{\"name\":\"Joe\",\"age\":{\"foo\": {\"r\" : 12}}}" :: Value

greatEx = fromJust $ decode $ "{\"glossary\": {\"title\": \"example glossary\",\"glossDiv\": { \"tit\": \"S\", \"glossList\": { \"glossEntry\": { \"id\": \"SGML\", \"sortAs\": \"SGML\", \"glossTerm\": \"Standard Generalized Markup Language\", \"acronym\": \"SGML\", \"abbrev\": \"ISO 8879:1986\", \"glossDef\": { \"para\": \"A meta-markup language, used to create markup languages such as DocBook.\", \"glossSeeAlso\": [\"GML\", \"XML\"] }, \"glossSee\": \"markup\"} } }}}" :: Value

arrObjects = fromJust $ decode $ "{\"students\" :[{\"name\": \"Oleg\", \"age\" : 21} , {\"name\": \"Igor\", \"age\" : 17}, {\"name\": \"Semen\", \"age\" : 20}]}" :: Value

--получение Hashmap из объекта
toHashMap :: Value -> Object
toHashMap (Object obj) = obj

toVector' (Array arr) = arr

--проверка на то, что Value является Object
isObject :: Value -> Bool
isObject (Object obj) = True;
isObject _ = False;

--проверка на то, что Value является Array
isArray :: Value -> Bool
isArray (Array obj) = True;
isArray _ = False;

--проверка на сложность JSON
isComp:: Value -> Bool
isComp (Object obj) = StrHash.foldl' (\pred elem -> if (isObject elem) then True
                                                  || pred else pred) False (obj)

--получение ключей HasMap
getMapKeys :: Maybe Value -> [Text]
getMapKeys map' = StrHash.foldlWithKey' (\list' key' val' -> if (isObject val')
                          then key' : list'
                          else key' : list') [] (toHashMap $ fromJust $ map')

--переход первой буквы из строчной в заглавную
firstLetterToUpper :: String -> String
firstLetterToUpper (x:xs) = (Data.Char.toUpper $ x)
                                                     : (xs)

--создание списка типов для полей
makeListVarStrictType :: Foldable t => t a -> [String -> Name]
makeListVarStrictType fNames = DList.foldl (\list' val' -> (mkName) : list')
                                                                       [] fNames

--извлечение содержимого из типа
getContentFromType :: Type -> Name
getContentFromType (ConT val') = val'

--создание типа для определения синтаксиса полей
mkValType :: Value -> String -> Type
mkValType (Number val') name'= ConT (mkName "Float")
mkValType (String val') name' = ConT (mkName "String")
mkValType (Bool val') name' = ConT (mkName "Bool")
mkValType (Array val') name' = AppT (ListT)
                                    (mkValType (Data.Vector.head val') name')
mkValType (Object val') name' = ConT (mkName $ firstLetterToUpper name')

--количество вложенных объектов
numOfInsertedObjects:: Value -> Int
numOfInsertedObjects (Object obj) = StrHash.foldlWithKey' (\acc' key' val' ->
         if isComp (Object obj)
           then acc' + 1 + numOfInsertedObjects val'
           else acc' + numOfInsertedObjects val') 0 obj
numOfInsertedObjects _ = 0

--преобразование HashMap
mapKeys' :: StrHash.HashMap Text v -> StrHash.HashMap String v
mapKeys' =  (StrHash.foldlWithKey'
    (\map' key' value' -> StrHash.insert (Data.Text.unpack key') value' map')
    StrHash.empty)

--монадическая свертка Hashmap по ключу и значению
foldlWithKeyM :: (Monad m) => (b -> k -> a -> m b) -> b ->
                                                      StrHash.HashMap k a -> m b
foldlWithKeyM f b = FB.foldlM f' b . StrHash.toList
  where f' a = uncurry (f a)

--конвертация полей объекта
convertFields:: MonadState [Dec] m => Value -> m [(Name, Strict, Type)]
convertFields map' = foldlWithKeyM upd [] (mapKeys' $ toHashMap map')
 where
    upd list' key' val'
        | isObject val' =
                do
                  result <- convertFields $ val'
                  MState.modify ((DList.++) [DataD
                          []
                          (mkName $ firstLetterToUpper key')
                          []
                          [ RecC (mkName $ firstLetterToUpper key')  (result) ]
                          [mkName "Generic", mkName "Show", mkName "Eq"] ])
                  (MState.return  (((mkName $  key'), NotStrict,
                                             (   mkValType val' key')) : list'))

	| (isArray val') && (isObject $ V.head $ toVector' val') =
		do
		  result <- convertFields $ V.head $ toVector' val'
		  MState.modify ((DList.++) [DataD
                          []
                          (mkName $ firstLetterToUpper key')
                          []
                          [ RecC (mkName $ firstLetterToUpper key')  (result) ]
                          [mkName "Generic", mkName "Show", mkName "Eq"] ])
                  (MState.return  (((mkName $  key'), NotStrict,
                                             (   mkValType val' key')) : list')) 
                  
        | otherwise =
                do
                  (MState.return (((mkName $  key'), NotStrict,
                                            (mkValType val' key') )    : list'))

--конвертация основного объекта
convertObject:: String -> Value -> State [Dec] ()
convertObject name' json' = do
    result <- convertFields json'
    MState.modify $ (DList.++) [DataD
                []
                (mkName name')
                []
                [ RecC (mkName name') result ]
                [mkName "Generic",mkName "Show", mkName "Eq"] ]

--DecsQ = Q [Dec]
getDataFromJSON::DecsQ
getDataFromJSON = do
  return $
          (snd (runState (convertObject "JSONData" arrObjects) $ []) )
