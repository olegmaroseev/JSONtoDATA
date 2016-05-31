## Data.Aeson ##
https://github.com/bos/aeson
https://artyom.me/aeson

A JSON parsing and encoding library optimized for ease of use and high performance.

There are 2 main classes used in Aeson – FromJSON and ToJSON. A type which you want to convert to/from JSON should be an instance of these classes. You can think of FromJSON as of Read, and of ToJSON as of Show – but instead of reading from a string or converting to a string, you read from JSON or convert to JSON.

There are also 2 functions for actually doing “reading” and “showing”, which are called decode and encode. (decode differs from read a bit by returning Nothing if reading was unsuccessful, instead of throwing an exception – so, it's closer to readMaybe in this regard.)

Data.Aeson - библиотека для работы с файлами в формате JSON. Самый простой способ использования библотеки - определить DATA. Помимо простого кодирования/декодирования JSON она также позволяет удобным образом писать сериализаторы и десериализаторы для произвольных типов. 

A JSON value represented as a Haskell value.
There are 2 main classes used in Aeson – FromJSON and ToJSON. A type which you want to convert to/from JSON should be an instance of these classes. 
Aeson имеют свой собственный тип для представления конвертируемого JSON файла. Этот тип называется Value и имеет 6 конструкторов:
```haskell
data Value
  = Object Object
  | Array Array
  | String Text
  | Number Scientific
  | Bool Bool
  | Null
```

Valuе представляет собой обертки над стандартными классами, поэтому с экземплярами этого типа легко работать.
```haskell
type Object = HashMap Text Value
type Array = Vector Value
```


The most common way to use the library is to define a data type, corresponding to some JSON data you want to work with, and then write either a FromJSON instance, a to ToJSON instance, or both for that type.

```haskell
ghci> :set -XOverloadedStrings
ghci> :m + Data.Aeson
ghci> import Data.ByteString.Lazy.Char8 as BSLC
ghci> BSLC.unpack $ encode $ object [("aaa", Number 123)]
"{\"aaa\":123}"
ghci> decode $ encode $ object [("aaa", Number 123)] :: Maybe (Object)
Just fromList [("aaa",Number 123)]
ghci> decode (BSLC.pack "[1,2,3]") :: Maybe [Int]
Just [1,2,3]
```

## Template Haskell ##
http://eax.me/template-haskell/
https://habrahabr.ru/post/131998/

Template Haskell (далее TH) — это расширение языка Haskell предназначенное для мета-программирования. Оно даёт возможность алгоритмического построения программы на стадии компиляции. Это позволяет разработчику использовать различные техники программирования, не доступные в самом Haskell’е, такие как, макро-подобные расширения, направляемые пользователем оптимизации (например inlining), обобщённое программирование (polytypic programming), генерация вспомогательных структур данных и функций из имеющихся.

