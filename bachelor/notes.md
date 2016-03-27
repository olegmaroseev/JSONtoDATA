## Data.Aeson ##
https://github.com/bos/aeson
https://artyom.me/aeson

Data.Aeson - библиотека для работы с файлами в формате JSON. Самый простой способ использования библотеки - определить DATA. Помимо простого кодирования/декодирования JSON она также позволяет удобным образом писать сериализаторы и десериализаторы для произвольных типов. Подробности вы найдете в документации по пакету.

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

