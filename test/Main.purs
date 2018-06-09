module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Shoronpo as S
import Test.Assert (assertEqual)
import Type.Prelude (Proxy(..), SProxy(..), reflectSymbol)

type MyRecord =
  { a :: SProxy "A"
  , b :: SProxy "B"
  , c :: SProxy "C"
  }

-- inferred type:
labels :: SProxy "a, b, c"
labels =
  S.intercalateRecordLabels
    (Proxy :: Proxy MyRecord)
    (SProxy :: SProxy ", ")

-- inferred type:
values :: SProxy "A, B, C"
values =
  S.intercalateRecordValues
    (Proxy :: Proxy MyRecord)
    (SProxy :: SProxy ", ")

-- inferred type:
formatted :: SProxy "my labels: a, b, c"
formatted =
  S.formatSymbol
    (SProxy :: SProxy "my labels: {labels}")
    { labels }

main :: Effect Unit
main = do
  let
    myLabels =
      S.intercalateRecordLabels
        (Proxy :: Proxy { apple :: Int, banana :: String })
        (SProxy :: SProxy ", ")
    myFormatted =
      S.formatSymbol
        (SProxy :: SProxy "myLabels: {myLabels}")
        { myLabels }

  assertEqual
    { actual: reflectSymbol myFormatted
    , expected: "myLabels: apple, banana"
    }
  log $ reflectSymbol myLabels
  log $ reflectSymbol myFormatted
