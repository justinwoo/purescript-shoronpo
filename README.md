# PureScript-Shoronpo

A library for type-level Symbol formatting with other Symbols, e.g. intercalated record labels.

![](https://i.imgur.com/VtZIfff.jpg)

Who doesn't love shoronpo/xiaolongbao?

You might want to use this together with [Jajanmen](https://github.com/justinwoo/purescript-jajanmen) for amazing results (example to be made, but imagine you had a record for select query results and wanted to use its labels in your query template).

## Usage

Define as separate terms or use inline:

```purs
type MyRecord =
  { a :: Int
  , b :: String
  , c :: Unit
  }

-- inferred type:
labels :: SProxy "a, b, c"
labels =
  S.intercalateRecordLabels
    (Proxy :: Proxy MyRecord)
    (SProxy :: SProxy ", ")

-- inferred type:
formatted :: SProxy "my labels: a, b, c"
formatted =
  S.formatSymbol
    (SProxy :: SProxy "my labels: {labels}")
    { labels }
```

Then put it to work:

```purs
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
```
