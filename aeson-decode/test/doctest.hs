import Test.DocTest

main :: IO ()
main =
  doctest [ "-isrc", "src/AesonDecode.hs"
          , "-XQuasiQuotes"
          , "-XOverloadedStrings"
          ]
