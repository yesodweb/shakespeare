module ShakespeareBaseTest (specs) where

import Test.Hspec
import Text.Shakespeare

import Text.ParserCombinators.Parsec (parse, ParseError, (<|>))
import Text.Shakespeare.Base (parseVarString, parseUrlString, parseIntString)
import Text.Shakespeare (preFilter, defaultShakespeareSettings, ShakespeareSettings(..), PreConvert(..), PreConversion(..))

-- run :: Text.Parsec.Prim.Parsec Text.Parsec.Pos.SourceName () c -> Text.Parsec.Pos.SourceName -> c

specs :: Spec
specs = describe "shakespeare-js" $ do
  {-
  it "parseStrings" $ do
    run varString "%{var}" `shouldBe` Right "%{var}"
    run urlString "@{url}" `shouldBe` Right "@{url}"
    run intString "^{int}" `shouldBe` Right "^{int}"

    run (varString <|> urlString <|> intString) "@{url} #{var}" `shouldBe` Right "@{url}"
  -}

  it "preFilter off" $ do
    preFilter defaultShakespeareSettings template
      `shouldReturn` template

  it "preFilter on" $ do
    preFilter preConversionSettings template `shouldReturn`
      "(function(yesod_var_int, yesod_var_url, yesod_var_var){unchanged yesod_var_var yesod_var_url yesod_var_int})(^{int}, @{url}, #{var})"

  it "preFilter ignore quotes" $ do
    preFilter preConversionSettings templateQuote `shouldReturn`
      "(function(yesod_var_url){unchanged '#{var}' yesod_var_url '^{int}'})(@{url})"

  it "preFilter ignore comments" $ do
    preFilter preConversionSettings templateCommented
      `shouldReturn` "(function(){unchanged & '#{var}' @{url} '^{int}'})()"

  where
    varString = parseVarString '%'
    urlString = parseUrlString '@' '?'
    intString = parseIntString '^'

    preConversionSettings = defaultShakespeareSettings {
      preConversion = Just PreConvert {
          preConvert = Id
        , preEscapeBegin = "`"
        , preEscapeEnd = "`"
        , preEscapeIgnoreBalanced = "'\""
        , preEscapeIgnoreLine = "&"
        , wrapInsertion = Just WrapInsertion { 
            wrapInsertionStartBegin = "(function("
          , wrapInsertionSeparator = ", "
          , wrapInsertionStartClose = "){"
          , wrapInsertionEndBegin = "})("
          , wrapInsertionEndClose = ")"
          }
        }
    }
    template  = "unchanged #{var} @{url} ^{int}"
    templateQuote = "unchanged '#{var}' @{url} '^{int}'"
    templateCommented = "unchanged & '#{var}' @{url} '^{int}'"

    run parser str = eShowErrors $ parse parser str str

    eShowErrors :: Either ParseError c -> c
    eShowErrors = either (error . show) id

