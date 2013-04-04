module ShakespeareBaseTest (specs) where

import Test.Hspec
import Text.Shakespeare

import Text.ParserCombinators.Parsec (parse, ParseError, (<|>))
import Text.Shakespeare.Base (parseVarString, parseUrlString, parseIntString)
import Text.Shakespeare (preFilter, defaultShakespeareSettings, ShakespeareSettings(..), PreConvert(..), PreConversion(..))

-- run :: Text.Parsec.Prim.Parsec Text.Parsec.Pos.SourceName () c -> Text.Parsec.Pos.SourceName -> c

specs :: Spec
specs = describe "shakespeare-js" $ do
  let preFilterN = preFilter Nothing
  {-
  it "parseStrings" $ do
    run varString "%{var}" `shouldBe` Right "%{var}"
    run urlString "@{url}" `shouldBe` Right "@{url}"
    run intString "^{int}" `shouldBe` Right "^{int}"

    run (varString <|> urlString <|> intString) "@{url} #{var}" `shouldBe` Right "@{url}"
  -}

  it "preFilter off" $ do
    preFilterN defaultShakespeareSettings template
      `shouldReturn` template

  it "preFilter on" $ do
    preFilterN preConversionSettings template `shouldReturn`
      "(function(shakespeare_var_var, shakespeare_var_url, shakespeare_var_int){unchanged shakespeare_var_var shakespeare_var_url shakespeare_var_int})(#{var}, @{url}, ^{int});\n"

  it "preFilter ignore quotes" $ do
    preFilterN preConversionSettings templateQuote `shouldReturn`
      "(function(shakespeare_var_url){unchanged '#{var}' shakespeare_var_url '^{int}'})(@{url});\n"

  it "preFilter ignore comments" $ do
    preFilterN preConversionSettings templateCommented
      `shouldReturn` "unchanged & '#{var}' @{url} '^{int}'"

  where
    varString = parseVarString '%'
    urlString = parseUrlString '@' '?'
    intString = parseIntString '^'

    preConversionSettings = defaultShakespeareSettings {
      preConversion = Just PreConvert {
          preConvert = Id
        , preEscapeIgnoreBalanced = "'\""
        , preEscapeIgnoreLine = "&"
        , wrapInsertion = Just WrapInsertion { 
            wrapInsertionIndent = Nothing
          , wrapInsertionStartBegin = "function("
          , wrapInsertionSeparator = ", "
          , wrapInsertionStartClose = "){"
          , wrapInsertionEnd = "}"
          , wrapInsertionAddParens = True
          }
        }
    }
    template  = "unchanged #{var} @{url} ^{int}"
    templateQuote = "unchanged '#{var}' @{url} '^{int}'"
    templateCommented = "unchanged & '#{var}' @{url} '^{int}'"

    run parser str = eShowErrors $ parse parser str str

    eShowErrors :: Either ParseError c -> c
    eShowErrors = either (error . show) id

