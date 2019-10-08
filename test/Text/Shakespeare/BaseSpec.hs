{-# LANGUAGE TemplateHaskell #-}
module Text.Shakespeare.BaseSpec (spec) where

import Test.Hspec
import Text.Shakespeare
import Text.ParserCombinators.Parsec
       ((<|>), ParseError, parse, runParser)
import Text.Shakespeare.Base
       (Deref(..), Ident(..), parseDeref, parseVarString, parseUrlString,
        parseIntString)
import Language.Haskell.TH.Syntax (Exp (VarE))
import Data.Text.Lazy.Builder (toLazyText, fromLazyText)
import Data.Text.Lazy (pack)

-- run :: Text.Parsec.Prim.Parsec Text.Parsec.Pos.SourceName () c -> Text.Parsec.Pos.SourceName -> c

partialShakespeareSettings = defaultShakespeareSettings unused unused unused
  where
    unused = error "Unused setting"

spec :: Spec
spec = do
  let preFilterN = preFilter Nothing
  {-
  it "parseStrings" $ do
    run varString "%{var}" `shouldBe` Right "%{var}"
    run urlString "@{url}" `shouldBe` Right "@{url}"
    run intString "^{int}" `shouldBe` Right "^{int}"

    run (varString <|> urlString <|> intString) "@{url} #{var}" `shouldBe` Right "@{url}"
  -}

  it "parseDeref parse expressions with infix operator and trailing spaces" $ do
    runParser parseDeref () "" " a + b \t " `shouldBe`
      (Right
         (DerefBranch
            (DerefBranch (DerefIdent (Ident "+")) (DerefIdent (Ident "a")))
            (DerefIdent (Ident "b"))))

  it "preFilter off" $ do
    preFilterN partialShakespeareSettings template
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

  it "reload" $ do
    let helper input = $(do
          shakespeareFileReload
            (defaultShakespeareSettings (VarE 'pack)
                                        (VarE 'toLazyText)
                                        (VarE 'fromLazyText))
            "test/reload.txt") undefined
    helper "here1" `shouldBe` pack "here1\n"
    helper "here2" `shouldBe` pack "here2\n"

  where
    varString = parseVarString '%'
    urlString = parseUrlString '@' '?'
    intString = parseIntString '^'

    preConversionSettings = partialShakespeareSettings {
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

