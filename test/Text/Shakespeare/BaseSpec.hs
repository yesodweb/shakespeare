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

  it "parseDeref parse single type applications" $ do
    runParser parseDeref () "" "x @y" `shouldBe`
      Right
        (DerefBranch
          (DerefIdent (Ident "x"))
          (DerefType "y"))
  it "parseDeref parse unit type applications" $ do
    runParser parseDeref () "" "x @()" `shouldBe`
      Right
        (DerefBranch
          (DerefIdent (Ident "x"))
          (DerefType "()"))
  it "parseDeref parse compound type applications" $ do
    runParser parseDeref () "" "x @(Maybe String)" `shouldBe`
      Right
        (DerefBranch
          (DerefIdent (Ident "x"))
          (DerefType "Maybe String"))
  it "parseDeref parse single @ as operator" $ do
    runParser parseDeref () "" "x @ y" `shouldBe`
      Right
        (DerefBranch
          (DerefBranch (DerefIdent (Ident "@")) (DerefIdent (Ident "x")))
          (DerefIdent (Ident "y")))

  it "parseDeref parse expressions with record dot" $ do
    runParser parseDeref () "" "x.y" `shouldBe`
      Right (DerefGetField (DerefIdent (Ident "x")) "y")

  it "parseDeref parse expressions with multiple record dots" $ do
    runParser parseDeref () "" "x.y.z" `shouldBe`
      Right (DerefGetField (DerefGetField (DerefIdent (Ident "x")) "y") "z")

  it "parseDeref dot surrounded by whitespace" $ do
    runParser parseDeref () "" "x . y" `shouldBe`
      Right
        (DerefBranch
          (DerefBranch (DerefIdent (Ident ".")) (DerefIdent (Ident "x")))
          (DerefIdent (Ident "y")))

  it "parseDeref parse expressions with parenthesized record dot" $ do
    runParser parseDeref () "" "(x).y" `shouldBe`
      Right (DerefGetField (DerefIdent (Ident "x")) "y")

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

  it "reload" $ do
    let helper input = $(do
            shakespeareFileReload defaultShakespeareSettings
                { toBuilder = VarE 'pack
                , wrap = VarE 'toLazyText
                , unwrap = VarE 'fromLazyText
                } "test/reload.txt") undefined
    helper "here1" `shouldBe` pack "here1\n"
    helper "here2" `shouldBe` pack "here2\n"

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
