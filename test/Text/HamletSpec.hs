{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Text.HamletSpec (spec) where

import HamletTestTypes (ARecord(..))

import Test.HUnit hiding (Test)
import Test.Hspec hiding (Arg)

import Prelude hiding (reverse)
import Text.Hamlet
import Text.Hamlet.RT
import Data.List (intercalate)
import qualified Data.Text.Lazy as T
import qualified Data.List
import qualified Data.List as L
import Data.Text (Text, pack, unpack)
import Data.Monoid (mappend)
import qualified Data.Set as Set
import qualified Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html (toHtml)
import Text.Blaze.Internal (preEscapedString)
import Text.Blaze

data ExampleModal widget = ExampleModal
    { modalHeader :: widget -> widget,
      modalContent :: widget -> widget
    }

spec = do
    it "empty" caseEmpty
    it "static" caseStatic
    it "tag" caseTag
    it "var" caseVar
    it "var chain " caseVarChain
    it "url" caseUrl
    it "url chain " caseUrlChain
    it "embed" caseEmbed
    it "embed chain " caseEmbedChain
    it "if" caseIf
    it "if chain " caseIfChain
    it "else" caseElse
    it "else chain " caseElseChain
    it "elseif" caseElseIf
    it "elseif chain " caseElseIfChain
    it "list" caseList
    it "list chain" caseListChain
    it "with" caseWith
    it "with multi" caseWithMulti
    it "with chain" caseWithChain
    it "with comma string" caseWithCommaString
    it "with multi scope" caseWithMultiBindingScope
    it "script not empty" caseScriptNotEmpty
    it "meta empty" caseMetaEmpty
    it "input empty" caseInputEmpty
    it "multiple classes" caseMultiClass
    it "attrib order" caseAttribOrder
    it "nothing" caseNothing
    it "nothing chain " caseNothingChain
    it "just" caseJust
    it "just chain " caseJustChain
    it "constructor" caseConstructor
    it "url + params" caseUrlParams
    it "escape" caseEscape
    it "empty statement list" caseEmptyStatementList
    it "attribute conditionals" caseAttribCond
    it "non-ascii" caseNonAscii
    it "maybe function" caseMaybeFunction
    it "trailing dollar sign" caseTrailingDollarSign
    it "non leading percent sign" caseNonLeadingPercent
    it "quoted attributes" caseQuotedAttribs
    it "spaced derefs" caseSpacedDerefs
    it "attrib vars" caseAttribVars
    it "strings and html" caseStringsAndHtml
    it "nesting" caseNesting
    it "trailing space" caseTrailingSpace
    it "currency symbols" caseCurrency
    it "external" caseExternal
    it "parens" caseParens
    it "hamlet literals" caseHamletLiterals
    it "hamlet' and xhamlet'" caseHamlet'
    it "hamlet tuple" caseTuple
    it "complex pattern" caseComplex
    it "record pattern" caseRecord
    it "record wildcard pattern #1" caseRecordWildCard
    it "record wildcard pattern #2" caseRecordWildCard1



    it "comments" $ do
    -- FIXME reconsider Hamlet comment syntax?
      helper "" [hamlet|$# this is a comment
$# another comment
$#a third one|]


    it "ignores a blank line" $ do
      helper "<p>foo</p>\n" [hamlet|
<p>

    foo


|]




    it "angle bracket syntax" $
      helper "<p class=\"foo\" height=\"100\"><span id=\"bar\" width=\"50\">HELLO</span></p>"
        [hamlet|
$newline never
<p.foo height="100">
    <span #bar width=50>HELLO
|]



    it "hamlet module names" $ do
      let foo = "foo"
      helper "oof oof 3.14 -5"
        [hamlet|
$newline never
#{Data.List.reverse foo} #
#{L.reverse foo} #
#{show 3.14} #{show -5}|]





    it "single dollar at and caret" $ do
      helper "$@^" [hamlet|\$@^|]

      helper "#{@{^{" [hamlet|#\{@\{^\{|]


    it "dollar operator" $ do
      let val = (1, (2, 3))
      helper "2" [hamlet|#{ show $ fst $ snd val }|]
      helper "2" [hamlet|#{ show $ fst $ snd $ val}|]


    it "in a row" $ do
      helper "1" [hamlet|#{ show $ const 1 2 }|]


    it "embedded slash" $ do
      helper "///" [hamlet|///|]

{- compile-time error
    it "tag with slash" $ do
    helper "" [hamlet|
<p>
  Text
</p>
|]
-}

    it "string literals" $ do
      helper "string" [hamlet|#{"string"}|]
      helper "string" [hamlet|#{id "string"}|]
      helper "gnirts" [hamlet|#{L.reverse $ id "string"}|]
      helper "str&quot;ing" [hamlet|#{"str\"ing"}|]
      helper "str&lt;ing" [hamlet|#{"str<ing"}|]


    it "interpolated operators" $ do
      helper "3" [hamlet|#{show $ (+) 1 2}|]
      helper "6" [hamlet|#{show $ sum $ (:) 1 ((:) 2 $ return 3)}|]


    it "HTML comments" $ do
      helper "<p>1</p><p>2 not ignored</p>" [hamlet|
$newline never
<p>1
<!-- ignored comment -->
<p>
    2
    <!-- ignored --> not ignored<!-- ignored -->
|]

    it "Keeps SSI includes" $
      helper "<!--# SSI -->" [hamlet|<!--# SSI -->|]



    it "nested maybes" $ do
      let muser = Just "User" :: Maybe String
          mprof = Nothing :: Maybe Int
          m3 = Nothing :: Maybe String
      helper "justnothing" [hamlet|
$maybe user <- muser
    $maybe profile <- mprof
        First two are Just
        $maybe desc <- m3
            \ and left us a description:
            <p>#{desc}
        $nothing
        and has left us no description.
    $nothing
        justnothing
$nothing
    <h1>No such Person exists.
   |]


    it "maybe with qualified constructor" $ do
        helper "5" [hamlet|
            $maybe HamletTestTypes.ARecord x y <- Just $ ARecord 5 True
                \#{x}
        |]

    it "record with qualified constructor" $ do
        helper "5" [hamlet|
            $maybe HamletTestTypes.ARecord {..} <- Just $ ARecord 5 True
                \#{field1}
        |]




    it "conditional class" $ do
      helper "<p class=\"current\"></p>\n"
        [hamlet|<p :False:.ignored :True:.current>|]

      helper "<p class=\"1 3 2 4\"></p>\n"
        [hamlet|<p :True:.1 :True:class=2 :False:.a :False:class=b .3 class=4>|]

      helper "<p class=\"foo bar baz\"></p>\n"
        [hamlet|<p class=foo class=bar class=baz>|]




    it "forall on Foldable" $ do
      let set = Set.fromList [1..5 :: Int]
      helper "12345" [hamlet|
$forall x <- set
  #{x}
|]



    it "non-poly HTML" $ do
      helperHtml "<h1>HELLO WORLD</h1>\n" [shamlet|
  <h1>HELLO WORLD
  |]
      helperHtml "<h1>HELLO WORLD</h1>\n" $(shamletFile "test/hamlets/nonpolyhtml.hamlet")
      helper "<h1>HELLO WORLD</h1>\n" $(hamletFileReload "test/hamlets/nonpolyhtml.hamlet")


    it "non-poly Hamlet" $ do
      let embed = [hamlet|<p>EMBEDDED|]
      helper "<h1>url</h1>\n<p>EMBEDDED</p>\n" [hamlet|
  <h1>@{Home}
  ^{embed}
  |]
      helper "<h1>url</h1>\n" $(hamletFile "test/hamlets/nonpolyhamlet.hamlet")
      helper "<h1>url</h1>\n" $(hamletFileReload "test/hamlets/nonpolyhamlet.hamlet")

    it "non-poly IHamlet" $ do
      let embed = [ihamlet|<p>EMBEDDED|]
      ihelper "<h1>Adios</h1>\n<p>EMBEDDED</p>\n" [ihamlet|
  <h1>_{Goodbye}
  ^{embed}
  |]
      ihelper "<h1>Hola</h1>\n" $(ihamletFile "test/hamlets/nonpolyihamlet.hamlet")
      ihelper "<h1>Hola</h1>\n" $(ihamletFileReload "test/hamlets/nonpolyihamlet.hamlet")

    it "pattern-match tuples: forall" $ do
      let people = [("Michael", 26), ("Miriam", 25)]
      helper "<dl><dt>Michael</dt><dd>26</dd><dt>Miriam</dt><dd>25</dd></dl>" [hamlet|
$newline never
<dl>
    $forall (name, age) <- people
        <dt>#{name}
        <dd>#{show age}
|]
    it "pattern-match tuples: maybe" $ do
      let people = Just ("Michael", 26)
      helper "<dl><dt>Michael</dt><dd>26</dd></dl>" [hamlet|
$newline never
<dl>
    $maybe (name, age) <- people
        <dt>#{name}
        <dd>#{show age}
|]
    it "pattern-match tuples: with" $ do
      let people = ("Michael", 26)
      helper "<dl><dt>Michael</dt><dd>26</dd></dl>" [hamlet|
$newline never
<dl>
    $with (name, age) <- people
        <dt>#{name}
        <dd>#{show age}
|]
    it "list syntax for interpolation" $ do
      helper "<ul><li>1</li><li>2</li><li>3</li></ul>" [hamlet|
$newline never
<ul>
    $forall num <- [1, 2, 3]
        <li>#{show num}
|]
    it "pattern-match list: maybe" $ do
      let example :: [Int]
          example = [1, 2, 3]
      helper "<ul><li>1</li><li>2</li><li>3</li></ul>" [hamlet|
$newline never
<ul>
    $case example
        $of (:) x xs
            <li>#{x}
            $forall y <- xs
                <li>#{y}
|]
    it "infix operators" $
      helper "5" [hamlet|#{show $ (4 + 5) - (2 + 2)}|]
    it "infix operators with parens" $
      helper "5" [hamlet|#{show ((+) 1 1 + 3)}|]
    it "doctypes" $ helper "<!DOCTYPE html>\n<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n" [hamlet|
$newline never
$doctype 5
$doctype strict
|]

    it "case on Maybe" $
      let nothing  = Nothing
          justTrue = Just True
      in helper "<br><br><br><br>" [hamlet|
$newline never
$case nothing
    $of Just val
    $of Nothing
        <br>
$case justTrue
    $of Just val
        $if val
            <br>
    $of Nothing
$case (Just $ not False)
    $of Nothing
    $of Just val
        $if val
            <br>
$case Nothing
    $of Just val
    $of _
        <br>
|]

    it "case on Url" $
      let url1 = Home
          url2 = Sub SubUrl
      in helper "<br>\n<br>\n" [hamlet|
$newline always
$case url1
    $of Home
        <br>
    $of _
$case url2
    $of Sub sub
        $case sub
            $of SubUrl
                <br>
    $of Home
|]

    it "pattern-match constructors: forall" $ do
      let people = [Pair "Michael" 26, Pair "Miriam" 25]
      helper "<dl><dt>Michael</dt><dd>26</dd><dt>Miriam</dt><dd>25</dd></dl>" [hamlet|
$newline text
<dl>
    $forall Pair name age <- people
        <dt>#{name}
        <dd>#{show age}
|]
    it "pattern-match constructors: maybe" $ do
      let people = Just $ Pair "Michael" 26
      helper "<dl><dt>Michael</dt><dd>26</dd></dl>" [hamlet|
$newline text
<dl>
    $maybe Pair name age <- people
        <dt>#{name}
        <dd>#{show age}
|]
    it "pattern-match constructors: with" $ do
      let people = Pair "Michael" 26
      helper "<dl><dt>Michael</dt><dd>26</dd></dl>" [hamlet|
$newline text
<dl>
    $with Pair name age <- people
        <dt>#{name}
        <dd>#{show age}
|]

    it "multiline tags" $ helper
      "<foo bar=\"baz\" bin=\"bin\">content</foo>\n" [hamlet|
<foo bar=baz
     bin=bin>content
|]
    it "*{...} attributes" $
      let attrs = [("bar", "baz"), ("bin", "<>\"&")] in helper
      "<foo bar=\"baz\" bin=\"&lt;&gt;&quot;&amp;\">content</foo>\n" [hamlet|
<foo *{attrs}>content
|]
    it "blank attr values" $ helper
      "<foo bar=\"\" baz bin=\"\"></foo>\n"
      [hamlet|<foo bar="" baz bin=>|]
    it "greater than in attr" $ helper
      "<button data-bind=\"enable: someFunction() > 5\">hello</button>\n"
      [hamlet|<button data-bind="enable: someFunction() > 5">hello|]
    it "normal doctype" $ helper
      "<!DOCTYPE html>\n"
      [hamlet|<!DOCTYPE html>|]
    it "newline style" $ helper
      "<p>foo</p>\n<pre>bar\nbaz\nbin</pre>\n"
      [hamlet|
$newline always
<p>foo
<pre>
    bar
    baz
    bin
|]
    it "avoid newlines" $ helper
      "<p>foo</p><pre>barbazbin</pre>"
      [hamlet|
$newline always
<p>foo#
<pre>#
    bar#
    baz#
    bin#
|]
    it "manual linebreaks" $ helper
      "<p>foo</p><pre>bar\nbaz\nbin</pre>"
      [hamlet|
$newline never
<p>foo
<pre>
    bar
    \
    baz
    \
    bin
|]
    it "indented newline" $ helper
      "<p>foo</p><pre>bar\nbaz\nbin</pre>"
      [hamlet|
    $newline never
    <p>foo
    <pre>
        bar
        \
        baz
        \
        bin
|]
    it "case underscore" $
        let num = 3
         in helper "<p>Many</p>\n" [hamlet|
$case num
    $of 1
        <p>1
    $of 2
        <p>2
    $of _
        <p>Many
|]
    it "optional and missing classes" $
        helper "<i>foo</i>\n" [hamlet|<i :False:.not-present>foo|]
    it "multiple optional and missing classes" $
        helper "<i>foo</i>\n" [hamlet|<i :False:.not-present :False:.also-not-here>foo|]
    it "optional and present classes" $
        helper "<i class=\"present\">foo</i>\n" [hamlet|<i :False:.not-present :True:.present>foo|]
    it "punctuation operators #115" $
        helper "foo"
            [hamlet|
                $if True && True
                    foo
                $else
                    bar
            |]

    it "list syntax" $
        helper "123"
            [hamlet|
                $forall x <- []
                    ignored
                $forall x <- [1, 2, 3]
                    #{show x}
            |]

    it "list in attribute" $
        let myShow :: [Int] -> String
            myShow = show
         in helper "<a href=\"[]\">foo</a>\n<a href=\"[1,2]\">bar</a>\n"
            [hamlet|
                <a href=#{myShow []}>foo
                <a href=#{myShow [1, 2]}>bar
            |]

    it "AngularJS attribute values #122" $
        helper "<li ng-repeat=\"addr in msgForm.new.split(/\\\\s/)\">{{addr}}</li>\n"
            [hamlet|<li ng-repeat="addr in msgForm.new.split(/\\s/)">{{addr}}|]

    it "Alpine.js multi-line attribute values #291" $
        helper "<div x-data=\"{\r\n                        search: '',\r\n\r\n                        items: ['foo', 'bar', 'baz'],\r\n\r\n                        get filteredItems() {\r\n                            return this.items.filter(\r\n                                i => i.startsWith(this.search)\r\n                            )\r\n                        }\r\n                    }\"></div>"
            [hamlet|
                $newline never
                <div
                    x-data="{
                        search: '',

                        items: ['foo', 'bar', 'baz'],

                        get filteredItems() {
                            return this.items.filter(
                                i => i.startsWith(this.search)
                            )
                        }
                    }"
                >
            |]

    it "supports $component without binding" $
        let
            container :: String -> HtmlUrl url -> HtmlUrl url
            container clazz x =
                [hamlet|
                    $newline never
                    <div class="container #{clazz}">
                        ^{x}
                |]
        in
            helper "<div class=\"container alert\"><p>Hello world</p></div><p>outside</p>"
                [hamlet|
                    $newline never
                    $component container "alert"
                      <p>Hello world
                    <p>outside
                |]

    it "supports $component with binding (modal example)" $
        let
            modalWidget :: (ExampleModal (HtmlUrl url) -> HtmlUrl url) -> HtmlUrl url
            modalWidget body =
              let
                exampleModal =
                    ExampleModal
                      { modalHeader = \content ->
                            [hamlet|
                                $newline never
                                <div class="modal-header">
                                    ^{content}
                            |],
                        modalContent = \content ->
                            [hamlet|
                                $newline never
                                <div class="modal-content">
                                    ^{content}
                            |]
                      }
              in
                [hamlet|
                    $newline never
                    <div class="modal">
                        ^{body exampleModal}
                |]
        in
            helper "<div class=\"modal\"><div class=\"modal-header\"><h1>This is the title</h1></div><div class=\"modal-content\"><p>This is the content</p></div></div><p>outside</p>"
                [hamlet|
                    $newline never
                    $component modal <- modalWidget
                        $component modalHeader modal
                          <h1>This is the title

                        $component modalContent modal
                          <p>This is the content
                    <p>outside
                |]

    it "runtime Hamlet with caret interpolation" $ do
        let toInclude render = render (5, [("hello", "world")])
        let renderer x y = pack $ show (x :: Int, y :: [(Text, Text)])
            template1 = "@?{url}"
            template2 = "foo^{toInclude}bar"
        toInclude <- parseHamletRT defaultHamletSettings template1
        hamletRT <- parseHamletRT defaultHamletSettings template2
        res <- renderHamletRT hamletRT
            [ (["toInclude"], HDTemplate toInclude)
            , (["url"], HDUrlParams 5 [(pack "hello", pack "world")])
            ] renderer
        helperHtml "foo(5,[(\"hello\",\"world\")])bar" res

    it "Hash in attribute value" $
        helper "<a id=\"logoutbutton\" href=\"#\"></a>\n"
        [hamlet|<a #logoutbutton href=#>|]

    it "Hash in attribute value" $
        helper "<a id=\"logoutbutton\" href=\"#\"></a>\n"
        [hamlet|<a #logoutbutton href="#">|]

data Pair = Pair String Int

data Url = Home | Sub SubUrl
data SubUrl = SubUrl
render :: Url -> [(Text, Text)] -> Text
render Home qs = pack "url" `mappend` showParams qs
render (Sub SubUrl) qs = pack "suburl" `mappend` showParams qs

showParams :: [(Text, Text)] -> Text
showParams [] = pack ""
showParams z =
    pack $ '?' : intercalate "&" (map go z)
  where
    go (x, y) = go' x ++ '=' : go' y
    go' = concatMap encodeUrlChar . unpack

-- | Taken straight from web-encodings; reimplemented here to avoid extra
-- dependencies.
encodeUrlChar :: Char -> String
encodeUrlChar c
    -- List of unreserved characters per RFC 3986
    -- Gleaned from http://en.wikipedia.org/wiki/Percent-encoding
    | 'A' <= c && c <= 'Z' = [c]
    | 'a' <= c && c <= 'z' = [c]
    | '0' <= c && c <= '9' = [c]
encodeUrlChar c@'-' = [c]
encodeUrlChar c@'_' = [c]
encodeUrlChar c@'.' = [c]
encodeUrlChar c@'~' = [c]
encodeUrlChar ' ' = "+"
encodeUrlChar y =
    let (a, c) = fromEnum y `divMod` 16
        b = a `mod` 16
        showHex' x
            | x < 10 = toEnum $ x + (fromEnum '0')
            | x < 16 = toEnum $ x - 10 + (fromEnum 'A')
            | otherwise = error $ "Invalid argument to showHex: " ++ show x
     in ['%', showHex' b, showHex' c]

data Arg url = Arg
    { getArg :: Arg url
    , var :: Html
    , url :: Url
    , embed :: HtmlUrl url
    , true :: Bool
    , false :: Bool
    , list :: [Arg url]
    , nothing :: Maybe String
    , just :: Maybe String
    , urlParams :: (Url, [(Text, Text)])
    }

theArg :: Arg url
theArg = Arg
    { getArg = theArg
    , var = toHtml "<var>"
    , url = Home
    , embed = [hamlet|embed|]
    , true = True
    , false = False
    , list = [theArg, theArg, theArg]
    , nothing = Nothing
    , just = Just "just"
    , urlParams = (Home, [(pack "foo", pack "bar"), (pack "foo1", pack "bar1")])
    }

helperHtml :: String -> Html -> Assertion
helperHtml res h = do
    let x = Text.Blaze.Html.Renderer.Text.renderHtml h
    T.pack res @=? x

helper :: String -> HtmlUrl Url -> Assertion
helper res h = do
    let x = Text.Blaze.Html.Renderer.Text.renderHtml $ h render
    T.pack res @=? x

caseEmpty :: Assertion
caseEmpty = helper "" [hamlet||]

caseStatic :: Assertion
caseStatic = helper "some static content" [hamlet|some static content|]

caseTag :: Assertion
caseTag = do
    helper "<p class=\"foo\"><div id=\"bar\">baz</div></p>" [hamlet|
$newline text
<p .foo>
  <#bar>baz
|]
    helper "<p class=\"foo.bar\"><div id=\"bar\">baz</div></p>" [hamlet|
$newline text
<p class=foo.bar>
  <#bar>baz
|]

caseVar :: Assertion
caseVar = do
    helper "&lt;var&gt;" [hamlet|#{var theArg}|]

caseVarChain :: Assertion
caseVarChain = do
    helper "&lt;var&gt;" [hamlet|#{var (getArg (getArg (getArg theArg)))}|]

caseUrl :: Assertion
caseUrl = do
    helper (unpack $ render Home []) [hamlet|@{url theArg}|]

caseUrlChain :: Assertion
caseUrlChain = do
    helper (unpack $ render Home []) [hamlet|@{url (getArg (getArg (getArg theArg)))}|]

caseEmbed :: Assertion
caseEmbed = do
    helper "embed" [hamlet|^{embed theArg}|]
    helper "embed" $(hamletFileReload "test/hamlets/embed.hamlet")
    ihelper "embed" $(ihamletFileReload "test/hamlets/embed.hamlet")

caseEmbedChain :: Assertion
caseEmbedChain = do
    helper "embed" [hamlet|^{embed (getArg (getArg (getArg theArg)))}|]

caseIf :: Assertion
caseIf = do
    helper "if" [hamlet|
$if true theArg
    if
|]

caseIfChain :: Assertion
caseIfChain = do
    helper "if" [hamlet|
$if true (getArg (getArg (getArg theArg)))
    if
|]

caseElse :: Assertion
caseElse = helper "else" [hamlet|
$if false theArg
    if
$else
    else
|]

caseElseChain :: Assertion
caseElseChain = helper "else" [hamlet|
$if false (getArg (getArg (getArg theArg)))
    if
$else
    else
|]

caseElseIf :: Assertion
caseElseIf = helper "elseif" [hamlet|
$if false theArg
    if
$elseif true theArg
    elseif
$else
    else
|]

caseElseIfChain :: Assertion
caseElseIfChain = helper "elseif" [hamlet|
$if false(getArg(getArg(getArg theArg)))
    if
$elseif true(getArg(getArg(getArg theArg)))
    elseif
$else
    else
|]

caseList :: Assertion
caseList = do
    helper "xxx" [hamlet|
$forall _x <- (list theArg)
    x
|]

caseListChain :: Assertion
caseListChain = do
    helper "urlurlurl" [hamlet|
$forall x <-  list(getArg(getArg(getArg(getArg(getArg (theArg))))))
    @{url x}
|]

caseWith :: Assertion
caseWith = do
    helper "it's embedded" [hamlet|
$with n <- embed theArg
    it's ^{n}ded
|]

caseWithMulti :: Assertion
caseWithMulti = do
    helper "it's embedded" [hamlet|
$with n <- embed theArg, m <- true theArg
    $if m
        it's ^{n}ded
|]

caseWithChain :: Assertion
caseWithChain = do
    helper "it's true" [hamlet|
$with n <- true(getArg(getArg(getArg(getArg theArg))))
    $if n
        it's true
|]

-- in multi-with binding, make sure that a comma in a string doesn't confuse the parser.
caseWithCommaString :: Assertion
caseWithCommaString = do
    helper "it's  , something" [hamlet|
$with n <- " , something"
    it's #{n}
|]

caseWithMultiBindingScope :: Assertion
caseWithMultiBindingScope = do
    helper "it's  , something" [hamlet|
$with n <- " , something", y <- n
    it's #{y}
|]

caseScriptNotEmpty :: Assertion
caseScriptNotEmpty = helper "<script></script>\n" [hamlet|<script>|]

caseMetaEmpty :: Assertion
caseMetaEmpty = do
    helper "<meta>\n" [hamlet|<meta>|]
    helper "<meta/>\n" [xhamlet|<meta>|]

caseInputEmpty :: Assertion
caseInputEmpty = do
    helper "<input>\n" [hamlet|<input>|]
    helper "<input/>\n" [xhamlet|<input>|]

caseMultiClass :: Assertion
caseMultiClass = helper "<div class=\"foo bar\"></div>\n" [hamlet|<.foo.bar>|]

caseAttribOrder :: Assertion
caseAttribOrder =
    helper "<meta 1 2 3>\n" [hamlet|<meta 1 2 3>|]

caseNothing :: Assertion
caseNothing = do
    helper "" [hamlet|
$maybe _n <- nothing theArg
    nothing
|]
    helper "nothing" [hamlet|
$maybe _n <- nothing theArg
    something
$nothing
    nothing
|]

caseNothingChain :: Assertion
caseNothingChain = helper "" [hamlet|
$maybe n <- nothing(getArg(getArg(getArg theArg)))
    nothing #{n}
|]

caseJust :: Assertion
caseJust = helper "it's just" [hamlet|
$maybe n <- just theArg
    it's #{n}
|]

caseJustChain :: Assertion
caseJustChain = helper "it's just" [hamlet|
$maybe n <- just(getArg(getArg(getArg theArg)))
    it's #{n}
|]

caseConstructor :: Assertion
caseConstructor = do
    helper "url" [hamlet|@{Home}|]
    helper "suburl" [hamlet|@{Sub SubUrl}|]
    let text = "<raw text>"
    helper "<raw text>" [hamlet|#{preEscapedString text}|]

caseUrlParams :: Assertion
caseUrlParams = do
    helper "url?foo=bar&amp;foo1=bar1" [hamlet|@?{urlParams theArg}|]

caseEscape :: Assertion
caseEscape = do
    helper "#this is raw\n " [hamlet|
$newline never
\#this is raw
\
\ 
|]
    helper "$@^" [hamlet|\$@^|]

caseEmptyStatementList :: Assertion
caseEmptyStatementList = do
    helper "" [hamlet|$if True|]
    helper "" [hamlet|$maybe _x <- Nothing|]
    let emptyList = []
    helper "" [hamlet|$forall _x <- emptyList|]

caseAttribCond :: Assertion
caseAttribCond = do
    helper "<select></select>\n" [hamlet|<select :False:selected>|]
    helper "<select selected></select>\n" [hamlet|<select :True:selected>|]
    helper "<meta var=\"foo:bar\">\n" [hamlet|<meta var=foo:bar>|]
    helper "<select selected></select>\n"
        [hamlet|<select :true theArg:selected>|]

    helper "<select></select>\n" [hamlet|<select :False:selected>|]
    helper "<select selected></select>\n" [hamlet|<select :True:selected>|]
    helper "<meta var=\"foo:bar\">\n" [hamlet|<meta var=foo:bar>|]
    helper "<select selected></select>\n"
        [hamlet|<select :true theArg:selected>|]

caseNonAscii :: Assertion
caseNonAscii = do
    helper "עִבְרִי" [hamlet|עִבְרִי|]

caseMaybeFunction :: Assertion
caseMaybeFunction = do
    helper "url?foo=bar&amp;foo1=bar1" [hamlet|
$maybe x <- Just urlParams
    @?{x theArg}
|]

caseTrailingDollarSign :: Assertion
caseTrailingDollarSign =
    helper "trailing space \ndollar sign #" [hamlet|
$newline never
trailing space #
\
dollar sign #\
|]

caseNonLeadingPercent :: Assertion
caseNonLeadingPercent =
    helper "<span style=\"height:100%\">foo</span>" [hamlet|
$newline never
<span style=height:100%>foo
|]

caseQuotedAttribs :: Assertion
caseQuotedAttribs =
    helper "<input type=\"submit\" value=\"Submit response\">" [hamlet|
$newline never
<input type=submit value="Submit response">
|]

caseSpacedDerefs :: Assertion
caseSpacedDerefs = do
    helper "&lt;var&gt;" [hamlet|#{var theArg}|]
    helper "<div class=\"&lt;var&gt;\"></div>\n" [hamlet|<.#{var theArg}>|]

caseAttribVars :: Assertion
caseAttribVars = do
    helper "<div id=\"&lt;var&gt;\"></div>\n" [hamlet|<##{var theArg}>|]
    helper "<div class=\"&lt;var&gt;\"></div>\n" [hamlet|<.#{var theArg}>|]
    helper "<div f=\"&lt;var&gt;\"></div>\n" [hamlet|< f=#{var theArg}>|]

    helper "<div id=\"&lt;var&gt;\"></div>\n" [hamlet|<##{var theArg}>|]
    helper "<div class=\"&lt;var&gt;\"></div>\n" [hamlet|<.#{var theArg}>|]
    helper "<div f=\"&lt;var&gt;\"></div>\n" [hamlet|< f=#{var theArg}>|]

caseStringsAndHtml :: Assertion
caseStringsAndHtml = do
    let str = "<string>"
    let html = preEscapedString "<html>"
    helper "&lt;string&gt; <html>" [hamlet|#{str} #{html}|]

caseNesting :: Assertion
caseNesting = do
    helper
      "<table><tbody><tr><td>1</td></tr><tr><td>2</td></tr></tbody></table>"
      [hamlet|
$newline never
<table>
  <tbody>
    $forall user <- users
        <tr>
         <td>#{user}
|]
    helper
        (concat
          [ "<select id=\"foo\" name=\"foo\"><option selected></option>"
          , "<option value=\"true\">Yes</option>"
          , "<option value=\"false\">No</option>"
          , "</select>"
          ])
        [hamlet|
$newline never
<select #"#{name}" name=#{name}>
    <option :isBoolBlank val:selected>
    <option value=true :isBoolTrue val:selected>Yes
    <option value=false :isBoolFalse val:selected>No
|]
  where
    users = ["1", "2"]
    name = "foo"
    val = 5 :: Int
    isBoolBlank _ = True
    isBoolTrue _ = False
    isBoolFalse _ = False

caseTrailingSpace :: Assertion
caseTrailingSpace =
    helper "" [hamlet|        |]

caseCurrency :: Assertion
caseCurrency =
    helper foo [hamlet|#{foo}|]
  where
    foo = "eg: 5, $6, €7.01, £75"

caseExternal :: Assertion
caseExternal = do
    helper "foo\n<br>\n" $(hamletFile "test/hamlets/external.hamlet")
    helper "foo\n<br/>\n" $(xhamletFile "test/hamlets/external.hamlet")
    helper "foo\n<br>\n" $(hamletFileReload "test/hamlets/external.hamlet")
  where
    foo = "foo"

caseParens :: Assertion
caseParens = do
    let plus = (++)
        x = "x"
        y = "y"
    helper "xy" [hamlet|#{(plus x) y}|]
    helper "xxy" [hamlet|#{plus (plus x x) y}|]
    let alist = ["1", "2", "3"]
    helper "123" [hamlet|
$forall x <- (id id id id alist)
    #{x}
|]

caseHamletLiterals :: Assertion
caseHamletLiterals = do
    helper "123" [hamlet|#{show 123}|]
    helper "123.456" [hamlet|#{show 123.456}|]
    helper "-123" [hamlet|#{show -123}|]
    helper "-123.456" [hamlet|#{show -123.456}|]

helper' :: String -> Html -> Assertion
helper' res h = T.pack res @=? Text.Blaze.Html.Renderer.Text.renderHtml h

caseHamlet' :: Assertion
caseHamlet' = do
    helper' "foo" [shamlet|foo|]
    helper' "foo" [xshamlet|foo|]
    helper "<br>\n" $ const $ [shamlet|<br>|]
    helper "<br/>\n" $ const $ [xshamlet|<br>|]

    -- new with generalized stuff
    helper' "foo" [shamlet|foo|]
    helper' "foo" [xshamlet|foo|]
    helper "<br>\n" $ const $ [shamlet|<br>|]
    helper "<br/>\n" $ const $ [xshamlet|<br>|]


instance Show Url where
    show _ = "FIXME remove this instance show Url"

caseDiffBindNames :: Assertion
caseDiffBindNames = do
    let list = words "1 2 3"
    -- FIXME helper "123123" $(hamletFileReload "test/hamlets/external-debug3.hamlet")
    error "test has been disabled"


caseTrailingSpaces :: Assertion
caseTrailingSpaces = helper "" [hamlet|
$if   True   
$elseif   False   
$else   
$maybe x <-   Nothing    
$nothing  
$forall   x     <-   empty       
|]
  where
    empty = []



caseTuple :: Assertion
caseTuple = do
   helper "(1,1)" [hamlet| #{("1","1")}|]
   helper "foo" [hamlet| 
    $with (a,b) <- ("foo","bar")
      #{a}
   |]
   helper "url" [hamlet| 
    $with (a,b) <- (Home,Home)
      @{a}
   |]
   helper "url" [hamlet| 
    $with p <- (Home,[])
      @?{p}
   |]



caseComplex :: Assertion
caseComplex = do
  let z :: ((Int,Int),Maybe Int,(),Bool,[[Int]])
      z = ((1,2),Just 3,(),True,[[4],[5,6]])
  helper "1 2 3 4 5 61 2 3 4 5 6" [hamlet|
    $with ((a,b),Just c, () ,True,d@[[e],[f,g]]) <- z
      $forall h <- d
        #{a} #{b} #{c} #{e} #{f} #{g}
    |]

caseRecord :: Assertion
caseRecord = do
  let z = ARecord 10 True
  helper "10" [hamlet|
    $with ARecord { field1, field2 = True } <- z
        #{field1}
    |]

caseRecordWildCard :: Assertion
caseRecordWildCard = do
  let z = ARecord 10 True
  helper "10 True" [hamlet|
    $with ARecord {..} <- z
        #{field1} #{field2}
    |]

caseRecordWildCard1 :: Assertion
caseRecordWildCard1 = do
  let z = ARecord 10 True
  helper "10" [hamlet|
    $with ARecord {field2 = True, ..} <- z
        #{field1}
    |]

caseCaseRecord :: Assertion
caseCaseRecord = do
  let z = ARecord 10 True
  helper "10\nTrue" [hamlet|
    $case z
      $of ARecord { field1, field2 = x }
        #{field1}
        #{x}
    |]

data Msg = Hello | Goodbye

ihelper :: String -> HtmlUrlI18n Msg Url -> Assertion
ihelper res h = do
    let x = Text.Blaze.Html.Renderer.Text.renderHtml $ h showMsg render
    T.pack res @=? x
  where
    showMsg Hello = preEscapedString "Hola"
    showMsg Goodbye = preEscapedString "Adios"

instance (ToMarkup a, ToMarkup b) => ToMarkup (a,b) where
  toMarkup (a,b) = do
    toMarkup "("
    toMarkup a
    toMarkup ","
    toMarkup b
    toMarkup ")"
