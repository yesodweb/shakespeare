# ChangeLog for shakespeare

### 2.2.0

* Added $component with binding for reusable blocks

You can now bind a component-producing function and reuse its sub-components within the same Hamlet block.

#### Example

```haskell
data Modal = Modal
  { modalHeader :: Widget -> Widget
  , modalBody   :: Widget -> Widget
  }

modalWidget :: (Modal -> Widget) -> Widget
```

```hamlet
$component modal <- modalWidget
  $component modalHeader modal
    <h1>This is the title

  $component modalBody modal
    <p>This is the content
```

Conceptually, this desugars to:

```haskell
^{ modalWidget $ \modal ->
     [whamlet|
       ^{ modalHeader modal [whamlet| <h1>This is the title |] }
       ^{ modalBody   modal [whamlet| <p>This is the content |] }
     |]
 }
<p>outside
```

Since everything here is just plain Haskell, you can freely pass additional data or parameters to your component-producing function—`modalWidget` and its subcomponents are ordinary functions.
Only the *outermost* component (`modalWidget` in this case) needs to follow the `(Modal -> Widget) -> Widget` pattern; all nested subcomponents can have arbitrary types and arguments.

### 2.1.7.1

* Add missing other-messages to shakespeare.cabal [#299](https://github.com/yesodweb/shakespeare/pull/299)

### 2.1.7

* Use ByteString’s Lift for Hamlet code generation [#298](https://github.com/yesodweb/shakespeare/pull/298)

### 2.1.6

* Introduce options type for i18n, add ability to stop using record types for messages [#290](https://github.com/yesodweb/shakespeare/pull/290)

### 2.1.5

* Add better support for `mkMessage`ing data types with params.[#295](https://github.com/yesodweb/shakespeare/pull/295)


### 2.1.4

* [#292](https://github.com/yesodweb/shakespeare/pull/292)
    * Add support for multi-line attributes. An example use of this is [here](https://github.com/yesodweb/shakespeare/issues/291).

### 2.1.2

* Add support for context parsing in mkMessage function and related ones [#282](https://github.com/yesodweb/shakespeare/issues/282). Added support for building with LTS versions of 22, 21, 20 and removed older ones.

### 2.1.0

* Add `OverloadedRecordDot`-style record access in expressions

### 2.0.30

* Add `Text.Cassius.Ordered` and `Text.Lucius.Ordered` modules with parsers to maintain order between attributes and mixin blocks.

### 2.0.29

* Support the upcoming `template-haskell` release with GHC 9.4 [#267](https://github.com/yesodweb/shakespeare/pull/267)

### 2.0.28

* Add support for sub-blocks in mixins [#264](https://github.com/yesodweb/shakespeare/pull/264)

### 2.0.27

* Change how embedded templates are located by the compiler. Relative files are now resolved using the Cabal project root, to fix builds of multi-project codebases. [#266](https://github.com/yesodweb/shakespeare/pull/266)
* Change how messages are located by the compiler. The message directory is now resolved using the Cabal project root, to fix builds of multi-project codebases. [#266](https://github.com/yesodweb/shakespeare/pull/266)

### 2.0.26

* Support `@supports` [#263](https://github.com/yesodweb/shakespeare/pull/263)

### 2.0.25.1

* Support for GHC 9.2 and aeson 2 [#260](https://github.com/yesodweb/shakespeare/pull/260)

### 2.0.25

* Support for GHC 9.0 [#254](https://github.com/yesodweb/shakespeare/pull/254)

### 2.0.24.1

* Derive Lift instances wherever possible [#252](https://github.com/yesodweb/shakespeare/pull/252)

### 2.0.24

* Fix build errors with GHC 8.10.1-alpha2 [#245](https://github.com/yesodweb/shakespeare/pull/245)

### 2.0.23

* Add support to use caret interpolation in only var shakespeares [#242](https://github.com/yesodweb/shakespeare/issues/242)

### 2.0.22

* Add `stextFile` to `Text.Shakespeare.Text`, which can be used to produce `Text` directly in the same way `shamletFile` can be used to produce `Html` directly. [#240](https://github.com/yesodweb/shakespeare/pull/240)

### 2.0.21

* Support for GHC 8.8

### 2.0.20

* Restore allowing GHC to detect changes to i18n message files in GHC >= 8.4.

### 2.0.19

* Change of the default behaviour of `*File` functions, they now will add their templates' source file to ghc-dependencies, thus recompiling on templates' changes.

### 2.0.18

* ToJavascript instance for String, Strict and Lazy Text [#227](https://github.com/yesodweb/shakespeare/pull/227)

### 2.0.17

* Fix parse pattern-match with operator constructor [#222](https://github.com/yesodweb/shakespeare/issues/222)

### 2.0.16

* Updated `encodeToTextBuilder` also escapes single quotes [#221](https://github.com/yesodweb/shakespeare/pull/221)

### 2.0.15

* Semigroup instances

### 2.0.14.1

* Handle expressions with infix operator and trailing spaces [#211](https://github.com/yesodweb/shakespeare/issues/211)

### 2.0.14

* Fix Cassius and Lucius reload mode [#206](https://github.com/yesodweb/shakespeare/issues/206)

### 2.0.13

* Expose Text.Internal.Css [#205](https://github.com/yesodweb/shakespeare/pull/205)

### 2.0.12.1

* New contentHash parser breaks hash hrefs [#200](https://github.com/yesodweb/shakespeare/issues/200)

### 2.0.12

* Parser fails without space between class and ID (Hamlet) [#197](https://github.com/yesodweb/shakespeare/issues/197)

### 2.0.11.2

* Support for template-haskell 2.12.0 [#196](https://github.com/yesodweb/shakespeare/pull/196)

### 2.0.11.1

* Handle parsing of trailing semicolon after mixins [#194](https://github.com/yesodweb/shakespeare/issues/194)

### 2.0.11

* Export hamletFromString [#191](https://github.com/yesodweb/shakespeare/pull/191)

### 2.0.10

* Added `ixhamlet` [#177](https://github.com/yesodweb/shakespeare/pull/177)

### 2.0.9

* Better empty HTML tag list

### 2.0.8.1

* Make it work with ghc-8.0 [#181](https://github.com/yesodweb/shakespeare/pull/181)

### 2.0.8

* Improve docs in Text.Hamlet [#180](https://github.com/yesodweb/shakespeare/pull/180)

### 2.0.7

* Include aeson's JSON encoding and escape `<`, `>` and `&` to avoid XSS attacks

### 2.0.6

* Provide the `Text.Hamlet.Runtime` module

### 2.0.5

* Drop system-filepath

### 2.0.4.1

Fix build for GHC 7.10 [#151](https://github.com/yesodweb/shakespeare/pull/151)

### 2.0.4

* [Add multiline literal aligned with bar #148](https://github.com/yesodweb/shakespeare/pull/148)

### 2.0.3

* `cassiusMixin` added

### 2.0.2.2

GHC 7.10 support

### shakesepare 2.0.2

shakespeare-i18n supports message directories.

### Hamlet 0.5.0 (August 29, 2010)

* Use can use parantheses when referencing variables. This allows you to have
  functions applied to multiple arguments.
* Added the hamlet' and xhamlet' quasiquoters for generating plain Html
  values.
* Added runtime Hamlet support.
* Added "file debug" support. This is a mode that is a drop-in replacement for
  external files compiled via template haskell. However, this mode also has a
  runtime component, in that is reads your templates at runtime, thus avoiding
  the need to a recompile for each template change. This takes a runtime hit
  obviously, so it's recommended that you switch back to the compile-time
  templates for production systems.
* Added the Cassius and Julius template languages for CSS and Javascript,
  respectively. The former is white-space sensitive, whereas the latter is just
  a passthrough for raw Javascript code. The big feature in both of them is that
  they support variable interpolation just like Hamlet does.

### New in Hamlet 0.4.0

* Internal template parsing is now done via Parsec. This opened the doors for
  the other changes mentioned below, but also hopefully gives more meaningful
  error messages. There's absolutely no runtime performance hit for this change,
  since all parsing is done at compile time, and if there *is* any compile-time
  hit, it's too negligible to be noticed.
* Attribute values can now be quoted. This allows you to embed spaces, periods
  and pounds in an attribute value. For example:
  [$hamlet|%input!type=submit!value="Add new value"|].
* Space-delimited references in addition to period-delimited ones. This only
  applies to references in content, not in statements. For example, you could
  write [\$hamlet|\$foo bar baz\$|].
* Dollar-sign interpolation is now polymorphic, based on the ToHtml typeclass.
  You can now do away with \$string.var\$ and simply type \$var\$. Currently, the
  ToHtml typeclass is not exposed, and it only provides instances for String and
  Html, though this is open for discussion.
* Added hamletFile and xhamletFile which loads a Hamlet template from an
  external file. The file is parsed at compile time, just like a quasi-quoted
  template, and must be UTF-8 encoded. Additionally, be warned that the compiler
  won't automatically know to recompile a module if the template file gets
  changed.
