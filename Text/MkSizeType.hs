-- | Internal functions to generate CSS size wrapper types.
module Text.MkSizeType (mkSizeType) where

import Language.Haskell.TH.Syntax

mkSizeType :: String -> String -> Q [Dec]
mkSizeType name' unit = return [ dataDec name
                               , showInstanceDec name unit
                               , numInstanceDec name
                               , fractionalInstanceDec name
                               , toCssInstanceDec name ]
  where name = mkName $ name'

dataDec :: Name -> Dec
dataDec name = DataD [] name [] [constructor] derives
  where constructor = NormalC name [(NotStrict, ConT $ mkName "Rational")]
        derives = map mkName ["Eq", "Ord"]

showInstanceDec :: Name -> String -> Dec
showInstanceDec name unit' = InstanceD [] (instanceType "Show" name) [showDec]
  where showSize = VarE $ mkName "showSize"
        x = mkName "x"
        unit = LitE $ StringL unit'
        showDec = FunD (mkName "show") [Clause [showPat] showBody []]
        showPat = ConP name [VarP x]
        showBody = NormalB $ AppE (AppE showSize $ VarE x) unit

numInstanceDec :: Name -> Dec
numInstanceDec name = InstanceD [] (instanceType "Num" name) decs
  where decs = map (binaryFunDec name) ["+", "*", "-"] ++
               map (unariFunDec1 name) ["abs", "signum"] ++
               [unariFunDec2 name "fromInteger"]

fractionalInstanceDec :: Name -> Dec
fractionalInstanceDec name = InstanceD [] (instanceType "Fractional" name) decs
  where decs = [binaryFunDec name "/", unariFunDec2 name "fromRational"]

toCssInstanceDec :: Name -> Dec
toCssInstanceDec name = InstanceD [] (instanceType "ToCss" name) [toCssDec]
  where toCssDec = FunD (mkName "toCss") [Clause [] showBody []]
        showBody = NormalB $ AppE (AppE dot pack) show
        pack = VarE (mkName "TL.pack")
        dot = VarE (mkName ".")
        show = VarE (mkName "show")

instanceType :: String -> Name -> Type
instanceType className name = AppT (ConT $ mkName className) (ConT name)

binaryFunDec :: Name -> String -> Dec
binaryFunDec name fun' = FunD fun [Clause [pat1, pat2] body []]
  where pat1 = ConP name [VarP v1]
        pat2 = ConP name [VarP v2]
        body = NormalB $ AppE (ConE name) result
        result = AppE (AppE (VarE fun) (VarE v1)) (VarE v2)
        fun = mkName fun'
        v1 = mkName "v1"
        v2 = mkName "v2"

unariFunDec1 :: Name -> String -> Dec
unariFunDec1 name fun' = FunD fun [Clause [pat] body []]
  where pat = ConP name [VarP v]
        body = NormalB $ AppE (ConE name) (AppE (VarE fun) (VarE v))
        fun = mkName fun'
        v = mkName "v"

unariFunDec2 :: Name -> String -> Dec
unariFunDec2 name fun' = FunD fun [Clause [pat] body []]
  where pat = VarP x
        body = NormalB $ AppE (ConE name) (AppE (VarE fun) (VarE x))
        fun = mkName fun'
        x = mkName "x"
