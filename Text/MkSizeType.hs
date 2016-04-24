{-# LANGUAGE CPP #-}

-- | Internal functions to generate CSS size wrapper types.
module Text.MkSizeType (mkSizeType) where

#if MIN_VERSION_template_haskell(2,11,0)
import Language.Haskell.TH (conT)
#endif
import Language.Haskell.TH.Syntax
import Data.Text.Lazy.Builder (fromLazyText)
import qualified Data.Text.Lazy as TL

mkSizeType :: String -> String -> Q [Dec]
mkSizeType name' unit = do
    ddn <- dataDec name
    return  [ ddn
            , showInstanceDec name unit
            , numInstanceDec name
            , fractionalInstanceDec name
            , toCssInstanceDec name ]
  where name = mkName $ name'

dataDec :: Name -> Q Dec
dataDec name =
#if MIN_VERSION_template_haskell(2,11,0)
     DataD [] name [] Nothing [constructor] <$> mapM conT derives
#else
    return $ DataD [] name [] [constructor] derives
#endif
  where constructor = NormalC name [(notStrict, ConT $ mkName "Rational")]
        derives = map mkName ["Eq", "Ord"]

showInstanceDec :: Name -> String -> Dec
showInstanceDec name unit' = instanceD [] (instanceType "Show" name) [showDec]
  where showSize = VarE $ mkName "showSize"
        x = mkName "x"
        unit = LitE $ StringL unit'
        showDec = FunD (mkName "show") [Clause [showPat] showBody []]
        showPat = ConP name [VarP x]
        showBody = NormalB $ AppE (AppE showSize $ VarE x) unit

numInstanceDec :: Name -> Dec
numInstanceDec name = instanceD [] (instanceType "Num" name) decs
  where decs = map (binaryFunDec name) ["+", "*", "-"] ++
               map (unariFunDec1 name) ["abs", "signum"] ++
               [unariFunDec2 name "fromInteger"]

fractionalInstanceDec :: Name -> Dec
fractionalInstanceDec name = instanceD [] (instanceType "Fractional" name) decs
  where decs = [binaryFunDec name "/", unariFunDec2 name "fromRational"]

toCssInstanceDec :: Name -> Dec
toCssInstanceDec name = instanceD [] (instanceType "ToCss" name) [toCssDec]
  where toCssDec = FunD (mkName "toCss") [Clause [] showBody []]
        showBody = NormalB $ (AppE dot from) `AppE` ((AppE dot pack) `AppE` show')
        from = VarE 'fromLazyText
        pack = VarE 'TL.pack
        dot = VarE 'Prelude.fmap
        show' = VarE 'Prelude.show

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

#if MIN_VERSION_template_haskell(2,11,0)
notStrict :: Bang
notStrict = Bang NoSourceUnpackedness NoSourceStrictness
#else
notStrict :: Strict
notStrict = NotStrict
#endif

instanceD :: Cxt -> Type -> [Dec] -> Dec
#if MIN_VERSION_template_haskell(2,11,0)
instanceD = InstanceD Nothing
#else
instanceD = InstanceD
#endif
