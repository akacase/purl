module ParseSpec (spec) where

import Purl (parsePurl, purlText)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec =
  do
    it "parses a simple url" $
      do
        fmap purlText (parsePurl "pkg:github/clojure/clojurescript@1.10.339?os=mac&lang=js#readme")
        `shouldBe` Right "pkg:github/clojure/clojurescript@1.10.339?os=mac&lang=js#readme"
