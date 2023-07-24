module ParseSpec (spec) where

import Purl (parsePurl, purlText)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec =
  do
    it "parses a simple url" $
      do
        fmap purlText (parsePurl "pkg:github/cabal/Cabal@3.10.1.0?os=mac&lang=hs#README.md")
        `shouldBe` Right "pkg:github/cabal/Cabal@3.10.1.0?os=mac&lang=hs#README.md"
    it "parses a url without a version" $
      do
        fmap purlText (parsePurl "pkg:github/cabal/Cabal?os=mac&lang=hs#README.md")
        `shouldBe` Right "pkg:github/cabal/Cabal?os=mac&lang=hs#README.md"
    it "parses a url without a qualifiers, but with a subpath" $
      do
        fmap purlText (parsePurl "pkg:github/cabal/Cabal@3.10.1.0#README.md")
        `shouldBe` Right "pkg:github/cabal/Cabal@3.10.1.0#README.md"
    it "parses a url with qualifiers, but without a subpath and version" $
      do
        fmap purlText (parsePurl "pkg:github/cabal/Cabal?os=mac&lang=hs")
        `shouldBe` Right "pkg:github/cabal/Cabal?os=mac&lang=hs"
    it "parses a url with qualifiers, but without a subpath and version" $
      do
        fmap purlText (parsePurl "pkg:github/cabal/Cabal?os=mac&lang=hs")
        `shouldBe` Right "pkg:github/cabal/Cabal?os=mac&lang=hs"
    it "parses a url without namespace" $
      do
        fmap purlText (parsePurl "pkg:github/Cabal?os=mac&lang=hs")
        `shouldBe` Right "pkg:github/Cabal?os=mac&lang=hs"
