module Test.RenderSpec where

import Prelude

import Tecton
  ( all
  , compact
  , height
  , media
  , nil
  , pretty
  , renderInline
  , renderSheet
  , universal
  , width
  , (:=)
  , (?)
  )
import Tecton.Rule as Rule
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do

  describe "renderInline" do
    it "renders a series of declarations" $
      let
        inlineStyles =
          renderInline Rule.do
            width := nil
            height := nil
      in
        inlineStyles `shouldEqual` "width: 0; height: 0"

  describe "renderSheet" do
    it "renders a compact style sheet" $
      let
        actual = renderSheet compact $ media all {} ? universal ? width := nil
        expected = "@media all{*{width:0}}"
      in
        actual `shouldEqual` expected
    it "renders a pretty-printed style sheet" $
      let
        actual = renderSheet pretty $ media all {} ? universal ? width := nil
        expected =
          """@media all {
  * {
    width: 0;
  }
}"""
      in
        actual `shouldEqual` expected
