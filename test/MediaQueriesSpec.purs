module Test.MediaQueriesSpec where

import Prelude

import PSCSS (all, media, pct, print, px, screen, universal, (?))
import Test.Spec (Spec, describe)
import Test.Util (renders)

spec :: Spec Unit
spec = describe "Media Queries Module" do
  describe "width feature" do

    (media all { width: px 400 } ? universal ? { width: pct 100 })
      `renders`
      "@media all and (width:400px){*{width:100%}}"

    (media print { width: px 400 } ? universal ? { width: pct 100 })
      `renders`
      "@media print and (width:400px){*{width:100%}}"

    (media screen { width: px 400 } ? universal ? { width: pct 100 })
      `renders`
      "@media screen and (width:400px){*{width:100%}}"

    (media all { minWidth: px 400 } ? universal ? { width: pct 100 })
      `renders`
      "@media all and (min-width:400px){*{width:100%}}"
