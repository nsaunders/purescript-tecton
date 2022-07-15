-- https://www.w3.org/TR/mediaqueries-3/

module Test.MediaQueriesSpec where

import Prelude

import PSCSS (all, media, pct, print, px, screen, universal, (?))
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFrom)

spec :: Spec Unit
spec =
  describe "Media Queries Module" do

    describe "Media features" do

      describe "width" do

        "@media all and (width:600px){*{}}"
          `isRenderedFrom` do
          media all { width: px 600 } ?
            universal ? {}

        "@media all and (min-width:400px) and (max-width:999px){*{}}"
          `isRenderedFrom` do
          media all { minWidth: px 400, maxWidth: px 999 } ?
            universal ? {}
