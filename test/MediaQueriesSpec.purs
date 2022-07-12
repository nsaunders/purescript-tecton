module Test.MediaQueriesSpec where

import Prelude

import PSCSS (all, media, pct, print, px, screen, universal, (?))
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFrom)

spec :: Spec Unit
spec =
  describe "Media Queries Module" do

    describe "width feature" do

      "@media all and (width:400px){*{width:100%}}"
        `isRenderedFrom` do
        media all { width: px 400 } ?
          universal ?
            { width: pct 100
            }

      "@media print and (width:400px){*{width:100%}}"
        `isRenderedFrom` do
        media print { width: px 400 } ?
          universal ?
            { width: pct 100
            }

      "@media screen and (width:400px){*{width:100%}}"
        `isRenderedFrom` do
        media screen { width: px 400 } ?
          universal ?
            { width: pct 100
            }

      "@media all and (min-width:400px){*{width:100%}}"
        `isRenderedFrom` do
        media all { minWidth: px 400 } ?
          universal ?
            { width: pct 100
            }
