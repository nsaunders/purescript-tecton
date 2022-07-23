-- https://www.w3.org/TR/css-backgrounds-3/

module Test.BackgroundsSpec where

import Prelude hiding (bottom)

import Color (rgb, white)
import Data.Tuple.Nested ((/\))
import PSCSS (at2, bottom, currentColor, inherit, initial, none, radialGradient1, right, stop, transparent, unset, url)
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFrom)

spec :: Spec Unit
spec =
  describe "Backgrounds and Borders Module" do

    describe "background-color property" do

      "background-color:inherit" `isRenderedFrom` { backgroundColor: inherit }

      "background-color:initial" `isRenderedFrom` { backgroundColor: initial }

      "background-color:unset" `isRenderedFrom` { backgroundColor: unset }

      "background-color:#0000ff"
        `isRenderedFrom`
        { backgroundColor: rgb 0 0 255 }

      "background-color:currentColor"
        `isRenderedFrom`
        { backgroundColor: currentColor }

      "background-color:transparent"
        `isRenderedFrom`
        { backgroundColor: transparent }

    describe "background-image property" do

      "background-image:inherit" `isRenderedFrom` { backgroundImage: inherit }

      "background-image:initial" `isRenderedFrom` { backgroundImage: initial }

      "background-image:unset" `isRenderedFrom` { backgroundImage: unset }

      "background-image:url(\"marble.svg\")"
        `isRenderedFrom`
        { backgroundImage: url "marble.svg" }

      "background-image:none"
        `isRenderedFrom`
        { backgroundImage: none }

      "background-image:url(\"tl.png\"),url(\"tr.png\")"
        `isRenderedFrom`
        { backgroundImage: url "tl.png" /\ url "tr.png" }

      "background-image:radial-gradient(at right bottom,transparent,#ffffff)"
        `isRenderedFrom`
        { backgroundImage:
            radialGradient1 (at2 right bottom) # stop transparent # stop white
        }

      "background-image:none,url(\"cat.jpg\")"
        `isRenderedFrom`
        { backgroundImage: none /\ url "cat.jpg" }
