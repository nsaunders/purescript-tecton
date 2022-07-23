-- https://www.w3.org/TR/css-backgrounds-3/

module Test.BackgroundsSpec where

import Prelude hiding (bottom, top)

import Color (rgb, white)
import Data.Tuple.Nested ((/\))
import PSCSS (at2, at3, at4, borderBox, bottom, center, contentBox, currentColor, fixed, inherit, initial, left, local, noRepeat, none, paddingBox, px, radialGradient1, repeat, repeat2, repeatX, repeatY, right, round, scroll, space, stop, top, transparent, unset, url)
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

    describe "background-repeat property" do

      "background-repeat:inherit" `isRenderedFrom` { backgroundRepeat: inherit }

      "background-repeat:initial" `isRenderedFrom` { backgroundRepeat: initial }

      "background-repeat:unset" `isRenderedFrom` { backgroundRepeat: unset }

      "background-repeat:repeat-x"
        `isRenderedFrom`
        { backgroundRepeat: repeatX }

      "background-repeat:repeat-y"
        `isRenderedFrom`
        { backgroundRepeat: repeatY }

      "background-repeat:repeat"
        `isRenderedFrom`
        { backgroundRepeat: repeat }

      "background-repeat:space"
        `isRenderedFrom`
        { backgroundRepeat: space }

      "background-repeat:round"
        `isRenderedFrom`
        { backgroundRepeat: round }

      "background-repeat:no-repeat"
        `isRenderedFrom`
        { backgroundRepeat: noRepeat }

      "background-repeat:repeat no-repeat"
        `isRenderedFrom`
        { backgroundRepeat: repeat2 repeat noRepeat }

      "background-repeat:space round,repeat-x"
        `isRenderedFrom`
        { backgroundRepeat: repeat2 space round /\ repeatX }

    describe "background-attachment property" do

      "background-attachment:inherit"
        `isRenderedFrom`
        { backgroundAttachment: inherit }

      "background-attachment:initial"
        `isRenderedFrom`
        { backgroundAttachment: initial }

      "background-attachment:unset"
        `isRenderedFrom`
        { backgroundAttachment: unset }

      "background-attachment:fixed"
        `isRenderedFrom`
        { backgroundAttachment: fixed }

      "background-attachment:local"
        `isRenderedFrom`
        { backgroundAttachment: local }

      "background-attachment:scroll"
        `isRenderedFrom`
        { backgroundAttachment: scroll }

      "background-attachment:fixed,local,scroll"
        `isRenderedFrom`
        { backgroundAttachment: fixed /\ local /\ scroll }

    describe "background-position property" do

      "background-position:inherit"
        `isRenderedFrom`
        { backgroundPosition: inherit }

      "background-position:initial"
        `isRenderedFrom`
        { backgroundPosition: initial }

      "background-position:unset" `isRenderedFrom` { backgroundPosition: unset }
       
      "background-position:top" `isRenderedFrom` { backgroundPosition: top } 

      "background-position:right" `isRenderedFrom` { backgroundPosition: right } 

      "background-position:bottom"
        `isRenderedFrom`
        { backgroundPosition: bottom } 

      "background-position:left" `isRenderedFrom` { backgroundPosition: left } 

      "background-position:left top"
        `isRenderedFrom`
        { backgroundPosition: at2 left top } 

      "background-position:left 10px top"
        `isRenderedFrom`
        { backgroundPosition: at3 left (px 10) top } 

      "background-position:left 10px top 15px"
        `isRenderedFrom`
        { backgroundPosition: at4 left (px 10) top (px 15) } 

      "background-position:center bottom,left 15px,center top -5px"
        `isRenderedFrom`
        { backgroundPosition:
            at2 center bottom /\ at2 left (px 15) /\ at3 center top (px (-5))
        } 

    describe "background-clip property" do

      "background-clip:inherit" `isRenderedFrom` { backgroundClip: inherit }

      "background-clip:initial" `isRenderedFrom` { backgroundClip: initial }

      "background-clip:unset" `isRenderedFrom` { backgroundClip: unset }

      "background-clip:border-box"
        `isRenderedFrom`
        { backgroundClip: borderBox }

      "background-clip:padding-box"
        `isRenderedFrom`
        { backgroundClip: paddingBox }

      "background-clip:content-box"
        `isRenderedFrom`
        { backgroundClip: contentBox }

      "background-clip:border-box,padding-box,content-box"
        `isRenderedFrom`
        { backgroundClip: borderBox /\ paddingBox /\ contentBox }

    describe "background-origin property" do

      "background-origin:inherit" `isRenderedFrom` { backgroundOrigin: inherit }

      "background-origin:initial" `isRenderedFrom` { backgroundOrigin: initial }

      "background-origin:unset" `isRenderedFrom` { backgroundOrigin: unset }

      "background-origin:border-box"
        `isRenderedFrom`
        { backgroundOrigin: borderBox }

      "background-origin:padding-box"
        `isRenderedFrom`
        { backgroundOrigin: paddingBox }

      "background-origin:content-box"
        `isRenderedFrom`
        { backgroundOrigin: contentBox }

      "background-origin:border-box,padding-box,content-box"
        `isRenderedFrom`
        { backgroundOrigin: borderBox /\ paddingBox /\ contentBox }
