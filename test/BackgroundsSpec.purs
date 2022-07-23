-- https://www.w3.org/TR/css-backgrounds-3/

module Test.BackgroundsSpec where

import Prelude hiding (bottom, top)

import Color (black, rgb, white)
import Data.Tuple.Nested ((/\))
import PSCSS (at2, at3, at4, auto, bgSize2, borderBox, bottom, center, contain, contentBox, cover, currentColor, fixed, inherit, initial, left, local, noRepeat, none, paddingBox, pct, px, radialGradient1, repeat, repeat2, repeatX, repeatY, right, round, scroll, space, stop, top, transparent, unset, url)
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

    describe "background-size property" do

      "background-size:inherit" `isRenderedFrom` { backgroundSize: inherit }

      "background-size:initial" `isRenderedFrom` { backgroundSize: initial }

      "background-size:unset" `isRenderedFrom` { backgroundSize: unset }

      "background-size:100px" `isRenderedFrom` { backgroundSize: px 100 }

      "background-size:10%" `isRenderedFrom` { backgroundSize: pct 10 }

      "background-size:auto" `isRenderedFrom` { backgroundSize: auto }

      "background-size:cover" `isRenderedFrom` { backgroundSize: cover }

      "background-size:contain" `isRenderedFrom` { backgroundSize: contain }

      "background-size:50% 100px"
        `isRenderedFrom`
        { backgroundSize: bgSize2 (pct 50) (px 100) }

      "background-size:25% auto"
        `isRenderedFrom`
        { backgroundSize: bgSize2 (pct 25) auto }

      "background-size:auto 5px"
        `isRenderedFrom`
        { backgroundSize: bgSize2 auto (px 5) }

      "background-size:50%,50px,auto 100px,auto,cover,contain"
        `isRenderedFrom`
        { backgroundSize:
            pct 50 /\ px 50 /\ bgSize2 auto (px 100) /\ auto /\ cover /\ contain
        }

    describe "border-top-color property" do

      "border-top-color:inherit" `isRenderedFrom` { borderTopColor: inherit }

      "border-top-color:initial" `isRenderedFrom` { borderTopColor: initial }

      "border-top-color:unset" `isRenderedFrom` { borderTopColor: unset }

      "border-top-color:currentColor"
        `isRenderedFrom`
        { borderTopColor: currentColor }

      "border-top-color:#000055" `isRenderedFrom` { borderTopColor: rgb 0 0 85 }

    describe "border-right-color property" do

      "border-right-color:inherit"
        `isRenderedFrom`
        { borderRightColor: inherit }

      "border-right-color:initial"
        `isRenderedFrom`
        { borderRightColor: initial }

      "border-right-color:unset" `isRenderedFrom` { borderRightColor: unset }

      "border-right-color:currentColor"
        `isRenderedFrom`
        { borderRightColor: currentColor }

      "border-right-color:#000055"
        `isRenderedFrom`
        { borderRightColor: rgb 0 0 85 }

    describe "border-bottom-color property" do

      "border-bottom-color:inherit"
        `isRenderedFrom`
        { borderBottomColor: inherit }

      "border-bottom-color:initial"
        `isRenderedFrom`
        { borderBottomColor: initial }

      "border-bottom-color:unset" `isRenderedFrom` { borderBottomColor: unset }

      "border-bottom-color:currentColor"
        `isRenderedFrom`
        { borderBottomColor: currentColor }

      "border-bottom-color:#000055"
        `isRenderedFrom`
        { borderBottomColor: rgb 0 0 85 }

    describe "border-left-color property" do

      "border-left-color:inherit" `isRenderedFrom` { borderLeftColor: inherit }

      "border-left-color:initial" `isRenderedFrom` { borderLeftColor: initial }

      "border-left-color:unset" `isRenderedFrom` { borderLeftColor: unset }

      "border-left-color:currentColor"
        `isRenderedFrom`
        { borderLeftColor: currentColor }

      "border-left-color:#000055"
        `isRenderedFrom`
        { borderLeftColor: rgb 0 0 85 }

    describe "border-color property" do

      "border-color:inherit" `isRenderedFrom` { borderColor: inherit }

      "border-color:initial" `isRenderedFrom` { borderColor: initial }

      "border-color:unset" `isRenderedFrom` { borderColor: unset }

      "border-color:transparent" `isRenderedFrom` { borderColor: transparent }

      "border-color:currentColor #0000ff"
        `isRenderedFrom`
        { borderColor: currentColor /\ rgb 0 0 255 }

      "border-color:#008000 #000000 #0000ff"
        `isRenderedFrom`
        { borderColor: rgb 0 128 0 /\ black /\ rgb 0 0 255 }

      "border-color:#008000 #000000 #0000ff #ffffff"
        `isRenderedFrom`
        { borderColor: rgb 0 128 0 /\ black /\ rgb 0 0 255 /\ rgb 255 255 255 }
