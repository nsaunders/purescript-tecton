-- https://www.w3.org/TR/css-backgrounds-3/

module Test.BackgroundsSpec where

import Prelude hiding (bottom, top)

import Color (black, rgb, white)
import Data.Tuple.Nested ((/\))
import PSCSS (at2, at3, at4, auto, bgSize2, borderBox, bottom, center, contain, contentBox, cover, currentColor, dashed, dotted, double, fixed, groove, hidden, inherit, initial, inset, left, local, medium, noRepeat, none, outset, paddingBox, pct, px, radialGradient1, repeat, repeat2, repeatX, repeatY, ridge, right, round, scroll, solid, space, stop, thick, thin, top, transparent, unset, url)
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

    describe "border-top-style property" do

      "border-top-style:inherit" `isRenderedFrom` { borderTopStyle: inherit }

      "border-top-style:initial" `isRenderedFrom` { borderTopStyle: initial }

      "border-top-style:unset" `isRenderedFrom` { borderTopStyle: unset }

      "border-top-style:none" `isRenderedFrom` { borderTopStyle: none }

      "border-top-style:hidden" `isRenderedFrom` { borderTopStyle: hidden }

      "border-top-style:dotted" `isRenderedFrom` { borderTopStyle: dotted }

      "border-top-style:dashed" `isRenderedFrom` { borderTopStyle: dashed }

      "border-top-style:solid" `isRenderedFrom` { borderTopStyle: solid }

      "border-top-style:double" `isRenderedFrom` { borderTopStyle: double }

      "border-top-style:groove" `isRenderedFrom` { borderTopStyle: groove }

      "border-top-style:ridge" `isRenderedFrom` { borderTopStyle: ridge }

      "border-top-style:inset" `isRenderedFrom` { borderTopStyle: inset }

      "border-top-style:outset" `isRenderedFrom` { borderTopStyle: outset }

    describe "border-right-style property" do

      "border-right-style:inherit"
        `isRenderedFrom`
        { borderRightStyle: inherit }

      "border-right-style:initial"
        `isRenderedFrom`
        { borderRightStyle: initial }

      "border-right-style:unset" `isRenderedFrom` { borderRightStyle: unset }

      "border-right-style:none" `isRenderedFrom` { borderRightStyle: none }

      "border-right-style:hidden" `isRenderedFrom` { borderRightStyle: hidden }

      "border-right-style:dotted" `isRenderedFrom` { borderRightStyle: dotted }

      "border-right-style:dashed" `isRenderedFrom` { borderRightStyle: dashed }

      "border-right-style:solid" `isRenderedFrom` { borderRightStyle: solid }

      "border-right-style:double" `isRenderedFrom` { borderRightStyle: double }

      "border-right-style:groove" `isRenderedFrom` { borderRightStyle: groove }

      "border-right-style:ridge" `isRenderedFrom` { borderRightStyle: ridge }

      "border-right-style:inset" `isRenderedFrom` { borderRightStyle: inset }

      "border-right-style:outset" `isRenderedFrom` { borderRightStyle: outset }

    describe "border-bottom-style property" do

      "border-bottom-style:inherit"
        `isRenderedFrom`
        { borderBottomStyle: inherit }

      "border-bottom-style:initial"
        `isRenderedFrom`
        { borderBottomStyle: initial }

      "border-bottom-style:unset" `isRenderedFrom` { borderBottomStyle: unset }

      "border-bottom-style:none" `isRenderedFrom` { borderBottomStyle: none }

      "border-bottom-style:hidden"
        `isRenderedFrom`
        { borderBottomStyle: hidden }

      "border-bottom-style:dotted"
        `isRenderedFrom`
        { borderBottomStyle: dotted }

      "border-bottom-style:dashed"
        `isRenderedFrom`
        { borderBottomStyle: dashed }

      "border-bottom-style:solid" `isRenderedFrom` { borderBottomStyle: solid }

      "border-bottom-style:double"
        `isRenderedFrom`
        { borderBottomStyle: double }

      "border-bottom-style:groove"
        `isRenderedFrom`
        { borderBottomStyle: groove }

      "border-bottom-style:ridge" `isRenderedFrom` { borderBottomStyle: ridge }

      "border-bottom-style:inset" `isRenderedFrom` { borderBottomStyle: inset }

      "border-bottom-style:outset"
        `isRenderedFrom`
        { borderBottomStyle: outset }

    describe "border-left-style property" do

      "border-left-style:inherit" `isRenderedFrom` { borderLeftStyle: inherit }

      "border-left-style:initial" `isRenderedFrom` { borderLeftStyle: initial }

      "border-left-style:unset" `isRenderedFrom` { borderLeftStyle: unset }

      "border-left-style:none" `isRenderedFrom` { borderLeftStyle: none }

      "border-left-style:hidden" `isRenderedFrom` { borderLeftStyle: hidden }

      "border-left-style:dotted" `isRenderedFrom` { borderLeftStyle: dotted }

      "border-left-style:dashed" `isRenderedFrom` { borderLeftStyle: dashed }

      "border-left-style:solid" `isRenderedFrom` { borderLeftStyle: solid }

      "border-left-style:double" `isRenderedFrom` { borderLeftStyle: double }

      "border-left-style:groove" `isRenderedFrom` { borderLeftStyle: groove }

      "border-left-style:ridge" `isRenderedFrom` { borderLeftStyle: ridge }

      "border-left-style:inset" `isRenderedFrom` { borderLeftStyle: inset }

      "border-left-style:outset" `isRenderedFrom` { borderLeftStyle: outset }

    describe "border-style property" do

      "border-style:inherit" `isRenderedFrom` { borderStyle: inherit }

      "border-style:initial" `isRenderedFrom` { borderStyle: initial }

      "border-style:unset" `isRenderedFrom` { borderStyle: unset }

      "border-style:none" `isRenderedFrom` { borderStyle: none }

      "border-style:hidden" `isRenderedFrom` { borderStyle: hidden }

      "border-style:dotted" `isRenderedFrom` { borderStyle: dotted }

      "border-style:dashed" `isRenderedFrom` { borderStyle: dashed }

      "border-style:solid" `isRenderedFrom` { borderStyle: solid }

      "border-style:double" `isRenderedFrom` { borderStyle: double }

      "border-style:groove" `isRenderedFrom` { borderStyle: groove }

      "border-style:ridge" `isRenderedFrom` { borderStyle: ridge }

      "border-style:inset" `isRenderedFrom` { borderStyle: inset }

      "border-style:outset" `isRenderedFrom` { borderStyle: outset }

      "border-style:none ridge"
        `isRenderedFrom`
        { borderStyle: none /\ ridge }

      "border-style:hidden groove"
        `isRenderedFrom`
        { borderStyle: hidden /\ groove }

      "border-style:dotted solid groove"
        `isRenderedFrom`
        { borderStyle: dotted /\ solid /\ groove }

      "border-style:double dashed hidden"
        `isRenderedFrom`
        { borderStyle: double /\ dashed /\ hidden }

      "border-style:none dotted solid double"
        `isRenderedFrom`
        { borderStyle: none /\ dotted /\ solid /\ double }

      "border-style:outset inset ridge groove"
        `isRenderedFrom`
        { borderStyle: outset /\ inset /\ ridge /\ groove }

    describe "border-top-width" do

      "border-top-width:inherit" `isRenderedFrom` { borderTopWidth: inherit }

      "border-top-width:initial" `isRenderedFrom` { borderTopWidth: initial }

      "border-top-width:unset" `isRenderedFrom` { borderTopWidth: unset }

      "border-top-width:thin" `isRenderedFrom` { borderTopWidth: thin }

      "border-top-width:medium" `isRenderedFrom` { borderTopWidth: medium }

      "border-top-width:thick" `isRenderedFrom` { borderTopWidth: thick }

      "border-top-width:1px" `isRenderedFrom` { borderTopWidth: px 1 }

    describe "border-right-width" do

      "border-right-width:inherit"
        `isRenderedFrom`
        { borderRightWidth: inherit }

      "border-right-width:initial"
        `isRenderedFrom`
        { borderRightWidth: initial }

      "border-right-width:unset" `isRenderedFrom` { borderRightWidth: unset }

      "border-right-width:thin" `isRenderedFrom` { borderRightWidth: thin }

      "border-right-width:medium" `isRenderedFrom` { borderRightWidth: medium }

      "border-right-width:thick" `isRenderedFrom` { borderRightWidth: thick }

      "border-right-width:1px" `isRenderedFrom` { borderRightWidth: px 1 }

    describe "border-bottom-width" do

      "border-bottom-width:inherit"
        `isRenderedFrom`
        { borderBottomWidth: inherit }

      "border-bottom-width:initial"
        `isRenderedFrom`
        { borderBottomWidth: initial }

      "border-bottom-width:unset" `isRenderedFrom` { borderBottomWidth: unset }

      "border-bottom-width:thin" `isRenderedFrom` { borderBottomWidth: thin }

      "border-bottom-width:medium"
        `isRenderedFrom`
        { borderBottomWidth: medium }

      "border-bottom-width:thick" `isRenderedFrom` { borderBottomWidth: thick }

      "border-bottom-width:1px" `isRenderedFrom` { borderBottomWidth: px 1 }

    describe "border-left-width" do

      "border-left-width:inherit" `isRenderedFrom` { borderLeftWidth: inherit }

      "border-left-width:initial" `isRenderedFrom` { borderLeftWidth: initial }

      "border-left-width:unset" `isRenderedFrom` { borderLeftWidth: unset }

      "border-left-width:thin" `isRenderedFrom` { borderLeftWidth: thin }

      "border-left-width:medium" `isRenderedFrom` { borderLeftWidth: medium }

      "border-left-width:thick" `isRenderedFrom` { borderLeftWidth: thick }

      "border-left-width:1px" `isRenderedFrom` { borderLeftWidth: px 1 }

    describe "border-width" do

      "border-width:inherit" `isRenderedFrom` { borderWidth: inherit }

      "border-width:initial" `isRenderedFrom` { borderWidth: initial }

      "border-width:unset" `isRenderedFrom` { borderWidth: unset }

      "border-width:thin" `isRenderedFrom` { borderWidth: thin }

      "border-width:medium" `isRenderedFrom` { borderWidth: medium }

      "border-width:thick" `isRenderedFrom` { borderWidth: thick }

      "border-width:1px" `isRenderedFrom` { borderWidth: px 1 }

      "border-width:1px medium"
        `isRenderedFrom`
        { borderWidth: px 1 /\ medium }

      "border-width:thick thin"
        `isRenderedFrom`
        { borderWidth: thick /\ thin }

      "border-width:medium 1px thick"
        `isRenderedFrom`
        { borderWidth: medium /\ px 1 /\ thick }

      "border-width:thin 1px 2px"
        `isRenderedFrom`
        { borderWidth: thin /\ px 1 /\ px 2 }

      "border-width:1px thin 1px 2px"
        `isRenderedFrom`
        { borderWidth: px 1 /\ thin /\ px 1 /\ px 2 }

      "border-width:medium thin 1px thick"
        `isRenderedFrom`
        { borderWidth: medium /\ thin /\ px 1 /\ thick }

    describe "border-top property" do

      "border-top:inherit" `isRenderedFrom` { borderTop: inherit }

      "border-top:initial" `isRenderedFrom` { borderTop: initial }

      "border-top:unset" `isRenderedFrom` { borderTop: unset }

      "border-top:1px none #000000"
        `isRenderedFrom`
        { borderTop: px 1 /\ none /\ black }

      "border-top:thin none #000000"
        `isRenderedFrom`
        { borderTop: thin /\ none /\ black }

      "border-top:medium none #000000"
        `isRenderedFrom`
        { borderTop: medium /\ none /\ black }

      "border-top:1px hidden #000000"
        `isRenderedFrom`
        { borderTop: px 1 /\ hidden /\ black }

      "border-top:thick hidden #000000"
        `isRenderedFrom`
        { borderTop: thick /\ hidden /\ black }

      "border-top:medium hidden #000000"
        `isRenderedFrom`
        { borderTop: medium /\ hidden /\ black }

      "border-top:1px solid #000000"
        `isRenderedFrom`
        { borderTop: px 1 /\ solid /\ black }

      "border-top:thin solid #000000"
        `isRenderedFrom`
        { borderTop: thin /\ solid /\ black }

      "border-top:medium solid #000000"
        `isRenderedFrom`
        { borderTop: medium /\ solid /\ black }

    describe "border-right property" do

      "border-right:inherit" `isRenderedFrom` { borderRight: inherit }

      "border-right:initial" `isRenderedFrom` { borderRight: initial }

      "border-right:unset" `isRenderedFrom` { borderRight: unset }

      "border-right:1px none #000000"
        `isRenderedFrom`
        { borderRight: px 1 /\ none /\ black }

      "border-right:thin none #000000"
        `isRenderedFrom`
        { borderRight: thin /\ none /\ black }

      "border-right:medium none #000000"
        `isRenderedFrom`
        { borderRight: medium /\ none /\ black }

      "border-right:1px hidden #000000"
        `isRenderedFrom`
        { borderRight: px 1 /\ hidden /\ black }

      "border-right:thick hidden #000000"
        `isRenderedFrom`
        { borderRight: thick /\ hidden /\ black }

      "border-right:medium hidden #000000"
        `isRenderedFrom`
        { borderRight: medium /\ hidden /\ black }

      "border-right:1px solid #000000"
        `isRenderedFrom`
        { borderRight: px 1 /\ solid /\ black }

      "border-right:thin solid #000000"
        `isRenderedFrom`
        { borderRight: thin /\ solid /\ black }

      "border-right:medium solid #000000"
        `isRenderedFrom`
        { borderRight: medium /\ solid /\ black }

    describe "border-bottom property" do

      "border-bottom:inherit" `isRenderedFrom` { borderBottom: inherit }

      "border-bottom:initial" `isRenderedFrom` { borderBottom: initial }

      "border-bottom:unset" `isRenderedFrom` { borderBottom: unset }

      "border-bottom:1px none #000000"
        `isRenderedFrom`
        { borderBottom: px 1 /\ none /\ black }

      "border-bottom:thin none #000000"
        `isRenderedFrom`
        { borderBottom: thin /\ none /\ black }

      "border-bottom:medium none #000000"
        `isRenderedFrom`
        { borderBottom: medium /\ none /\ black }

      "border-bottom:1px hidden #000000"
        `isRenderedFrom`
        { borderBottom: px 1 /\ hidden /\ black }

      "border-bottom:thick hidden #000000"
        `isRenderedFrom`
        { borderBottom: thick /\ hidden /\ black }

      "border-bottom:medium hidden #000000"
        `isRenderedFrom`
        { borderBottom: medium /\ hidden /\ black }

      "border-bottom:1px solid #000000"
        `isRenderedFrom`
        { borderBottom: px 1 /\ solid /\ black }

      "border-bottom:thin solid #000000"
        `isRenderedFrom`
        { borderBottom: thin /\ solid /\ black }

      "border-bottom:medium solid #000000"
        `isRenderedFrom`
        { borderBottom: medium /\ solid /\ black }

    describe "border-left property" do

      "border-left:inherit" `isRenderedFrom` { borderLeft: inherit }

      "border-left:initial" `isRenderedFrom` { borderLeft: initial }

      "border-left:unset" `isRenderedFrom` { borderLeft: unset }

      "border-left:1px none #000000"
        `isRenderedFrom`
        { borderLeft: px 1 /\ none /\ black }

      "border-left:thin none #000000"
        `isRenderedFrom`
        { borderLeft: thin /\ none /\ black }

      "border-left:medium none #000000"
        `isRenderedFrom`
        { borderLeft: medium /\ none /\ black }

      "border-left:1px hidden #000000"
        `isRenderedFrom`
        { borderLeft: px 1 /\ hidden /\ black }

      "border-left:thick hidden #000000"
        `isRenderedFrom`
        { borderLeft: thick /\ hidden /\ black }

      "border-left:medium hidden #000000"
        `isRenderedFrom`
        { borderLeft: medium /\ hidden /\ black }

      "border-left:1px solid #000000"
        `isRenderedFrom`
        { borderLeft: px 1 /\ solid /\ black }

      "border-left:thin solid #000000"
        `isRenderedFrom`
        { borderLeft: thin /\ solid /\ black }

      "border-left:medium solid #000000"
        `isRenderedFrom`
        { borderLeft: medium /\ solid /\ black }

    describe "border property" do

      "border:inherit" `isRenderedFrom` { border: inherit }

      "border:initial" `isRenderedFrom` { border: initial }

      "border:unset" `isRenderedFrom` { border: unset }

      "border:1px none #000000"
        `isRenderedFrom`
        { border: px 1 /\ none /\ black }

      "border:thin none #000000"
        `isRenderedFrom`
        { border: thin /\ none /\ black }

      "border:medium none #000000"
        `isRenderedFrom`
        { border: medium /\ none /\ black }

      "border:1px hidden #000000"
        `isRenderedFrom`
        { border: px 1 /\ hidden /\ black }

      "border:thick hidden #000000"
        `isRenderedFrom`
        { border: thick /\ hidden /\ black }

      "border:medium hidden #000000"
        `isRenderedFrom`
        { border: medium /\ hidden /\ black }

      "border:1px solid #000000"
        `isRenderedFrom`
        { border: px 1 /\ solid /\ black }

      "border:thin solid #000000"
        `isRenderedFrom`
        { border: thin /\ solid /\ black }

      "border:medium solid #000000"
        `isRenderedFrom`
        { border: medium /\ solid /\ black }

    describe "border-top-left-radius property" do

      "border-top-left-radius:inherit"
        `isRenderedFrom`
        { borderTopLeftRadius: inherit }

      "border-top-left-radius:initial"
        `isRenderedFrom`
        { borderTopLeftRadius: initial }

      "border-top-left-radius:unset"
        `isRenderedFrom`
        { borderTopLeftRadius: unset }

      "border-top-left-radius:1px"
        `isRenderedFrom`
        { borderTopLeftRadius: px 1 }

      "border-top-left-radius:10%"
        `isRenderedFrom`
        { borderTopLeftRadius: pct 10 }

      "border-top-left-radius:1px 10%"
        `isRenderedFrom`
        { borderTopLeftRadius: px 1 /\ pct 10 }

      "border-top-left-radius:10% 1px"
        `isRenderedFrom`
        { borderTopLeftRadius: pct 10 /\ px 1 }

    describe "border-top-right-radius property" do

      "border-top-right-radius:inherit"
        `isRenderedFrom`
        { borderTopRightRadius: inherit }

      "border-top-right-radius:initial"
        `isRenderedFrom`
        { borderTopRightRadius: initial }

      "border-top-right-radius:unset"
        `isRenderedFrom`
        { borderTopRightRadius: unset }

      "border-top-right-radius:1px"
        `isRenderedFrom`
        { borderTopRightRadius: px 1 }

      "border-top-right-radius:10%"
        `isRenderedFrom`
        { borderTopRightRadius: pct 10 }

      "border-top-right-radius:1px 10%"
        `isRenderedFrom`
        { borderTopRightRadius: px 1 /\ pct 10 }

      "border-top-right-radius:10% 1px"
        `isRenderedFrom`
        { borderTopRightRadius: pct 10 /\ px 1 }
