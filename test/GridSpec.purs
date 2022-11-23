-- https://www.w3.org/TR/css-grid-2/

module Test.GridSpec where

import Prelude

import Data.Tuple.Nested ((/\))
import Tecton (auto, autoFill, autoFit, column, dense, em, fitContent, fr, gridAutoColumns, gridAutoFlow, gridAutoRows, gridTemplateColumns, gridTemplateRows, inherit, initial, lineName, maxContent, minContent, minmax, none, pct, px, repeat, row, unset, vw, (:=), (@-@), (~))
import Test.Spec (Spec, describe)
import Test.Util (isRenderedFromInline)

spec :: Spec Unit
spec = do

  let isRenderedFrom = isRenderedFromInline

  describe "Grid Layout" do

    describe "grid-template-columns property" do

      "grid-template-columns:inherit"
        `isRenderedFrom`
        (gridTemplateColumns := inherit)

      "grid-template-columns:initial"
        `isRenderedFrom`
        (gridTemplateColumns := initial)

      "grid-template-columns:unset"
        `isRenderedFrom`
        (gridTemplateColumns := unset)

      "grid-template-columns:none"
        `isRenderedFrom`
        (gridTemplateColumns := none)
      
      "grid-template-columns:[attend settle] repeat(3,[document-festival] fit-content(421.3px) auto fit-content(399.6px))"
        `isRenderedFrom`
        ( gridTemplateColumns :=
            lineName "attend"
            /\ lineName "settle"
            /\ repeat 3
               ( lineName "document-festival"
                 /\ fitContent (px 421.3)
                 /\ auto
                 /\ fitContent (px 399.6)
               )
        )

      "grid-template-columns:repeat(auto-fit,53.1% minmax(138.5px,auto) [resist]) minmax(6.1%,max-content)"
        `isRenderedFrom`
        ( gridTemplateColumns :=
            repeat autoFit
            ( pct 53.1
              /\ minmax (px 138.5) auto
              /\ lineName "resist"
            )
            /\ minmax (pct 6.1) maxContent
        )

      "grid-template-columns:repeat(auto-fit,21.4px 90.5%) [despair-stage join-burden true-genius]"
        `isRenderedFrom`
        ( gridTemplateColumns :=
            repeat autoFit
            ( px 21.4
              /\ pct 90.5
            )
            /\ lineName "despair-stage"
            /\ lineName "join-burden"
            /\ lineName "true-genius"
        )

      "grid-template-columns:repeat(3,[leg] minmax(max-content,475px) max-content [mutual])"
        `isRenderedFrom`
        ( gridTemplateColumns :=
            repeat 3
            ( lineName "leg"
              /\ minmax maxContent (px 475)
              /\ maxContent
              /\ lineName "mutual"
            )
        )

      "grid-template-columns:21% [tobacco mandate] repeat(auto-fill,minmax(172.8vw,max-content) 60.7% 384.7vw) [wide]"
        `isRenderedFrom`
        ( gridTemplateColumns :=
            pct 21
            /\ lineName "tobacco"
            /\ lineName "mandate"
            /\ repeat autoFill
               ( minmax (vw 172.8) maxContent
                 /\ pct 60.7
                 /\ vw 384.7
               )
            /\ lineName "wide"
        )

      "grid-template-columns:[also] 27.8% repeat(auto-fill,minmax(29.5%,13.8%) minmax(76.2%,2fr))"
        `isRenderedFrom`
        ( gridTemplateColumns :=
            lineName "also"
            /\ pct 27.8
            /\ repeat autoFill
               ( minmax (pct 29.5) (pct 13.8)
                 /\ minmax (pct 76.2) (fr 2)
               )
        )

      "grid-template-columns:fit-content(153.5vw)"
        `isRenderedFrom`
        (gridTemplateColumns := fitContent (vw 153.5))

      "grid-template-columns:repeat(5,auto minmax(min-content,max-content) [afraid-pony ask-group])"
        `isRenderedFrom`
        ( gridTemplateColumns :=
            repeat 5
            ( auto
              /\ minmax minContent maxContent
              /\ lineName "afraid-pony"
              /\ lineName "ask-group"
            )
        )

      "grid-template-columns:minmax(max-content,1.1fr)"
        `isRenderedFrom`
        (gridTemplateColumns := minmax maxContent $ fr 1.1)

      "grid-template-columns:repeat(auto-fill,[family] 453.2vw 76.2%) repeat(1,minmax(121.1px,2fr) 329.5vw 67.2%)"
        `isRenderedFrom`
        ( gridTemplateColumns :=
            repeat autoFill (lineName "family" /\ vw 453.2 /\ pct 76.2)
            /\ repeat 1 (minmax (px 121.1) (fr 2) /\ vw 329.5 /\ pct 67.2)
        )

      "grid-template-columns:[arrive] minmax(max-content,1.7fr) [repeat-emerge]"
        `isRenderedFrom`
        ( gridTemplateColumns :=
            lineName "arrive"
            /\ minmax maxContent (fr 1.7)
            /\ lineName "repeat-emerge"
        )

      "grid-template-columns:[theme-aim argue-master] repeat(auto-fill,25.2% minmax(11%,1.8fr) [rigid-immense]) [wife ahead option-purpose]"
        `isRenderedFrom`
        ( gridTemplateColumns :=
            lineName "theme-aim"
            /\ lineName "argue-master"
            /\ repeat autoFill
               ( pct 25.2
                 /\ minmax (pct 11) (fr 1.8)
                 /\ lineName "rigid-immense"
               )
            /\ lineName "wife"
            /\ lineName "ahead"
            /\ lineName "option-purpose"
        )

      "grid-template-columns:repeat(auto-fill,minmax(38%,min-content) 467vw)"
        `isRenderedFrom`
        ( gridTemplateColumns :=
            repeat autoFill (minmax (pct 38) minContent /\ vw 467)
        )

      "grid-template-columns:minmax(auto,385vw)"
        `isRenderedFrom`
        (gridTemplateColumns := minmax auto $ vw 385)

      "grid-template-columns:440.2px repeat(auto-fill,minmax(475.2vw,auto) minmax(7.9%,71.3%) minmax(271.8em,max-content))"
        `isRenderedFrom`
        ( gridTemplateColumns :=
            px 440.2
            /\ repeat autoFill
               ( minmax (vw 475.2) auto
                 /\ minmax (pct 7.9) (pct 71.3)
                 /\ minmax (em 271.8) maxContent
               )
        )

      "grid-template-columns:repeat(2,minmax(min-content,57.4%) fit-content(71%))"
        `isRenderedFrom`
        ( gridTemplateColumns :=
            repeat 2 $ minmax minContent (pct 57.4) /\ fitContent (pct 71)
        )

      "grid-template-columns:[pass] repeat(1,[foot-battle brother trick-category] fit-content(21.5em) minmax(min-content,min-content) [salad])"
        `isRenderedFrom`
        ( gridTemplateColumns :=
            lineName "pass"
            /\ repeat 1
               ( lineName "foot-battle"
                 /\ lineName "brother"
                 /\ lineName "trick-category"
                 /\ fitContent (em 21.5)
                 /\ minmax minContent minContent
                 /\ lineName "salad"
               )
        )

      "grid-template-columns:[replace-another] repeat(4,[inmate valley believe] minmax(auto,2.5fr) fit-content(26%) [find])"
        `isRenderedFrom`
        ( gridTemplateColumns :=
            lineName "replace-another"
            /\ repeat 4
               ( lineName "inmate"
                 /\ lineName "valley"
                 /\ lineName "believe"
                 /\ minmax auto (fr 2.5)
                 /\ fitContent (pct 26)
                 /\ lineName "find"
               )
        )

      "grid-template-columns:repeat(auto-fill,[mask] 76.5% 191vw 17em)"
        `isRenderedFrom`
        ( gridTemplateColumns :=
            repeat autoFill $ lineName "mask" /\ pct 76.5 /\ vw 191 /\ em 17
        )

      "grid-template-columns:[shoulder-flavor] repeat(2,1.2fr fit-content(187.1em))"
        `isRenderedFrom`
        ( gridTemplateColumns :=
            lineName "shoulder-flavor"
            /\ repeat 2 (fr 1.2 /\ fitContent (em 187.1))
        )

      "grid-template-columns:repeat(auto-fill,minmax(404.2px,min-content) 91.6%)"
        `isRenderedFrom`
        ( gridTemplateColumns :=
            repeat autoFill $ minmax (px 404.2) minContent /\ pct 91.6
        )

      "grid-template-columns:repeat(2,[fetch-silent] minmax(24.2%,28%) minmax(auto,min-content))"
        `isRenderedFrom`
        ( gridTemplateColumns :=
            repeat 2
            ( lineName "fetch-silent"
              /\ minmax (pct 24.2) (pct 28)
              /\ minmax auto minContent
            )
        )

      "grid-template-columns:minmax(14.2%,max-content)"
        `isRenderedFrom`
        (gridTemplateColumns := minmax (pct 14.2) maxContent)

      "grid-template-columns:[spot] repeat(4,[idea-forum] 80% fit-content(55vw))"
        `isRenderedFrom`
        ( gridTemplateColumns :=
            lineName "spot"
            /\ repeat 4 (lineName "idea-forum" /\ pct 80 /\ fitContent (vw 55))
        )

      "grid-template-columns:[salmon-crop among parent] repeat(auto-fill,446.1px 310.9px)"
        `isRenderedFrom`
        ( gridTemplateColumns :=
            lineName "salmon-crop"
            /\ lineName "among"
            /\ lineName "parent"
            /\ repeat autoFill (px 446.1 /\ px 310.9)
        )

    describe "grid-template-rows property" do

      "grid-template-rows:inherit"
        `isRenderedFrom`
        (gridTemplateRows := inherit)

      "grid-template-rows:initial"
        `isRenderedFrom`
        (gridTemplateRows := initial)

      "grid-template-rows:unset"
        `isRenderedFrom`
        (gridTemplateRows := unset)

      "grid-template-rows:none"
        `isRenderedFrom`
        (gridTemplateRows := none)

      "grid-template-rows:[only helmet-exchange] 80.1% repeat(2,[online] minmax(24.6vw,358.9vw) minmax(55%,auto) minmax(92.3%,auto)) repeat(auto-fill,96.2% 20.7% [history]) repeat(2,minmax(75%,auto) [sock-dutch]) repeat(2,259em 11.9%)"
          `isRenderedFrom`
          ( gridTemplateRows :=
              lineName "only"
              /\ lineName "helmet-exchange"
              /\ pct 80.1
              /\ repeat 2
                 ( lineName "online"
                   /\ minmax (vw 24.6) (vw 358.9)
                   /\ minmax (pct 55) auto
                   /\ minmax (pct 92.3) auto
                 )
              /\ repeat autoFill (pct 96.2 /\ pct 20.7 /\ lineName "history")
              /\ repeat 2 (minmax (pct 75) auto /\ lineName "sock-dutch") 
              /\ repeat 2 (em 259 /\ pct 11.9)
          )

      "grid-template-rows:minmax(396em,max-content) repeat(auto-fill,354.8em [crash]) [melt chief]"
          `isRenderedFrom`
          ( gridTemplateRows :=
              minmax (em 396) maxContent
              /\ repeat autoFill (em 354.8 /\ lineName "crash")
              /\ lineName "melt"
              /\ lineName "chief"
          )

      "grid-template-rows:repeat(2,minmax(69.7%,auto) minmax(169.6px,2fr)) [square copper-venture] repeat(auto-fill,[device-banana] 15% minmax(57%,auto) [tiger-desk legal-sign stereo-rocket]) [giant-kid]"
          `isRenderedFrom`
          ( gridTemplateRows :=
              repeat 2 (minmax (pct 69.7) auto /\ minmax (px 169.6) (fr 2))
              /\ lineName "square"
              /\ lineName "copper-venture"
              /\ repeat autoFill 
                 ( lineName "device-banana"
                   /\ pct 15
                   /\ minmax (pct 57) auto
                   /\ lineName "tiger-desk"
                   /\ lineName "legal-sign"
                   /\ lineName "stereo-rocket"
                 )
              /\ lineName "giant-kid"
          )

      "grid-template-rows:minmax(max-content,2fr)"
          `isRenderedFrom`
          (gridTemplateRows := minmax maxContent $ fr 2)

      "grid-template-rows:repeat(1,477vw 7.2% [dumb-anger]) repeat(auto-fit,297.5px minmax(43.3%,auto))"
          `isRenderedFrom`
          ( gridTemplateRows :=
              repeat 1 (vw 477 /\ pct 7.2 /\ lineName "dumb-anger")
              /\ repeat autoFit (px 297.5 /\ minmax (pct 43.3) auto)
          )

      "grid-template-rows:min-content"
          `isRenderedFrom`
          (gridTemplateRows := minContent)

      "grid-template-rows:repeat(3,minmax(31.4%,auto) [stable-small]) repeat(auto-fit,minmax(40.8%,max-content) minmax(129.6vw,154px) [noble]) minmax(447.9px,min-content)"
          `isRenderedFrom`
          ( gridTemplateRows :=
              repeat 3 (minmax (pct 31.4) auto /\ lineName "stable-small")
              /\ repeat autoFit
                 ( minmax (pct 40.8) maxContent
                   /\ minmax (vw 129.6) (px 154)
                   /\ lineName "noble"
                 )
              /\ minmax (px 447.9) minContent
          )

      "grid-template-rows:fit-content(491em)"
          `isRenderedFrom`
          (gridTemplateRows := fitContent $ em 491)

      "grid-template-rows:minmax(54%,40.4%) repeat(1,378.4em minmax(40.9%,max-content)) repeat(auto-fill,[inner-uphold] minmax(450.8px,auto) minmax(96.1em,1.9fr)) [usual-brave] repeat(2,minmax(456.8em,max-content) 127.6px) repeat(1,184vw 11.9%)"
          `isRenderedFrom`
          ( gridTemplateRows :=
              minmax (pct 54) (pct 40.4)
              /\ repeat 1 (em 378.4 /\ minmax (pct 40.9) maxContent)
              /\ repeat autoFill
                 ( lineName "inner-uphold"
                   /\ minmax (px 450.8) auto
                   /\ minmax (em 96.1) (fr 1.9)
                 )
              /\ lineName "usual-brave"
              /\ repeat 2 (minmax (em 456.8) maxContent /\ px 127.6)
              /\ repeat 1 (vw 184 /\ pct 11.9)
          )

      "grid-template-rows:[spirit-dawn either deliver] repeat(auto-fill,minmax(38.1%,auto) 434em) repeat(1,minmax(482.1em,24.9%) minmax(90.7%,min-content)) minmax(358.2px,max-content)"
          `isRenderedFrom`
          ( gridTemplateRows :=
              lineName "spirit-dawn"
              /\ lineName "either"
              /\ lineName "deliver"
              /\ repeat autoFill (minmax (pct 38.1) auto /\ em 434)
              /\ repeat 1
                 ( minmax (em 482.1) (pct 24.9)
                   /\ minmax (pct 90.7) minContent
                 )
              /\ minmax (px 358.2) maxContent
          )

      "grid-template-rows:repeat(4,min-content minmax(max-content,48.4%) fit-content(63.7%))"
          `isRenderedFrom`
          ( gridTemplateRows :=
              repeat 4
              ( minContent
                /\ minmax maxContent (pct 48.4)
                /\ fitContent (pct 63.7)
              )
          )

      "grid-template-rows:repeat(auto-fill,67.1% 75% minmax(95%,min-content) [unlock-already luggage]) [grocery-father]"
          `isRenderedFrom`
          ( gridTemplateRows :=
              repeat autoFill
              ( pct 67.1
                /\ pct 75
                /\ minmax (pct 95) minContent
                /\ lineName "unlock-already"
                /\ lineName "luggage"
              )
              /\ lineName "grocery-father"
          )

      "grid-template-rows:repeat(5,[toward-crystal] minmax(min-content,94.4%) fit-content(216.5vw) minmax(min-content,30.1%))"
          `isRenderedFrom`
          ( gridTemplateRows :=
              repeat 5
              ( lineName "toward-crystal"
                /\ minmax minContent (pct 94.4)
                /\ fitContent (vw 216.5)
                /\ minmax minContent (pct 30.1)
              )
          )

      "grid-template-rows:repeat(4,min-content minmax(max-content,2fr) minmax(min-content,max-content))"
          `isRenderedFrom`
          ( gridTemplateRows :=
              repeat 4
              ( minContent
                /\ minmax maxContent (fr 2)
                /\ minmax minContent maxContent
              )
          )

      "grid-template-rows:[shove] repeat(1,minmax(max-content,min-content) minmax(auto,336.2vw) [habit-type cool-bronze melody-trip])"
          `isRenderedFrom`
          ( gridTemplateRows :=
              lineName "shove"
              /\ repeat 1
                 ( minmax maxContent minContent
                   /\ minmax auto (vw 336.2)
                   /\ lineName "habit-type"
                   /\ lineName "cool-bronze"
                   /\ lineName "melody-trip"
                 )
          )

      "grid-template-rows:repeat(2,minmax(84.1%,min-content) 36%) repeat(1,24.9%) repeat(auto-fit,minmax(182.7em,min-content) 442.4em [unhappy aunt sunny])"
          `isRenderedFrom`
          ( gridTemplateRows :=
              repeat 2 (minmax (pct 84.1) minContent /\ pct 36)
              /\ repeat 1 (pct 24.9)
              /\ repeat autoFit
                 ( minmax (em 182.7) minContent
                   /\ em 442.4
                   /\ lineName "unhappy"
                   /\ lineName "aunt"
                   /\ lineName "sunny"
                 )
          )

      "grid-template-rows:[wedding-impose gloom] repeat(auto-fill,176.1px 483.8vw minmax(48.5%,auto))"
          `isRenderedFrom`
          ( gridTemplateRows :=
              lineName "wedding-impose"
              /\ lineName "gloom"
              /\ repeat autoFill
                 ( px 176.1
                   /\ vw 483.8
                   /\ minmax (pct 48.5) auto
                 )
          )

      "grid-template-rows:repeat(auto-fill,33.5vw) [garlic potato drill]"
          `isRenderedFrom`
          ( gridTemplateRows :=
              repeat autoFill (vw 33.5)
              /\ lineName "garlic"
              /\ lineName "potato"
              /\ lineName "drill"
          )

      "grid-template-rows:42% 52.3% repeat(auto-fit,52.6% [syrup]) 36.2% minmax(36.6%,auto) [fever]"
          `isRenderedFrom`
          ( gridTemplateRows :=
              pct 42
              /\ pct 52.3
              /\ repeat autoFit (pct 52.6 /\ lineName "syrup")
              /\ pct 36.2
              /\ minmax (pct 36.6) auto
              /\ lineName "fever"
          )

      "grid-template-rows:max-content"
          `isRenderedFrom`
          (gridTemplateRows := maxContent)

      "grid-template-rows:repeat(5,fit-content(92.4%) 55.7% [stomach-myself])"
          `isRenderedFrom`
          ( gridTemplateRows :=
              repeat 5
              ( fitContent (pct 92.4)
                /\ pct 55.7
                /\ lineName "stomach-myself"
              )
          )

      "grid-template-rows:[arch-smoke harvest link-common] repeat(2,minmax(max-content,51vw) auto fit-content(70.1%))"
          `isRenderedFrom`
          ( gridTemplateRows :=
              lineName "arch-smoke"
              /\ lineName "harvest"
              /\ lineName "link-common"
              /\ repeat 2
                 ( minmax maxContent (vw 51)
                   /\ auto
                   /\ fitContent (pct 70.1)
                 )
          )

      "grid-template-rows:repeat(auto-fill,minmax(89.1%,55.3px)) minmax(86.9%,1.1fr) minmax(224.8px,auto) [dose-spatial]"
          `isRenderedFrom`
          ( gridTemplateRows :=
              repeat autoFill (minmax (pct 89.1) (px 55.3))
              /\ minmax (pct 86.9) (fr 1.1)
              /\ minmax (px 224.8) auto
              /\ lineName "dose-spatial"
          )

      "grid-template-rows:repeat(3,[trap] minmax(400.1vw,244.6px) 58em minmax(69.2%,max-content)) repeat(3,419em 47%) repeat(auto-fill,[expose-focus curious-coconut stone-cigar] 54.3% minmax(9.6%,max-content) minmax(69%,119.4vw)) repeat(2,[buyer-stage] minmax(7%,9.6%) 62.8%)"
          `isRenderedFrom`
          ( gridTemplateRows :=
              repeat 3
              ( lineName "trap"
                /\ minmax (vw 400.1) (px 244.6)
                /\ em 58
                /\ minmax (pct 69.2) maxContent
              )
              /\ repeat 3 (em 419 /\ pct 47)
              /\ repeat autoFill
                 ( lineName "expose-focus"
                   /\ lineName "curious-coconut"
                   /\ lineName "stone-cigar"
                   /\ pct 54.3
                   /\ minmax (pct 9.6) maxContent
                   /\ minmax (pct 69) (vw 119.4)
                  )
               /\ repeat 2
                  ( lineName "buyer-stage"
                    /\ minmax (pct 7) (pct 9.6)
                    /\ pct 62.8
                  )
          )

      "grid-template-rows:[lottery omit-hockey] repeat(auto-fill,minmax(15.4%,min-content) minmax(492vw,max-content) 498.6px [taxi]) repeat(2,minmax(89.6%,65vw))"
          `isRenderedFrom`
          ( gridTemplateRows :=
              lineName "lottery"
              /\ lineName "omit-hockey"
              /\ repeat autoFill
                 ( minmax (pct 15.4) minContent
                   /\ minmax (vw 492) maxContent
                   /\ px 498.6
                   /\ lineName "taxi"
                 )
              /\ repeat 2 (minmax (pct 89.6) (vw 65))
          )

    describe "grid-auto-columns property" do

      "grid-auto-columns:inherit" `isRenderedFrom` (gridAutoColumns := inherit)

      "grid-auto-columns:initial" `isRenderedFrom` (gridAutoColumns := initial)

      "grid-auto-columns:unset" `isRenderedFrom` (gridAutoColumns := unset)

      "grid-auto-columns:100px" `isRenderedFrom` (gridAutoColumns := px 100)

      "grid-auto-columns:10%" `isRenderedFrom` (gridAutoColumns := pct 10)

      "grid-auto-columns:1fr" `isRenderedFrom` (gridAutoColumns := fr 1)

      "grid-auto-columns:min-content"
        `isRenderedFrom`
        (gridAutoColumns := minContent)

      "grid-auto-columns:max-content"
        `isRenderedFrom`
        (gridAutoColumns := maxContent)

      "grid-auto-columns:auto" `isRenderedFrom` (gridAutoColumns := auto)

      "grid-auto-columns:minmax(min-content,auto)"
        `isRenderedFrom`
        (gridAutoColumns := minmax minContent auto)

      "grid-auto-columns:fit-content(75%)"
        `isRenderedFrom`
        (gridAutoColumns := fitContent $ pct 75)

      "grid-auto-columns:fit-content(379.3vw) minmax(464.4em,227vw) fit-content(18.3%) 71.6%"
        `isRenderedFrom`
        ( gridAutoColumns :=
            fitContent (vw 379.3)
            /\ minmax (em 464.4) (vw 227)
            /\ fitContent (pct 18.3)
            /\ pct 71.6
        )

      "grid-auto-columns:fit-content(375.2px) minmax(auto,217.9em) max-content minmax(84.3%,auto)"
        `isRenderedFrom`
        ( gridAutoColumns :=
            fitContent (px 375.2)
            /\ minmax auto (em 217.9)
            /\ maxContent
            /\ minmax (pct 84.3) auto
        )

      "grid-auto-columns:fit-content(177px) minmax(auto,1fr) max-content max-content"
        `isRenderedFrom`
        ( gridAutoColumns :=
            fitContent (px 177)
            /\ minmax auto (fr 1)
            /\ maxContent
            /\ maxContent
        )

      "grid-auto-columns:minmax(max-content,max-content) fit-content(39.3%) minmax(max-content,80px) minmax(min-content,2fr)"
        `isRenderedFrom`
        ( gridAutoColumns :=
            minmax maxContent maxContent
            /\ fitContent (pct 39.3)
            /\ minmax maxContent (px 80)
            /\ minmax minContent (fr 2)
        )

      "grid-auto-columns:minmax(83.8%,auto) fit-content(36.1%)"
        `isRenderedFrom`
        (gridAutoColumns := minmax (pct 83.8) auto /\ fitContent (pct 36.1))

    describe "grid-auto-rows property" do

      "grid-auto-rows:inherit" `isRenderedFrom` (gridAutoRows := inherit)

      "grid-auto-rows:initial" `isRenderedFrom` (gridAutoRows := initial)

      "grid-auto-rows:unset" `isRenderedFrom` (gridAutoRows := unset)

      "grid-auto-rows:calc(25% - 4px)"
        `isRenderedFrom`
        (gridAutoRows := pct 25 @-@ px 4)

      "grid-auto-rows:2.5fr" `isRenderedFrom` (gridAutoRows := fr 2.5)

      "grid-auto-rows:min-content" `isRenderedFrom` (gridAutoRows := minContent)

      "grid-auto-rows:max-content" `isRenderedFrom` (gridAutoRows := maxContent)

      "grid-auto-rows:auto" `isRenderedFrom` (gridAutoRows := auto)

      "grid-auto-rows:minmax(100px,2fr)"
        `isRenderedFrom`
        (gridAutoRows := minmax (px 100) (fr 2))

      "grid-auto-rows:fit-content(100px)"
        `isRenderedFrom`
        (gridAutoRows := fitContent $ px 100)

      "grid-auto-rows:fit-content(270.3vw) fit-content(87px)"
        `isRenderedFrom`
        (gridAutoRows := fitContent (vw 270.3) /\ fitContent (px 87))

      "grid-auto-rows:fit-content(318.8vw) fit-content(450.7px) fit-content(319.5em) minmax(min-content,max-content)"
        `isRenderedFrom`
        ( gridAutoRows :=
            fitContent (vw 318.8)
            /\ fitContent (px 450.7)
            /\ fitContent (em 319.5)
            /\ minmax minContent maxContent
        )

      "grid-auto-rows:min-content minmax(max-content,max-content) fit-content(235.8px) fit-content(22.6px)"
        `isRenderedFrom`
        ( gridAutoRows :=
            minContent
            /\ minmax maxContent maxContent
            /\ fitContent (px 235.8)
            /\ fitContent (px 22.6)
        )

      "grid-auto-rows:minmax(auto,2.6fr) minmax(max-content,min-content) fit-content(389.9em) minmax(max-content,1.5fr)"
        `isRenderedFrom`
        ( gridAutoRows :=
            minmax auto (fr 2.6)
            /\ minmax maxContent minContent
            /\ fitContent (em 389.9)
            /\ minmax maxContent (fr 1.5)
        )

      "grid-auto-rows:minmax(88.1%,min-content) auto min-content minmax(max-content,2.8fr)"
        `isRenderedFrom`
        ( gridAutoRows :=
            minmax (pct 88.1) minContent
            /\ auto
            /\ minContent
            /\ minmax maxContent (fr 2.8)
        )

    describe "grid-auto-flow property" do

      "grid-auto-flow:inherit" `isRenderedFrom` (gridAutoFlow := inherit)

      "grid-auto-flow:initial" `isRenderedFrom` (gridAutoFlow := initial)

      "grid-auto-flow:unset" `isRenderedFrom` (gridAutoFlow := unset)

      "grid-auto-flow:row" `isRenderedFrom` (gridAutoFlow := row)

      "grid-auto-flow:column" `isRenderedFrom` (gridAutoFlow := column)

      "grid-auto-flow:dense" `isRenderedFrom` (gridAutoFlow := dense)

      "grid-auto-flow:row dense" `isRenderedFrom` (gridAutoFlow := row ~ dense)

      "grid-auto-flow:column dense"
        `isRenderedFrom`
        (gridAutoFlow := column ~ dense)
