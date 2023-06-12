module Tecton.Internal
  ( (#+)
  , (#-)
  , ($=)
  , (&#)
  , (&.)
  , (&:)
  , (&::)
  , (&@)
  , (*=)
  , (*@)
  , (:/)
  , (:=)
  , (?)
  , (@*)
  , (@+@)
  , (@-@)
  , (@/)
  , (@=)
  , (^=)
  , (|*)
  , (|+)
  , (|=)
  , (|>)
  , (|~)
  , (~)
  , (~=)
  , Add
  , AnPlusB(..)
  , Angle
  , AttributePredicate
  , Auto
  , CSS
  , CSSColor
  , CommonKeyword
  , Configuration
  , Declaration'
  , Declarations
  , Divide
  , EasingFunction
  , ElementId(..)
  , Extensible
  , FitContent
  , Fixed
  , Flex
  , FontFace
  , FontFaceDeclaration'
  , FontFaceFormatFunction
  , Gradient
  , Inextensible
  , KeyframeBlock
  , Keyframes
  , KeyframesName(..)
  , Length
  , LengthPercentage
  , LineName(..)
  , LocalFunction
  , Measure
  , MediaQuery
  , Minmax'
  , Multiply
  , Names
  , Nil
  , NoAuto
  , Orientation
  , Pair(..)
  , Percentage
  , PseudoClass(..)
  , PseudoElement(..)
  , Ratio(..)
  , Repeat'
  , Repeating
  , Resolution
  , Selector
  , Statement
  , Subtract
  , Time
  , Track
  , TransformFunction
  , URL
  , Val
  , a
  , abbr
  , absolute
  , accept
  , acceptCharset
  , accesskey
  , acronym
  , action
  , active
  , add
  , adjacentSibling
  , after
  , alignContent
  , alignItems
  , alignSelf
  , alignmentBaseline
  , all
  , alphabetic
  , alt
  , alternate
  , alternateReverse
  , animationDelay
  , animationDirection
  , animationDuration
  , animationFillMode
  , animationIterationCount
  , animationName
  , animationPlayState
  , animationTimingFunction
  , anminusb
  , appearance
  , arabicIndic
  , armenian
  , article
  , aside
  , assoc
  , async
  , attContains
  , attElemWhitespace
  , attEndsWith
  , attEq
  , attStartsWith
  , attStartsWithHyphen
  , audio
  , auto
  , autoFill
  , autoFit
  , autocomplete
  , autofocus
  , autoplay
  , b
  , backgroundAttachment
  , backgroundClip
  , backgroundColor
  , backgroundImage
  , backgroundOrigin
  , backgroundPosition
  , backgroundRepeat
  , backgroundSize
  , backwards
  , baseline
  , baselineShift
  , baselineSource
  , before
  , bengali
  , blink
  , block
  , blockquote
  , body
  , bold
  , bolder
  , borderBottomColor
  , borderBottomLeftRadius
  , borderBottomRightRadius
  , borderBottomStyle
  , borderBottomWidth
  , borderBox
  , borderColor
  , borderLeftColor
  , borderLeftStyle
  , borderLeftWidth
  , borderRadius
  , borderRightColor
  , borderRightStyle
  , borderRightWidth
  , borderStyle
  , borderTopColor
  , borderTopLeftRadius
  , borderTopRightRadius
  , borderTopStyle
  , borderTopWidth
  , borderWidth
  , both
  , bottom
  , boxShadow
  , boxSizing
  , breakAll
  , breakSpaces
  , breakWord
  , button
  , byAtt
  , byClass
  , byId
  , byPseudoClass
  , byPseudoElement
  , cambodian
  , canvas
  , capitalize
  , caption
  , center
  , central
  , ch
  , charset
  , checked
  , child
  , circle
  , cite
  , cjkDecimal
  , cjkEarthlyBranch
  , cjkHeavenlyStem
  , class AlignmentBaselineKeyword
  , class AlignmentBaselineOrBaselineShiftKeyword
  , class AllPropertiesAnimatable
  , class AngleTag
  , class Animatable
  , class AnimationDirectionKeyword
  , class AnimationFillModeKeyword
  , class AnimationPlayStateKeyword
  , class AppearanceKeyword
  , class Assoc
  , class AttachmentKeyword
  , class AttrName
  , class AutoRepeatKeyword
  , class BaselineShiftKeyword
  , class BaselineSourceKeyword
  , class BoxKeyword
  , class BoxSizingKeyword
  , class ByAtt
  , class Calc
  , class ClearKeyword
  , class CollectMediaFeatures
  , class Combine
  , class ContentKeyword
  , class ContentPositionKeyword
  , class CounterStyleKeyword
  , class Declaration
  , class DirectionKeyword
  , class DisplayKeyword
  , class DominantBaselineKeyword
  , class Element
  , class ExtentKeyword
  , class FlexDirectionKeyword
  , class FlexWrapKeyword
  , class FloatKeyword
  , class FoldLineNames
  , class FontFaceDeclaration
  , class FontFaceFontStyleKeyword
  , class FontFaceFontWeightKeyword
  , class FontFormatKeyword
  , class FontSizeKeyword
  , class FontStretchKeyword
  , class FontStyleKeyword
  , class FontWeightKeyword
  , class GenericCursorKeyword
  , class GenericFontFamilyKeyword
  , class IsAnimationNameList
  , class IsAttachmentList
  , class IsAttrName
  , class IsBaselineShift
  , class IsBgImageList
  , class IsBgSize
  , class IsBgSizeList
  , class IsBorderColor
  , class IsBorderRadius
  , class IsBorderStyle
  , class IsBorderWidth
  , class IsBoxList
  , class IsColor
  , class IsColorStopListHead
  , class IsColorStopListTail
  , class IsCursorImage
  , class IsCursorList
  , class IsExtensibleSelector
  , class IsFontFaceFontStyle
  , class IsFontFaceFontWeight
  , class IsFontFaceSrcList
  , class IsFontFamilyList
  , class IsFontSize
  , class IsFontStretch
  , class IsFontStyle
  , class IsFontWeight
  , class IsImage
  , class IsInset
  , class IsInsetBlock
  , class IsLetterSpacing
  , class IsLineHeight
  , class IsLineWidth
  , class IsList
  , class IsListStyleImage
  , class IsListStyleType
  , class IsMargin
  , class IsMaskReferenceList
  , class IsMaxWidth
  , class IsMinWidth
  , class IsOverflow
  , class IsPadding
  , class IsPerspective
  , class IsPosition
  , class IsPositionList
  , class IsPositionX
  , class IsPositionY
  , class IsPseudoClass
  , class IsPseudoElement
  , class IsRadialGradientDimensions
  , class IsRepeatStyle
  , class IsRepeatStyleList
  , class IsSelector
  , class IsSelectorList
  , class IsShadow
  , class IsSingleAnimationDirectionList
  , class IsSingleAnimationFillModeList
  , class IsSingleAnimationIterationCountList
  , class IsSingleAnimationPlayStateList
  , class IsSingleBorderRadius
  , class IsSingleMargin
  , class IsSingleTransitionPropertyList
  , class IsTextDecorationLine
  , class IsTextShadow
  , class IsTextShadowList
  , class IsTextTransform
  , class IsTimeList
  , class IsTop
  , class IsTransformList
  , class IsTransformOrigin
  , class IsVerticalAlign
  , class IsWidth
  , class IsWordSpacing
  , class LengthPercentageTag
  , class LengthTag
  , class LineStyleKeyword
  , class LineWidthKeyword
  , class ListStylePositionKeyword
  , class MaxWidthKeyword
  , class MediaFeature
  , class MediaTypeKeyword
  , class MinWidthKeyword
  , class Minmax
  , class MkStatement
  , class MultiVal
  , class OutlineLineStyleKeyword
  , class OverflowKeyword
  , class OverflowPositionKeyword
  , class PercentageTag
  , class PositionKeyword
  , class Property
  , class Repeat
  , class RepeatStyle1dKeyword
  , class RepeatStyle2dKeyword
  , class RepeatTrackList
  , class SelfPositionKeyword
  , class ShapeKeyword
  , class StepPosition
  , class TextAlignKeyword
  , class TextDecorationStyleKeyword
  , class TextOverflowKeyword
  , class TextTransformCapitalizationKeyword
  , class TimeTag
  , class ToNumber
  , class ToVal
  , class TrackBreadthKeyword
  , class TrackCompat
  , class TrackList
  , class VisibilityKeyword
  , class WhiteSpaceKeyword
  , class WidthKeyword
  , class WordBreakKeyword
  , class'
  , clear
  , clip
  , closestCorner
  , closestSide
  , cm
  , code
  , col
  , colgroup
  , collapse
  , collectMediaFeatures
  , collection
  , color
  , cols
  , colspan
  , column
  , columnGap
  , columnReverse
  , combine
  , compact
  , condensed
  , contain
  , content
  , contentBox
  , contenteditable
  , contents
  , controls
  , coords
  , cover
  , cubicBezier
  , currentColor
  , cursive
  , cursor
  , dashed
  , data'
  , datetime
  , dd
  , decimal
  , decimalLeadingZero
  , default
  , defer
  , deg
  , dense
  , descendant
  , details
  , devanagari
  , dir
  , direction
  , dirname
  , disabled
  , disc
  , disclosureClosed
  , disclosureOpen
  , display
  , div
  , divide
  , dl
  , dominantBaseline
  , dotted
  , double
  , download
  , dpcm
  , dpi
  , draggable
  , dt
  , ease
  , easeIn
  , easeInOut
  , easeOut
  , ellipse
  , ellipsis
  , em
  , em'
  , embeddedOpentype
  , emoji
  , empty
  , enabled
  , enctype
  , end
  , even
  , ex
  , expanded
  , extraCondensed
  , extraExpanded
  , fangsong
  , fantasy
  , farthestCorner
  , farthestSide
  , fdval
  , fieldset
  , first
  , firstChild
  , firstLetter
  , firstLine
  , firstOfType
  , fitContent
  , fixed
  , flex
  , flexBasis
  , flexDirection
  , flexEnd
  , flexGrow
  , flexShrink
  , flexStart
  , flexWrap
  , float
  , flowRoot
  , focus
  , focusWithin
  , foldLineNames
  , foldlMultiVal
  , fontFace
  , fontFamily
  , fontSize
  , fontSizeAdjust
  , fontStretch
  , fontStyle
  , fontWeight
  , footer
  , for
  , form
  , formaction
  , format
  , forwards
  , fr
  , fullSizeKana
  , fullWidth
  , gap
  , generalSibling
  , georgian
  , grid
  , gridAutoColumns
  , gridAutoFlow
  , gridAutoRows
  , gridColumnEnd
  , gridColumnStart
  , gridRowEnd
  , gridRowStart
  , gridTemplateColumns
  , gridTemplateRows
  , groove
  , gujarati
  , gurmukhi
  , h1
  , h2
  , h3
  , h4
  , h5
  , h6
  , hanging
  , header
  , headers
  , hebrew
  , height
  , hidden
  , high
  , hiragana
  , hiraganaIroha
  , hover
  , hr
  , href
  , hreflang
  , html
  , httpEquiv
  , i
  , id
  , ideographic
  , img
  , inch
  , indeterminate
  , infinite
  , inherit
  , initial
  , inline
  , inlineBlock
  , inlineFlex
  , inlineGrid
  , inlineTable
  , input
  , inset
  , insetBlock
  , insetBlockEnd
  , insetBlockStart
  , insetInline
  , insetInlineEnd
  , insetInlineStart
  , inside
  , invert
  , ismap
  , italic
  , jumpBoth
  , jumpEnd
  , jumpNone
  , jumpStart
  , justify
  , justifyAll
  , justifyContent
  , justifyItems
  , justifySelf
  , kannada
  , katakana
  , katakanaIroha
  , keepAll
  , keyframes
  , khmer
  , kind
  , label
  , landscape
  , lang
  , lang'
  , lao
  , large
  , larger
  , last
  , lastChild
  , lastOfType
  , left
  , legacy
  , legend
  , letterSpacing
  , li
  , lighter
  , line
  , lineHeight
  , lineName
  , lineThrough
  , linear
  , linearGradient
  , link
  , list
  , listItem
  , listStyleImage
  , listStylePosition
  , listStyleType
  , local
  , local'
  , loop
  , low
  , lowerAlpha
  , lowerArmenian
  , lowerGreek
  , lowerLatin
  , lowerRoman
  , lowercase
  , ltr
  , main'
  , malayalam
  , mapVal
  , margin
  , marginBottom
  , marginLeft
  , marginRight
  , marginTop
  , mark
  , marker
  , maskImage
  , matchParent
  , math
  , mathematical
  , matrix
  , matrix3d
  , max
  , maxContent
  , maxHeight
  , maxWidth
  , maxlength
  , media
  , media'
  , medium
  , menu
  , menulistButton
  , method
  , middle
  , min
  , minContent
  , minHeight
  , minWidth
  , minmax
  , mkStatement
  , mm
  , mongolian
  , monospace
  , ms
  , multiple
  , multiply
  , multiplyFlipped
  , muted
  , myanmar
  , name
  , nav
  , nil
  , noRepeat
  , none
  , normal
  , not
  , novalidate
  , nowrap
  , nthChild
  , nthLastChild
  , nthOfType
  , number
  , oblique
  , odd
  , ol
  , onabort
  , onafterprint
  , onbeforeprint
  , onbeforeunload
  , onblur
  , oncanplay
  , oncanplaythrough
  , onchange
  , onclick
  , oncontextmenu
  , oncopy
  , oncuechange
  , oncut
  , ondblclick
  , ondrag
  , ondragend
  , ondragenter
  , ondragleave
  , ondragover
  , ondragstart
  , ondrop
  , ondurationchange
  , onemptied
  , onended
  , onerror
  , onfocus
  , onhashchange
  , oninput
  , oninvalid
  , onkeydown
  , onkeypress
  , onkeyup
  , onload
  , onloadeddata
  , onloadedmetadata
  , onloadstart
  , onlyChild
  , onlyOfType
  , onmousedown
  , onmousemove
  , onmouseout
  , onmouseover
  , onmouseup
  , onmousewheel
  , onoffline
  , ononline
  , onpagehide
  , onpageshow
  , onpaste
  , onpause
  , onplay
  , onplaying
  , onpopstate
  , onprogress
  , onratechange
  , onreset
  , onresize
  , onscroll
  , onsearch
  , onseeked
  , onseeking
  , onselect
  , onstalled
  , onstorage
  , onsubmit
  , onsuspend
  , ontimeupdate
  , ontoggle
  , onunload
  , onvolumechange
  , onwaiting
  , onwheel
  , opacity
  , open
  , opentype
  , optgroup
  , optimum
  , option
  , order
  , oriya
  , outlineColor
  , outlineOffset
  , outlineStyle
  , outlineWidth
  , outset
  , outside
  , overflow
  , overflowX
  , overflowY
  , overline
  , p
  , padding
  , paddingBottom
  , paddingBox
  , paddingLeft
  , paddingRight
  , paddingTop
  , path
  , pattern
  , paused
  , pc
  , pct
  , persian
  , perspective
  , placeholder
  , polygon
  , polyline
  , portrait
  , position
  , poster
  , pre
  , preLine
  , preWrap
  , preload
  , pretty
  , print
  , progress
  , pt
  , pval
  , px
  , q
  , rad
  , radialGradient
  , readonly
  , rect
  , rel
  , relative
  , rem
  , renderInline
  , renderInline'
  , renderSheet
  , repeat
  , repeat'
  , repeatX
  , repeatY
  , repeating
  , required
  , reverse
  , reversed
  , ridge
  , right
  , role
  , root
  , rotate
  , rotate3d
  , rotateX
  , rotateY
  , rotateZ
  , round
  , row
  , rowGap
  , rowReverse
  , rows
  , rowspan
  , rtl
  , runVal
  , running
  , safe
  , sandbox
  , sansSerif
  , scale
  , scale3d
  , scaleX
  , scaleY
  , scaleZ
  , scope
  , screen
  , scroll
  , sec
  , section
  , select
  , selected
  , selection
  , selfEnd
  , selfStart
  , semiCondensed
  , semiExpanded
  , serif
  , shape
  , size
  , sizes
  , skewX
  , skewY
  , small
  , smaller
  , solid
  , space
  , spaceAround
  , spaceBetween
  , spaceEvenly
  , span
  , spellcheck
  , square
  , src
  , srcdoc
  , srclang
  , srcset
  , start
  , static
  , step
  , stepEnd
  , stepStart
  , steps
  , sticky
  , stretch
  , strong
  , style
  , sub
  , subtract
  , summary
  , sup
  , super
  , svg
  , systemUI
  , tabindex
  , table
  , tableCaption
  , tableCell
  , tableColumn
  , tableColumnGroup
  , tableFooterGroup
  , tableHeaderGroup
  , tableRow
  , tableRowGroup
  , tamil
  , target
  , tbody
  , td
  , telugu
  , textAlign
  , textBottom
  , textDecorationColor
  , textDecorationLine
  , textDecorationStyle
  , textIndent
  , textOverflow
  , textShadow
  , textTop
  , textTransform
  , textarea
  , textfield
  , tfoot
  , th
  , thai
  , thead
  , thick
  , thin
  , tibetan
  , time
  , title
  , top
  , tr
  , transform
  , transformOrigin
  , transitionDelay
  , transitionDuration
  , transitionProperty
  , transitionTimingFunction
  , translate
  , translate'
  , translate3d
  , translateX
  , translateY
  , translateZ
  , transparent
  , truetype
  , turn
  , type'
  , uiMonospace
  , uiRounded
  , uiSansSerif
  , uiSerif
  , ul
  , ultraCondensed
  , ultraExpanded
  , underline
  , universal
  , unsafe
  , unsafeDeclaration
  , unset
  , upperAlpha
  , upperArmenian
  , upperLatin
  , upperRoman
  , uppercase
  , url
  , usemap
  , val
  , value
  , verticalAlign
  , vh
  , video
  , visibility
  , visible
  , visited
  , vmax
  , vmin
  , vw
  , wavy
  , whiteSpace
  , width
  , woff
  , woff2
  , wordBreak
  , wordSpacing
  , wrap
  , wrapReverse
  , xLarge
  , xSmall
  , xxLarge
  , xxSmall
  , zIndex
  , contextMenu
  , help
  , pointer
  , wait
  , cell
  , crosshair
  , text
  , verticalText
  , alias
  , copy
  , move
  , noDrop
  , notAllowed
  , grab
  , grabbing
  , eResize
  , nResize
  , neResize
  , nwResize
  , sResize
  , seResize
  , swResize
  , wResize
  , ewResize
  , nsResize
  , neswResize
  , nwseResize
  , colResize
  , rowResize
  , allScroll
  , zoomIn
  , zoomOut
  ) where

import Prelude hiding (add, bottom, sub, top)

import Color (Color, cssStringHSLA, toHexString)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Either (Either(..))
import Data.Either as Either
import Data.Either.Nested (type (\/))
import Data.Foldable (foldl, intercalate)
import Data.Int as Int
import Data.List (List(..), (:))
import Data.Number.Format as Number
import Data.Ord (abs)
import Data.String as String
import Data.String.Regex (regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (global)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (curry, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.TypeError (class Warn, Text)
import Record as Record
import Type.Proxy (Proxy(..))
import Web.HTML.Common (AttrName(..), ClassName(..))

--------------------------------------------------------------------------------

-- Utilities

foreign import quote :: String -> String

camelToKebab :: String -> String
camelToKebab s =
  case regex "[A-Z]" global of
    Either.Left _ ->
      s
    Either.Right caps ->
      Regex.replace' caps (const <<< ("-" <> _) <<< String.toLower) s

class IsList (x :: Type) (xs :: Type)

instance IsList x xs => IsList x (x /\ xs)
else instance IsList x x

--------------------------------------------------------------------------------

-- Configuration

type Configuration =
  { newline :: String
  , indentation :: String
  , indentLevel :: Int
  , separator :: String
  , finalSemicolon :: Boolean
  , color :: Color -> String
  }

pretty :: Configuration
pretty =
  { newline: "\n"
  , indentation: "  "
  , indentLevel: 0
  , separator: " "
  , finalSemicolon: true
  , color: cssStringHSLA
  }

compact :: Configuration
compact =
  { newline: ""
  , indentation: ""
  , indentLevel: 0
  , separator: ""
  , finalSemicolon: false
  , color: toHexString
  }

--------------------------------------------------------------------------------

-- General values

newtype Val = Val (Configuration -> String)

instance Semigroup Val where
  append (Val f) (Val g) = Val \x -> f x <> g x

instance Monoid Val where
  mempty = val ""

mapVal :: (String -> String) -> Val -> Val
mapVal f v = Val \c -> f $ runVal c v

class ToVal (a :: Type) where
  val :: a -> Val

instance ToVal Val where
  val = identity

instance ToVal String where
  val = Val <<< const

instance ToVal Number where
  val = val <<< Number.toString

instance ToVal Int where
  val = val <<< Int.toNumber

instance IsSymbol a => ToVal (Proxy a) where
  val = val <<< reflectSymbol

instance (ToVal a, ToVal b) => ToVal (Pair a b) where
  val (a' ~ b') = val a' <> val " " <> val b'

class MultiVal (xs :: Type) where
  foldlMultiVal :: forall acc. (acc -> Val -> acc) -> acc -> xs -> acc

instance multiValImplXXS :: (ToVal x, MultiVal xs) => MultiVal (x /\ xs) where
  foldlMultiVal f acc (x /\ xs) = foldlMultiVal f (f acc $ val x) xs

else instance multiValImplX :: ToVal x => MultiVal x where
  foldlMultiVal f acc x = f acc $ val x

intercalateMultiVal
  :: forall sep xs
   . ToVal sep
  => MultiVal xs
  => sep
  -> xs
  -> Val
intercalateMultiVal sep =
  snd <<< foldlMultiVal combine (false /\ mempty)
  where
  combine (needsSep /\ a') b' =
    true /\ (a' <> (if needsSep then val sep else mempty) <> b')

runVal :: Configuration -> Val -> String
runVal x (Val f) = f x

--------------------------------------------------------------------------------

-- Statements

type CSS = Writer (List Statement) Unit

data Statement = Statement Val (Val \/ (List Statement))

class MkStatement (a :: Type) (b :: Type) (c :: Type) | a -> b where
  mkStatement :: a -> b -> Writer (List c) Unit

instance MkStatement MediaQuery (Writer (List Statement) Unit) Statement where
  mkStatement mq nested =
    tell
      $ pure
      $ Statement (val "@" <> val mq)
      $ Right
      $ execWriter nested

else instance
  MkStatement FontFace
    ( Writer (List FontFaceDeclaration')
        (Proxy ("font-family" :: a, "src" :: b | descriptors))
    )
    Statement where
  mkStatement _ decls =
    tell
      $ pure
      $ Statement (val "@font-face")
      $ Left
      $ concatDeclarations
      $ map (\(FontFaceDeclaration' d) -> d)
      $ execWriter decls

else instance
  MkStatement Keyframes (Writer (List KeyframeBlock) Unit) Statement where
  mkStatement (Keyframes (KeyframesName kfname)) blocks =
    tell
      $ pure
      $ Statement (val "@keyframes " <> val kfname)
      $ Right
      $
        ( (\(KeyframeBlock sel decls) -> Statement sel $ Left decls) <$>
            execWriter
              blocks
        )

else instance
  ( RowToList ps psl
  , AllPropertiesAnimatable psl
  ) =>
  MkStatement (Measure Percentage)
    (Writer (List Declaration') (Proxy ps))
    KeyframeBlock where
  mkStatement sel decls =
    tell
      $ pure
      $ KeyframeBlock (val sel)
      $ concatDeclarations
      $ map (\(Declaration' d) -> d)
      $ execWriter decls

else instance
  ( RowToList ps psl
  , AllPropertiesAnimatable psl
  , IsList (Measure Percentage) xs
  , MultiVal xs
  ) =>
  MkStatement (Measure Percentage /\ xs)
    (Writer (List Declaration') (Proxy ps))
    KeyframeBlock where
  mkStatement sels decls =
    tell
      $ pure
      $ KeyframeBlock (intercalateMultiVal (val "," <> Val _.separator) sels)
      $ concatDeclarations
      $ map (\(Declaration' d) -> d)
      $ execWriter decls

else instance
  ( IsSelectorList selectors
  , MultiVal selectors
  ) =>
  MkStatement selectors (Writer (List Declaration') ps) Statement where
  mkStatement sel decls =
    tell
      $ pure
      $ Statement (intercalateMultiVal (val "," <> Val _.separator) sel)
      $ Left
      $ concatDeclarations
      $ map (\(Declaration' d) -> d)
      $ execWriter decls

infixr 0 mkStatement as ?

--------------------------------------------------------------------------------

-- Declarations

type Declarations ps = Writer (List Declaration') ps

newtype Declaration' = Declaration' (Val /\ Val)

newtype FontFaceDeclaration' = FontFaceDeclaration' (Val /\ Val)

class Assoc (k :: Symbol) v w (k' :: Row Type) | k -> k' where
  assoc :: Proxy k -> v -> Writer (List w) (Proxy k')

instance
  ( IsSymbol p
  , Property p
  , Row.Cons p CommonKeyword () p'
  ) =>
  Assoc p CommonKeyword Declaration' p' where
  assoc _ v =
    let
      prop = Proxy :: _ p
    in
      tell (pure $ Declaration' $ val prop /\ val v) *> pure Proxy
else instance
  ( IsSymbol p
  , Declaration p v
  , Row.Cons p v () p'
  ) =>
  Assoc p v Declaration' p' where
  assoc _ v =
    let
      prop = Proxy :: _ p
    in
      tell (pure $ Declaration' $ val prop /\ pval prop v) *> pure Proxy

instance
  ( IsSymbol d
  , FontFaceDeclaration d v
  , Row.Cons d v () d'
  ) =>
  Assoc d v FontFaceDeclaration' d' where
  assoc _ v =
    let
      desc = Proxy :: _ d
    in
      tell (pure $ FontFaceDeclaration' $ val desc /\ fdval desc v) *> pure
        Proxy

infixr 0 assoc as :=

-- | Adds a declaration to a rule. The first parameter is the name of the
-- | property, while the second parameter is the corresponding value as it would
-- | be written directly in CSS.
unsafeDeclaration :: String -> String -> Writer (List Declaration') (Proxy ())
unsafeDeclaration p' v' =
  tell (pure $ Declaration' $ val p' /\ val v') *> pure Proxy

concatDeclarations :: List (Val /\ Val) -> Val
concatDeclarations decls = Val \c ->
  let
    mkIndent n acc
      | n < 1 = acc
      | otherwise = mkIndent (n - 1) $ c.indentation <> acc
    indent = mkIndent c.indentLevel mempty
    go Nil acc = acc <> if c.finalSemicolon then ";" else mempty
    go ((k /\ v) : xs) acc =
      go xs $ (if acc /= mempty then acc <> ";" <> c.newline else mempty)
        <> indent
        <> runVal c k
        <> ":"
        <> c.separator
        <> runVal c v
  in
    go decls mempty

class Property (p :: Symbol)

class Property p <= Declaration (p :: Symbol) (v :: Type) where
  pval :: Proxy p -> v -> Val

--------------------------------------------------------------------------------

-- Rendering

renderInline'
  :: forall ps
   . Configuration
  -> Writer (List Declaration') ps
  -> String
renderInline' c =
  runVal c
    <<< concatDeclarations
    <<< map (\(Declaration' d) -> d)
    <<< execWriter

renderInline :: forall ps. Writer (List Declaration') ps -> String
renderInline = renderInline' pretty { newline = " ", finalSemicolon = false }

renderSheet :: Configuration -> Writer (List Statement) Unit -> String
renderSheet config =
  runVal config
    <<< intercalate (val config.newline)
    <<< map renderStatement
    <<< execWriter

  where

  renderStatement (Statement outer inner) =
    case inner of
      Left inner' ->
        nested outer inner'
      Right inner' ->
        nested outer $
          intercalate (val config.newline) (renderStatement <$> inner')

  nested outer inner =
    Val \c@{ indentLevel, indentation, newline, separator } ->
      let
        mkIndent n acc
          | n < 1 = acc
          | otherwise = mkIndent (n - 1) $ indentation <> acc
        indent = mkIndent indentLevel mempty
      in
        indent
          <> runVal c outer
          <> separator
          <> "{"
          <> newline
          <> runVal (c { indentLevel = indentLevel + 1 }) inner
          <> newline
          <> indent
          <> "}"

--------------------------------------------------------------------------------

-- Box Alignment
-- https://www.w3.org/TR/css-align-3/

-- https://www.w3.org/TR/css-align-3/#propdef-justify-content

justifyContent = Proxy :: Proxy "justify-content"

instance Property "justify-content"

normal = Proxy :: Proxy "normal"

spaceAround = Proxy :: Proxy "space-around"
spaceBetween = Proxy :: Proxy "space-between"
spaceEvenly = Proxy :: Proxy "space-evenly"
stretch = Proxy :: Proxy "stretch"

safe = Proxy :: Proxy "safe"
unsafe = Proxy :: Proxy "unsafe"

class OverflowPositionKeyword (s :: Symbol)

instance OverflowPositionKeyword "safe"
instance OverflowPositionKeyword "unsafe"

class ContentPositionKeyword (s :: Symbol)

instance ContentPositionKeyword "center"
instance ContentPositionKeyword "start"
instance ContentPositionKeyword "end"
instance ContentPositionKeyword "flex-start"
instance ContentPositionKeyword "flex-end"

instance declarationJustifyContentOverflowPositionKeywordLeft ::
  ( OverflowPositionKeyword s
  , IsSymbol s
  ) =>
  Declaration "justify-content" (Proxy s ~ Proxy "left") where
  pval = const val

else instance declarationJustifyContentOverflowPositionKeywordRight ::
  ( OverflowPositionKeyword s
  , IsSymbol s
  ) =>
  Declaration "justify-content" (Proxy s ~ Proxy "right") where
  pval = const val

else instance declarationJustifyContentOverflowPositionKeywordContentPositionKeyword ::
  ( OverflowPositionKeyword sa
  , IsSymbol sa
  , ContentPositionKeyword sb
  , IsSymbol sb
  ) =>
  Declaration "justify-content" (Proxy sa ~ Proxy sb) where
  pval = const val

instance declarationJustifyContentNormal ::
  Declaration "justify-content" (Proxy "normal") where
  pval = const val

else instance declarationJustifyContentSpaceBetween ::
  Declaration "justify-content" (Proxy "space-between") where
  pval = const val

else instance declarationJustifyContentSpaceAround ::
  Declaration "justify-content" (Proxy "space-around") where
  pval = const val

else instance declarationJustifyContentSpaceEvenly ::
  Declaration "justify-content" (Proxy "space-evenly") where
  pval = const val

else instance declarationJustifyContentStretch ::
  Declaration "justify-content" (Proxy "stretch") where
  pval = const val

else instance declarationJustifyContentLeft ::
  Declaration "justify-content" (Proxy "left") where
  pval = const val

else instance declarationJustifyContentRight ::
  Declaration "justify-content" (Proxy "right") where
  pval = const val

else instance declarationJustifyContentContentPositionKeyword ::
  ( ContentPositionKeyword s
  , IsSymbol s
  ) =>
  Declaration "justify-content" (Proxy s) where
  pval = const val

-- https://www.w3.org/TR/css-align-3/#propdef-align-content

alignContent = Proxy :: Proxy "align-content"

instance Property "align-content"

instance declarationAlignContentFirstBaseline ::
  Declaration "align-content" (Proxy "first" ~ Proxy "baseline") where
  pval = const val

else instance declarationAlignContentLastBaseline ::
  Declaration "align-content" (Proxy "last" ~ Proxy "baseline") where
  pval = const val

else instance declarationAlignContentOverflowPositionKeywordContentPositionKeyword ::
  ( OverflowPositionKeyword sa
  , IsSymbol sa
  , ContentPositionKeyword sb
  , IsSymbol sb
  ) =>
  Declaration "align-content" (Proxy sa ~ Proxy sb) where
  pval = const val

instance declarationAlignContentNormal ::
  Declaration "align-content" (Proxy "normal") where
  pval = const val

else instance declarationAlignContentBaseline ::
  Declaration "align-content" (Proxy "baseline") where
  pval = const val

else instance declarationAlignContentSpaceBetween ::
  Declaration "align-content" (Proxy "space-between") where
  pval = const val

else instance declarationAlignContentSpaceAround ::
  Declaration "align-content" (Proxy "space-around") where
  pval = const val

else instance declarationAlignContentSpaceEvenly ::
  Declaration "align-content" (Proxy "space-evenly") where
  pval = const val

else instance declarationAlignContentStretch ::
  Declaration "align-content" (Proxy "stretch") where
  pval = const val

else instance declarationAlignContentContentPositionKeyword ::
  ( ContentPositionKeyword s
  , IsSymbol s
  ) =>
  Declaration "align-content" (Proxy s) where
  pval = const val

-- https://www.w3.org/TR/css-align-3/#propdef-justify-self

justifySelf = Proxy :: Proxy "justify-self"

instance Property "justify-self"

baseline = Proxy :: Proxy "baseline"

center = Proxy :: Proxy "center"
selfStart = Proxy :: Proxy "self-start"
selfEnd = Proxy :: Proxy "self-end"
flexStart = Proxy :: Proxy "flex-start"
flexEnd = Proxy :: Proxy "flex-end"

class SelfPositionKeyword (s :: Symbol)

instance SelfPositionKeyword "center"
instance SelfPositionKeyword "start"
instance SelfPositionKeyword "end"
instance SelfPositionKeyword "self-start"
instance SelfPositionKeyword "self-end"
instance SelfPositionKeyword "flex-start"
instance SelfPositionKeyword "flex-end"

instance declarationJustifySelfFirstBaseline ::
  Declaration "justify-self" (Proxy "first" ~ Proxy "baseline") where
  pval = const val

else instance declarationJustifySelfLastBaseline ::
  Declaration "justify-self" (Proxy "last" ~ Proxy "baseline") where
  pval = const val

else instance declarationJustifySelfOverflowPositionKeywordLeft ::
  ( OverflowPositionKeyword s
  , IsSymbol s
  ) =>
  Declaration "justify-self" (Proxy s ~ Proxy "left") where
  pval = const val

else instance declarationJustifySelfOverflowPositionKeywordRight ::
  ( OverflowPositionKeyword s
  , IsSymbol s
  ) =>
  Declaration "justify-self" (Proxy s ~ Proxy "right") where
  pval = const val

else instance declarationJustifySelfOverflowPositionKeywordSelfPositionKeyword ::
  ( OverflowPositionKeyword sa
  , IsSymbol sa
  , SelfPositionKeyword sb
  , IsSymbol sb
  ) =>
  Declaration "justify-self" (Proxy sa ~ Proxy sb) where
  pval = const val

instance declarationJustifySelfAuto :: Declaration "justify-self" (Proxy "auto") where
  pval = const val

else instance declarationJustifySelfNormal ::
  Declaration "justify-self" (Proxy "normal") where
  pval = const val

else instance declarationJustifySelfStretch ::
  Declaration "justify-self" (Proxy "stretch") where
  pval = const val

else instance declarationJustifySelfBaseline ::
  Declaration "justify-self" (Proxy "baseline") where
  pval = const val

else instance declarationJustifySelfLeft ::
  Declaration "justify-self" (Proxy "left") where
  pval = const val

else instance declarationJustifySelfRight ::
  Declaration "justify-self" (Proxy "right") where
  pval = const val

else instance declarationJustifySelfSelfPositionKeyword ::
  ( SelfPositionKeyword s
  , IsSymbol s
  ) =>
  Declaration "justify-self" (Proxy s) where
  pval = const val

-- https://www.w3.org/TR/css-align-3/#propdef-align-self

alignSelf = Proxy :: Proxy "align-self"

instance Property "align-self"

instance declarationAlignSelfFirstBaseline ::
  Declaration "align-self" (Proxy "first" ~ Proxy "baseline") where
  pval = const val

else instance declarationAlignSelfLastBaseline ::
  Declaration "align-self" (Proxy "last" ~ Proxy "baseline") where
  pval = const val

else instance declarationAlignSelfOverflowPositionKeywordLeft ::
  ( OverflowPositionKeyword s
  , IsSymbol s
  ) =>
  Declaration "align-self" (Proxy s ~ Proxy "left") where
  pval = const val

else instance declarationAlignSelfOverflowPositionKeywordRight ::
  ( OverflowPositionKeyword s
  , IsSymbol s
  ) =>
  Declaration "align-self" (Proxy s ~ Proxy "right") where
  pval = const val

else instance declarationAlignSelfOverflowPositionKeywordSelfPositionKeyword ::
  ( OverflowPositionKeyword sa
  , IsSymbol sa
  , SelfPositionKeyword sb
  , IsSymbol sb
  ) =>
  Declaration "align-self" (Proxy sa ~ Proxy sb) where
  pval = const val

instance declarationAlignSelfAuto :: Declaration "align-self" (Proxy "auto") where
  pval = const val

else instance declarationAlignSelfNormal ::
  Declaration "align-self" (Proxy "normal") where
  pval = const val

else instance declarationAlignSelfStretch ::
  Declaration "align-self" (Proxy "stretch") where
  pval = const val

else instance declarationAlignSelfBaseline ::
  Declaration "align-self" (Proxy "baseline") where
  pval = const val

else instance declarationAlignSelfSelfPositionKeyword ::
  ( SelfPositionKeyword s
  , IsSymbol s
  ) =>
  Declaration "align-self" (Proxy s) where
  pval = const val

-- https://www.w3.org/TR/css-align-3/#propdef-justify-items

justifyItems = Proxy :: Proxy "justify-items"

instance Property "justify-items"

legacy = Proxy :: Proxy "legacy"

instance declarationJustifyItemsFirstBaseline ::
  Declaration "justify-items" (Proxy "first" ~ Proxy "baseline") where
  pval = const val

else instance declarationJustifyItemsLastBaseline ::
  Declaration "justify-items" (Proxy "last" ~ Proxy "baseline") where
  pval = const val

else instance declarationJustifyItemsLegacyLeft ::
  Declaration "justify-items" (Proxy "legacy" ~ Proxy "left") where
  pval = const val

else instance declarationJustifyItemsLegacyRight ::
  Declaration "justify-items" (Proxy "legacy" ~ Proxy "right") where
  pval = const val

else instance declarationJustifyItemsLegacyCenter ::
  Declaration "justify-items" (Proxy "legacy" ~ Proxy "center") where
  pval = const val

else instance declarationJustifyItemsOverflowPositionKeywordLeft ::
  ( OverflowPositionKeyword s
  , IsSymbol s
  ) =>
  Declaration "justify-items" (Proxy s ~ Proxy "left") where
  pval = const val

else instance declarationJustifyItemsOverflowPositionKeywordRight ::
  ( OverflowPositionKeyword s
  , IsSymbol s
  ) =>
  Declaration "justify-items" (Proxy s ~ Proxy "right") where
  pval = const val

else instance declarationJustifyItemsOverflowPositionKeywordSelfPositionKeyword ::
  ( OverflowPositionKeyword sa
  , IsSymbol sa
  , SelfPositionKeyword sb
  , IsSymbol sb
  ) =>
  Declaration "justify-items" (Proxy sa ~ Proxy sb) where
  pval = const val

else instance declarationJustifyItemsNormal ::
  Declaration "justify-items" (Proxy "normal") where
  pval = const val

else instance declarationJustifyItemsStretch ::
  Declaration "justify-items" (Proxy "stretch") where
  pval = const val

else instance declarationJustifyItemsBaseline ::
  Declaration "justify-items" (Proxy "baseline") where
  pval = const val

else instance declarationJustifyItemsLeft ::
  Declaration "justify-items" (Proxy "left") where
  pval = const val

else instance declarationJustifyItemsRight ::
  Declaration "justify-items" (Proxy "right") where
  pval = const val

else instance declarationJustifyItemsLegacy ::
  Declaration "justify-items" (Proxy "legacy") where
  pval = const val

else instance declarationJustifyItemsSelfPositionKeyword ::
  ( SelfPositionKeyword s
  , IsSymbol s
  ) =>
  Declaration "justify-items" (Proxy s) where
  pval = const val

-- https://www.w3.org/TR/css-align-3/#propdef-align-items

alignItems = Proxy :: Proxy "align-items"

instance Property "align-items"

instance declarationAlignItemsFirstBaseline ::
  Declaration "align-items" (Proxy "first" ~ Proxy "baseline") where
  pval = const val

else instance declarationAlignItemsLastBaseline ::
  Declaration "align-items" (Proxy "last" ~ Proxy "baseline") where
  pval = const val

else instance declarationAlignItemsOverflowPositionKeywordLeft ::
  ( OverflowPositionKeyword s
  , IsSymbol s
  ) =>
  Declaration "align-items" (Proxy s ~ Proxy "left") where
  pval = const val

else instance declarationAlignItemsOverflowPositionKeywordRight ::
  ( OverflowPositionKeyword s
  , IsSymbol s
  ) =>
  Declaration "align-items" (Proxy s ~ Proxy "right") where
  pval = const val

else instance declarationAlignItemsOverflowPositionKeywordSelfPositionKeyword ::
  ( OverflowPositionKeyword sa
  , IsSymbol sa
  , SelfPositionKeyword sb
  , IsSymbol sb
  ) =>
  Declaration "align-items" (Proxy sa ~ Proxy sb) where
  pval = const val

instance declarationAlignItemsNormal ::
  Declaration "align-items" (Proxy "normal") where
  pval = const val

else instance declarationAlignItemsStretch ::
  Declaration "align-items" (Proxy "stretch") where
  pval = const val

else instance declarationAlignItemsBaseline ::
  Declaration "align-items" (Proxy "baseline") where
  pval = const val

else instance declarationAlignItemsSelfPositionKeyword ::
  ( SelfPositionKeyword s
  , IsSymbol s
  ) =>
  Declaration "align-items" (Proxy s) where
  pval = const val

-- https://www.w3.org/TR/css-align-3/#propdef-row-gap

rowGap = Proxy :: Proxy "row-gap"

instance Property "row-gap"
instance Animatable "row-gap"

instance declarationRowGapNormal :: Declaration "row-gap" (Proxy "normal") where
  pval = const val

instance declarationRowGapLengthPercentage ::
  LengthPercentageTag t =>
  Declaration "row-gap" (Measure t) where
  pval = const val

-- https://www.w3.org/TR/css-align-3/#propdef-column-gap

columnGap = Proxy :: Proxy "column-gap"

instance Property "column-gap"
instance Animatable "column-gap"

instance declarationColumnGapRowGap ::
  Declaration "row-gap" a =>
  Declaration "column-gap" a where
  pval = const $ pval rowGap

-- https://www.w3.org/TR/css-align-3/#propdef-gap

gap = Proxy :: Proxy "gap"

instance Property "gap"
instance Animatable "gap"

instance declarationGapRowColumn ::
  ( Declaration "row-gap" row
  , Declaration "column-gap" column
  ) =>
  Declaration "gap" (row ~ column) where
  pval _ (r ~ c) = pval rowGap r <> val " " <> pval columnGap c

else instance declarationGapRow ::
  Declaration "row-gap" a =>
  Declaration "gap" a where
  pval = const $ pval rowGap

--------------------------------------------------------------------------------

-- Animations
-- https://www.w3.org/TR/css-animations-1/

class Property p <= Animatable (p :: Symbol)

class AllPropertiesAnimatable (ps :: RowList Type)

instance AllPropertiesAnimatable RL.Nil
instance
  ( Animatable p
  , AllPropertiesAnimatable tail
  ) =>
  AllPropertiesAnimatable (RL.Cons p v tail)

newtype KeyframesName = KeyframesName String

derive newtype instance ToVal KeyframesName

newtype Keyframes = Keyframes KeyframesName

keyframes :: KeyframesName -> Keyframes
keyframes = Keyframes

data KeyframeBlock = KeyframeBlock Val Val

-- https://www.w3.org/TR/css-animations-1/#propdef-animation-name

animationName = Proxy :: Proxy "animation-name"

instance Property "animation-name"

class IsAnimationNameList (a :: Type)

instance IsAnimationNameList (Proxy "none")
instance IsAnimationNameList KeyframesName
instance IsAnimationNameList xs => IsAnimationNameList (Proxy "none" /\ xs)
instance IsAnimationNameList xs => IsAnimationNameList (KeyframesName /\ xs)

instance declarationAnimationName ::
  ( IsAnimationNameList a
  , MultiVal a
  ) =>
  Declaration "animation-name" a where
  pval = const $ intercalateMultiVal $ val "," <> Val _.separator

-- https://www.w3.org/TR/css-animations-1/#propdef-animation-duration

animationDuration = Proxy :: Proxy "animation-duration"

instance Property "animation-duration"

class IsTimeList (a :: Type)

instance (TimeTag tx, IsTimeList xs) => IsTimeList (Measure tx /\ xs)
instance TimeTag t => IsTimeList (Measure t)

instance declarationAnimationDuration ::
  ( IsTimeList a
  , MultiVal a
  ) =>
  Declaration "animation-duration" a where
  pval = const $ intercalateMultiVal $ val "," <> Val _.separator

-- https://www.w3.org/TR/css-animations-1/#propdef-animation-timing-function

animationTimingFunction = Proxy :: Proxy "animation-timing-function"

instance Property "animation-timing-function"

instance declarationAnimationTimingFunction ::
  ( IsList EasingFunction a
  , MultiVal a
  ) =>
  Declaration "animation-timing-function" a where
  pval = const $ intercalateMultiVal $ val "," <> Val _.separator

-- https://www.w3.org/TR/css-animations-1/#propdef-animation-iteration-count

animationIterationCount = Proxy :: Proxy "animation-iteration-count"

instance Property "animation-iteration-count"

infinite = Proxy :: Proxy "infinite"

class IsSingleAnimationIterationCountList (a :: Type)

instance
  IsSingleAnimationIterationCountList xs =>
  IsSingleAnimationIterationCountList (Int /\ xs)

instance
  IsSingleAnimationIterationCountList xs =>
  IsSingleAnimationIterationCountList (Proxy "infinite" /\ xs)

instance IsSingleAnimationIterationCountList Int
instance IsSingleAnimationIterationCountList (Proxy "infinite")

instance declarationAnimationIterationCount ::
  ( IsSingleAnimationIterationCountList a
  , MultiVal a
  ) =>
  Declaration "animation-iteration-count" a where
  pval = const $ intercalateMultiVal $ val "," <> Val _.separator

-- https://www.w3.org/TR/css-animations-1/#propdef-animation-direction

animationDirection = Proxy :: Proxy "animation-direction"

instance Property "animation-direction"

reverse = Proxy :: Proxy "reverse"
alternate = Proxy :: Proxy "alternate"
alternateReverse = Proxy :: Proxy "alternate-reverse"

class AnimationDirectionKeyword (s :: Symbol)

instance AnimationDirectionKeyword "normal"
instance AnimationDirectionKeyword "reverse"
instance AnimationDirectionKeyword "alternate"
instance AnimationDirectionKeyword "alternate-reverse"

class IsSingleAnimationDirectionList (a :: Type)

instance
  ( AnimationDirectionKeyword sx
  , IsSingleAnimationDirectionList xs
  ) =>
  IsSingleAnimationDirectionList (Proxy sx /\ xs)

instance AnimationDirectionKeyword s => IsSingleAnimationDirectionList (Proxy s)

instance declarationAnimationDirection ::
  ( IsSingleAnimationDirectionList a
  , MultiVal a
  ) =>
  Declaration "animation-direction" a where
  pval = const $ intercalateMultiVal $ val "," <> Val _.separator

-- https://www.w3.org/TR/css-animations-1/#propdef-animation-play-state

animationPlayState = Proxy :: Proxy "animation-play-state"

instance Property "animation-play-state"

running = Proxy :: Proxy "running"
paused = Proxy :: Proxy "paused"

class AnimationPlayStateKeyword (s :: Symbol)

instance AnimationPlayStateKeyword "running"
instance AnimationPlayStateKeyword "paused"

class IsSingleAnimationPlayStateList (a :: Type)

instance
  ( AnimationPlayStateKeyword sx
  , IsSingleAnimationPlayStateList xs
  ) =>
  IsSingleAnimationPlayStateList (Proxy sx /\ xs)

instance AnimationPlayStateKeyword s => IsSingleAnimationPlayStateList (Proxy s)

instance declarationAnimationPlayState ::
  ( IsSingleAnimationPlayStateList a
  , MultiVal a
  ) =>
  Declaration "animation-play-state" a where
  pval = const $ intercalateMultiVal $ val "," <> Val _.separator

-- https://www.w3.org/TR/css-animations-1/#propdef-animation-delay

animationDelay = Proxy :: Proxy "animation-delay"

instance Property "animation-delay"

instance declarationAnimationDelay ::
  ( IsTimeList a
  , MultiVal a
  ) =>
  Declaration "animation-delay" a where
  pval = const $ intercalateMultiVal $ val "," <> Val _.separator

-- https://www.w3.org/TR/css-animations-1/#propdef-animation-fill-mode

animationFillMode = Proxy :: Proxy "animation-fill-mode"

instance Property "animation-fill-mode"

none = Proxy :: Proxy "none"
forwards = Proxy :: Proxy "forwards"
backwards = Proxy :: Proxy "backwards"
both = Proxy :: Proxy "both"

class AnimationFillModeKeyword (s :: Symbol)

instance AnimationFillModeKeyword "none"
instance AnimationFillModeKeyword "forwards"
instance AnimationFillModeKeyword "backwards"
instance AnimationFillModeKeyword "both"

class IsSingleAnimationFillModeList (a :: Type)

instance
  ( AnimationFillModeKeyword sx
  , IsSingleAnimationFillModeList xs
  ) =>
  IsSingleAnimationFillModeList (Proxy sx /\ xs)

instance AnimationFillModeKeyword s => IsSingleAnimationFillModeList (Proxy s)

instance declarationAnimationFillMode ::
  ( IsSingleAnimationFillModeList a
  , MultiVal a
  ) =>
  Declaration "animation-fill-mode" a where
  pval = const $ intercalateMultiVal $ val "," <> Val _.separator

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/css-backgrounds-3/

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-background-color

backgroundColor = Proxy :: Proxy "background-color"

instance Property "background-color"
instance Animatable "background-color"

instance declarationBackgroundColor ::
  ( IsColor a
  , ToVal a
  ) =>
  Declaration "background-color" a where
  pval = const val

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-background-image

backgroundImage = Proxy :: Proxy "background-image"

instance Property "background-image"

class IsBgImageList (a :: Type)

instance IsBgImageList (Proxy "none")
else instance (IsBgImageList xs) => IsBgImageList (Proxy "none" /\ xs)
else instance (IsImage x, IsBgImageList xs) => IsBgImageList (x /\ xs)
else instance IsImage x => IsBgImageList x

instance declarationBackgroundImage ::
  ( IsBgImageList a
  , MultiVal a
  ) =>
  Declaration "background-image" a where
  pval = const $ intercalateMultiVal $ val "," <> Val _.separator

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-background-repeat

backgroundRepeat = Proxy :: Proxy "background-repeat"

instance Property "background-repeat"

repeatX = Proxy :: Proxy "repeat-x"
repeatY = Proxy :: Proxy "repeat-y"
repeat' = Proxy :: Proxy "repeat"
space = Proxy :: Proxy "space"
round = Proxy :: Proxy "round"
noRepeat = Proxy :: Proxy "no-repeat"

class RepeatStyle1dKeyword (s :: Symbol)

instance RepeatStyle1dKeyword "repeat"
instance RepeatStyle1dKeyword "space"
instance RepeatStyle1dKeyword "round"
instance RepeatStyle1dKeyword "no-repeat"

class RepeatStyle2dKeyword (s :: Symbol)

instance RepeatStyle2dKeyword "repeat-x"
instance RepeatStyle2dKeyword "repeat-y"
instance RepeatStyle2dKeyword "repeat"
instance RepeatStyle2dKeyword "space"
instance RepeatStyle2dKeyword "round"
instance RepeatStyle2dKeyword "no-repeat"

class IsRepeatStyle (a :: Type)

instance
  ( RepeatStyle1dKeyword sx
  , RepeatStyle1dKeyword sy
  ) =>
  IsRepeatStyle (Proxy sx ~ Proxy sy)

instance RepeatStyle2dKeyword s => IsRepeatStyle (Proxy s)

class IsRepeatStyleList (a :: Type)

instance (IsRepeatStyle x, IsRepeatStyleList xs) => IsRepeatStyleList (x /\ xs)
else instance IsRepeatStyle a => IsRepeatStyleList a

instance declarationBackgroundRepeat ::
  ( IsRepeatStyleList a
  , MultiVal a
  ) =>
  Declaration "background-repeat" a where
  pval = const $ intercalateMultiVal $ val "," <> Val _.separator

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-background-attachment

backgroundAttachment = Proxy :: Proxy "background-attachment"

instance Property "background-attachment"

local' = Proxy :: Proxy "local"

class AttachmentKeyword (s :: Symbol)

instance AttachmentKeyword "fixed"
instance AttachmentKeyword "local"
instance AttachmentKeyword "scroll"

class IsAttachmentList (a :: Type)

instance
  ( AttachmentKeyword sx
  , IsAttachmentList xs
  ) =>
  IsAttachmentList (Proxy sx /\ xs)

instance AttachmentKeyword s => IsAttachmentList (Proxy s)

instance declarationBackgroundAttachment ::
  ( IsAttachmentList a
  , MultiVal a
  ) =>
  Declaration "background-attachment" a where
  pval = const $ intercalateMultiVal $ val "," <> Val _.separator

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-background-position

backgroundPosition = Proxy :: Proxy "background-position"

instance Property "background-position"
instance Animatable "background-position"

class IsPositionList (a :: Type)

instance (IsPosition x, IsPositionList xs) => IsPositionList (x /\ xs)
else instance IsPosition a => IsPositionList a

instance declarationBackgroundPosition ::
  ( IsPositionList a
  , MultiVal a
  ) =>
  Declaration "background-position" a where
  pval = const $ intercalateMultiVal $ val "," <> Val _.separator

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-background-clip

backgroundClip = Proxy :: Proxy "background-clip"

instance Property "background-clip"

borderBox = Proxy :: Proxy "border-box"
paddingBox = Proxy :: Proxy "padding-box"
contentBox = Proxy :: Proxy "content-box"

class BoxKeyword (s :: Symbol)

instance BoxKeyword "border-box"
instance BoxKeyword "padding-box"
instance BoxKeyword "content-box"

class IsBoxList (a :: Type)

instance (BoxKeyword sx, IsBoxList xs) => IsBoxList (Proxy sx /\ xs)
else instance BoxKeyword s => IsBoxList (Proxy s)

instance declarationBackgroundClip ::
  ( IsBoxList a
  , MultiVal a
  ) =>
  Declaration "background-clip" a where
  pval = const $ intercalateMultiVal $ val "," <> Val _.separator

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-background-origin

backgroundOrigin = Proxy :: Proxy "background-origin"

instance Property "background-origin"

instance declarationBackgroundOrigin ::
  ( IsBoxList a
  , MultiVal a
  ) =>
  Declaration "background-origin" a where
  pval = const $ intercalateMultiVal $ val "," <> Val _.separator

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-background-size

backgroundSize = Proxy :: Proxy "background-size"

instance Property "background-size"
instance Animatable "background-size"

cover = Proxy :: Proxy "cover"
contain = Proxy :: Proxy "contain"

class IsBgSize (a :: Type)

instance
  ( LengthPercentageTag tx
  , LengthPercentageTag ty
  ) =>
  IsBgSize (Measure tx ~ Measure ty)

instance LengthPercentageTag ty => IsBgSize (Proxy "auto" ~ Measure ty)
instance LengthPercentageTag tx => IsBgSize (Measure tx ~ Proxy "auto")
instance IsBgSize (Proxy "auto" ~ Proxy "auto")
instance LengthPercentageTag t => IsBgSize (Measure t)
instance IsBgSize (Proxy "auto")
instance IsBgSize (Proxy "cover")
instance IsBgSize (Proxy "contain")

class IsBgSizeList (a :: Type)

instance (IsBgSize x, IsBgSizeList xs) => IsBgSizeList (x /\ xs)
else instance IsBgSize a => IsBgSizeList a

instance declarationBackgroundSize ::
  ( IsBgSizeList a
  , MultiVal a
  ) =>
  Declaration "background-size" a where
  pval = const $ intercalateMultiVal $ val "," <> Val _.separator

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-top-color

borderTopColor = Proxy :: Proxy "border-top-color"

instance Property "border-top-color"
instance Animatable "border-top-color"

instance declarationBorderTopColor ::
  ( IsColor a
  , ToVal a
  ) =>
  Declaration "border-top-color" a where
  pval = const val

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-right-color

borderRightColor = Proxy :: Proxy "border-right-color"

instance Property "border-right-color"
instance Animatable "border-right-color"

instance declarationBorderRightColor ::
  Declaration "border-top-color" a =>
  Declaration "border-right-color" a where
  pval = const $ pval borderTopColor

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-bottom-color

borderBottomColor = Proxy :: Proxy "border-bottom-color"

instance Property "border-bottom-color"
instance Animatable "border-bottom-color"

instance declarationBorderBottomColor ::
  Declaration "border-top-color" a =>
  Declaration "border-bottom-color" a where
  pval = const $ pval borderTopColor

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-left-color

borderLeftColor = Proxy :: Proxy "border-left-color"

instance Property "border-left-color"
instance Animatable "border-left-color"

instance declarationBorderLeftColor ::
  Declaration "border-top-color" a =>
  Declaration "border-left-color" a where
  pval = const $ pval borderTopColor

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-color

borderColor = Proxy :: Proxy "border-color"

instance Property "border-color"
instance Animatable "border-color"

class IsBorderColor (a :: Type)

instance
  ( IsColor t
  , IsColor r
  , IsColor b
  , IsColor l
  , ToVal (t ~ r ~ b ~ l)
  ) =>
  IsBorderColor (t ~ r ~ b ~ l)
else instance
  ( IsColor t
  , IsColor x
  , IsColor b
  , ToVal (t ~ x ~ b)
  ) =>
  IsBorderColor (t ~ x ~ b)
else instance (IsColor y, IsColor x, ToVal (y ~ x)) => IsBorderColor (y ~ x)
else instance (IsColor a, ToVal a) => IsBorderColor a

instance declarationBorderColor ::
  ( IsBorderColor a
  , ToVal a
  ) =>
  Declaration "border-color" a where
  pval = const val

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-top-style

borderTopStyle = Proxy :: Proxy "border-top-style"

instance Property "border-top-style"

hidden = Proxy :: Proxy "hidden"
dotted = Proxy :: Proxy "dotted"
dashed = Proxy :: Proxy "dashed"
solid = Proxy :: Proxy "solid"
double = Proxy :: Proxy "double"
groove = Proxy :: Proxy "groove"
ridge = Proxy :: Proxy "ridge"
inset = Proxy :: Proxy "inset"
outset = Proxy :: Proxy "outset"

class LineStyleKeyword (s :: Symbol)

instance LineStyleKeyword "none"
instance LineStyleKeyword "hidden"
instance LineStyleKeyword "dotted"
instance LineStyleKeyword "dashed"
instance LineStyleKeyword "solid"
instance LineStyleKeyword "double"
instance LineStyleKeyword "groove"
instance LineStyleKeyword "ridge"
instance LineStyleKeyword "inset"
instance LineStyleKeyword "outset"

instance declarationBorderTopStyle ::
  ( LineStyleKeyword s
  , ToVal (Proxy s)
  ) =>
  Declaration "border-top-style" (Proxy s) where
  pval = const val

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-right-style

borderRightStyle = Proxy :: Proxy "border-right-style"

instance Property "border-right-style"

instance declarationBorderRightStyle ::
  Declaration "border-top-style" a =>
  Declaration "border-right-style" a where
  pval = const $ pval borderTopStyle

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-bottom-style

borderBottomStyle = Proxy :: Proxy "border-bottom-style"

instance Property "border-bottom-style"

instance declarationBorderBottomStyle ::
  Declaration "border-top-style" a =>
  Declaration "border-bottom-style" a where
  pval = const $ pval borderTopStyle

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-left-style

borderLeftStyle = Proxy :: Proxy "border-left-style"

instance Property "border-left-style"

instance declarationBorderLeftStyle ::
  Declaration "border-top-style" a =>
  Declaration "border-left-style" a where
  pval = const $ pval borderTopStyle

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-style

borderStyle = Proxy :: Proxy "border-style"

instance Property "border-style"

class IsBorderStyle (a :: Type)

instance
  ( LineStyleKeyword st
  , LineStyleKeyword sr
  , LineStyleKeyword sb
  , LineStyleKeyword sl
  , ToVal (Proxy st ~ Proxy sr ~ Proxy sb ~ Proxy sl)
  ) =>
  IsBorderStyle (Proxy st ~ Proxy sr ~ Proxy sb ~ Proxy sl)

instance
  ( LineStyleKeyword st
  , LineStyleKeyword sx
  , LineStyleKeyword sb
  , ToVal (Proxy st ~ Proxy sx ~ Proxy sb)
  ) =>
  IsBorderStyle (Proxy st ~ Proxy sx ~ Proxy sb)

instance
  ( LineStyleKeyword sy
  , LineStyleKeyword sx
  , ToVal (Proxy sy ~ Proxy sx)
  ) =>
  IsBorderStyle (Proxy sy ~ Proxy sx)

instance (LineStyleKeyword s, ToVal (Proxy s)) => IsBorderStyle (Proxy s)

instance declarationBorderStyle ::
  ( IsBorderStyle a
  , ToVal a
  ) =>
  Declaration "border-style" a where
  pval = const val

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-top-width

borderTopWidth = Proxy :: Proxy "border-top-width"

instance Property "border-top-width"
instance Animatable "border-top-width"

thin = Proxy :: Proxy "thin"
medium = Proxy :: Proxy "medium"
thick = Proxy :: Proxy "thick"

class LineWidthKeyword (s :: Symbol)

instance LineWidthKeyword "thin"
instance LineWidthKeyword "medium"
instance LineWidthKeyword "thick"

class IsLineWidth (a :: Type)

instance LineWidthKeyword s => IsLineWidth (Proxy s)
instance LengthTag t => IsLineWidth (Measure t)

instance declarationBorderTopWidth ::
  ( IsLineWidth a
  , ToVal a
  ) =>
  Declaration "border-top-width" a where
  pval = const val

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-right-width

borderRightWidth = Proxy :: Proxy "border-right-width"

instance Property "border-right-width"
instance Animatable "border-right-width"

instance declarationBorderRightWidth ::
  Declaration "border-top-width" a =>
  Declaration "border-right-width" a where
  pval = const $ pval borderTopWidth

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-bottom-width

borderBottomWidth = Proxy :: Proxy "border-bottom-width"

instance Property "border-bottom-width"
instance Animatable "border-bottom-width"

instance declarationBorderBottomWidth ::
  Declaration "border-top-width" a =>
  Declaration "border-bottom-width" a where
  pval = const $ pval borderTopWidth

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-left-width

borderLeftWidth = Proxy :: Proxy "border-left-width"

instance Property "border-left-width"
instance Animatable "border-left-width"

instance declarationBorderLeftWidth ::
  Declaration "border-top-width" a =>
  Declaration "border-left-width" a where
  pval = const $ pval borderTopWidth

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-width

borderWidth = Proxy :: Proxy "border-width"

instance Property "border-width"
instance Animatable "border-width"

class IsBorderWidth (a :: Type)

instance
  ( IsLineWidth t
  , IsLineWidth r
  , IsLineWidth b
  , IsLineWidth l
  , ToVal (t ~ r ~ b ~ l)
  ) =>
  IsBorderWidth (t ~ r ~ b ~ l)
else instance
  ( IsLineWidth t
  , IsLineWidth x
  , IsLineWidth b
  , ToVal (t ~ x ~ b)
  ) =>
  IsBorderWidth (t ~ x ~ b)
else instance
  ( IsLineWidth y
  , IsLineWidth x
  , ToVal (y ~ x)
  ) =>
  IsBorderWidth (y ~ x)
else instance (IsLineWidth a, ToVal a) => IsBorderWidth a

instance declarationBorderWidth ::
  ( IsBorderWidth a
  , ToVal a
  ) =>
  Declaration "border-width" a where
  pval = const val

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-top-left-radius

borderTopLeftRadius = Proxy :: Proxy "border-top-left-radius"

instance Property "border-top-left-radius"
instance Animatable "border-top-left-radius"

class IsSingleBorderRadius (a :: Type)

instance
  ( LengthPercentageTag tx
  , LengthPercentageTag ty
  ) =>
  IsSingleBorderRadius (Measure tx ~ Measure ty)

instance LengthPercentageTag t => IsSingleBorderRadius (Measure t)

instance declarationBorderTopRadius ::
  ( IsSingleBorderRadius a
  , ToVal a
  ) =>
  Declaration "border-top-left-radius" a where
  pval = const val

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-top-right-radius

borderTopRightRadius = Proxy :: Proxy "border-top-right-radius"

instance Property "border-top-right-radius"
instance Animatable "border-top-right-radius"

instance declarationBorderTopRightRadius ::
  Declaration "border-top-left-radius" a =>
  Declaration "border-top-right-radius" a where
  pval = const $ pval borderTopLeftRadius

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-bottom-right-radius

borderBottomRightRadius = Proxy :: Proxy "border-bottom-right-radius"

instance Property "border-bottom-right-radius"
instance Animatable "border-bottom-right-radius"

instance declarationBorderBottomRightRadius ::
  Declaration "border-top-left-radius" a =>
  Declaration "border-bottom-right-radius" a where
  pval = const $ pval borderTopLeftRadius

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-bottom-left-radius

borderBottomLeftRadius = Proxy :: Proxy "border-bottom-left-radius"

instance Property "border-bottom-left-radius"
instance Animatable "border-bottom-left-radius"

instance declarationBorderBottomLeftRadius ::
  Declaration "border-top-left-radius" a =>
  Declaration "border-bottom-left-radius" a where
  pval = const $ pval borderTopLeftRadius

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-radius

borderRadius = Proxy :: Proxy "border-radius"

instance Property "border-radius"
instance Animatable "border-radius"

class IsBorderRadius (a :: Type)

instance
  ( LengthPercentageTag ttlx
  , LengthPercentageTag ttrx
  , LengthPercentageTag tbrx
  , LengthPercentageTag tblx
  , LengthPercentageTag ttly
  , LengthPercentageTag ttry
  , LengthPercentageTag tbry
  , LengthPercentageTag tbly
  ) =>
  IsBorderRadius ( Measure ttlx ~ Measure ttrx ~ Measure tbrx ~ Measure tblx /\
        Measure ttly ~ Measure ttry ~ Measure tbry ~ Measure tbly
    )

instance
  ( LengthPercentageTag ttlx
  , LengthPercentageTag ttrx
  , LengthPercentageTag tbrx
  , LengthPercentageTag tblx
  , LengthPercentageTag ttly
  , LengthPercentageTag ttrbly
  , LengthPercentageTag tbry
  ) =>
  IsBorderRadius ( Measure ttlx ~ Measure ttrx ~ Measure tbrx ~ Measure tblx /\
        Measure ttly ~ Measure ttrbly ~ Measure tbry
    )

instance
  ( LengthPercentageTag ttlx
  , LengthPercentageTag ttrx
  , LengthPercentageTag tbrx
  , LengthPercentageTag tblx
  , LengthPercentageTag ttlbry
  , LengthPercentageTag ttrbly
  ) =>
  IsBorderRadius ( Measure ttlx ~ Measure ttrx ~ Measure tbrx ~ Measure tblx /\
        Measure ttlbry ~ Measure ttrbly
    )

instance
  ( LengthPercentageTag ttlx
  , LengthPercentageTag ttrx
  , LengthPercentageTag tbrx
  , LengthPercentageTag tblx
  , LengthPercentageTag ty
  ) =>
  IsBorderRadius ( Measure ttlx ~ Measure ttrx ~ Measure tbrx ~ Measure tblx /\
        Measure ty
    )

instance
  ( LengthPercentageTag ttlx
  , LengthPercentageTag ttrx
  , LengthPercentageTag tbrx
  , LengthPercentageTag tblx
  ) =>
  IsBorderRadius (Measure ttlx ~ Measure ttrx ~ Measure tbrx ~ Measure tblx)

instance
  ( LengthPercentageTag ttlx
  , LengthPercentageTag ttrblx
  , LengthPercentageTag tbrx
  , LengthPercentageTag ttly
  , LengthPercentageTag ttry
  , LengthPercentageTag tbry
  , LengthPercentageTag tbly
  ) =>
  IsBorderRadius ( Measure ttlx ~ Measure ttrblx ~ Measure tbrx /\ Measure ttly
        ~ Measure ttry
        ~ Measure tbry
        ~ Measure tbly
    )

instance
  ( LengthPercentageTag ttlx
  , LengthPercentageTag ttrblx
  , LengthPercentageTag tbrx
  , LengthPercentageTag ttly
  , LengthPercentageTag ttrbly
  , LengthPercentageTag tbry
  ) =>
  IsBorderRadius ( Measure ttlx ~ Measure ttrblx ~ Measure tbrx /\ Measure ttly
        ~ Measure ttrbly
        ~ Measure tbry
    )

instance
  ( LengthPercentageTag ttlx
  , LengthPercentageTag ttrblx
  , LengthPercentageTag tbrx
  , LengthPercentageTag ttlbry
  , LengthPercentageTag ttrbly
  ) =>
  IsBorderRadius ( Measure ttlx ~ Measure ttrblx ~ Measure tbrx /\
        Measure ttlbry ~ Measure ttrbly
    )

instance
  ( LengthPercentageTag ttlx
  , LengthPercentageTag ttrblx
  , LengthPercentageTag tbrx
  , LengthPercentageTag ty
  ) =>
  IsBorderRadius (Measure ttlx ~ Measure ttrblx ~ Measure tbrx /\ Measure ty)

instance
  ( LengthPercentageTag ttlx
  , LengthPercentageTag ttrblx
  , LengthPercentageTag tbrx
  ) =>
  IsBorderRadius (Measure ttlx ~ Measure ttrblx ~ Measure tbrx)

instance
  ( LengthPercentageTag ttlbrx
  , LengthPercentageTag ttrblx
  , LengthPercentageTag ttly
  , LengthPercentageTag ttry
  , LengthPercentageTag tbry
  , LengthPercentageTag tbly
  ) =>
  IsBorderRadius ( Measure ttlbrx ~ Measure ttrblx /\ Measure ttly
        ~ Measure ttry
        ~ Measure tbry
        ~ Measure tbly
    )

instance
  ( LengthPercentageTag ttlbrx
  , LengthPercentageTag ttrblx
  , LengthPercentageTag ttly
  , LengthPercentageTag ttrbly
  , LengthPercentageTag tbry
  ) =>
  IsBorderRadius ( Measure ttlbrx ~ Measure ttrblx /\ Measure ttly
        ~ Measure ttrbly
        ~ Measure tbry
    )

instance
  ( LengthPercentageTag ttlbrx
  , LengthPercentageTag ttrblx
  , LengthPercentageTag ttlbry
  , LengthPercentageTag ttrbly
  ) =>
  IsBorderRadius ( Measure ttlbrx ~ Measure ttrblx /\ Measure ttlbry ~ Measure
        ttrbly
    )

instance
  ( LengthPercentageTag ttlbrx
  , LengthPercentageTag ttrblx
  , LengthPercentageTag ty
  ) =>
  IsBorderRadius (Measure ttlbrx ~ Measure ttrblx /\ Measure ty)

instance
  ( LengthPercentageTag ttlbrx
  , LengthPercentageTag ttrblx
  ) =>
  IsBorderRadius (Measure ttlbrx ~ Measure ttrblx)

instance
  ( LengthPercentageTag tx
  , LengthPercentageTag ttly
  , LengthPercentageTag ttry
  , LengthPercentageTag tbry
  , LengthPercentageTag tbly
  ) =>
  IsBorderRadius ( Measure tx /\ Measure ttly ~ Measure ttry ~ Measure tbry ~
        Measure tbly
    )

instance
  ( LengthPercentageTag tx
  , LengthPercentageTag ttly
  , LengthPercentageTag ttrbly
  , LengthPercentageTag tbry
  ) =>
  IsBorderRadius (Measure tx /\ Measure ttly ~ Measure ttrbly ~ Measure tbry)

instance
  ( LengthPercentageTag tx
  , LengthPercentageTag ttlbry
  , LengthPercentageTag ttrbly
  ) =>
  IsBorderRadius (Measure tx /\ Measure ttlbry ~ Measure ttrbly)

instance
  ( LengthPercentageTag tx
  , LengthPercentageTag ty
  ) =>
  IsBorderRadius (Measure tx /\ Measure ty)

instance LengthPercentageTag t => IsBorderRadius (Measure t)

instance declarationBorderRadius ::
  ( IsBorderRadius a
  , MultiVal a
  ) =>
  Declaration "border-radius" a where
  pval =
    const $ intercalateMultiVal $ Val _.separator <> val "/" <> Val _.separator

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-box-shadow

boxShadow = Proxy :: Proxy "box-shadow"

instance Property "box-shadow"
instance Animatable "box-shadow"

class IsShadow (a :: Type)

instance
  ( IsColor color
  , LengthTag txo
  , LengthTag tyo
  , LengthTag tblur
  , LengthTag tspread
  ) =>
  IsShadow ( color ~ Measure txo ~ Measure tyo ~ Measure tblur ~ Measure tspread
        ~ Proxy "inset"
    )

instance
  ( IsColor color
  , LengthTag txo
  , LengthTag tyo
  , LengthTag tblur
  , LengthTag tspread
  ) =>
  IsShadow (color ~ Measure txo ~ Measure tyo ~ Measure tblur ~ Measure tspread)

instance
  ( LengthTag txo
  , LengthTag tyo
  , LengthTag tblur
  , LengthTag tspread
  ) =>
  IsShadow ( Measure txo ~ Measure tyo ~ Measure tblur ~ Measure tspread ~ Proxy
        "inset"
    )

else instance
  ( IsColor color
  , LengthTag txo
  , LengthTag tyo
  , LengthTag tblur
  ) =>
  IsShadow (color ~ Measure txo ~ Measure tyo ~ Measure tblur ~ Proxy "inset")

instance
  ( LengthTag txo
  , LengthTag tyo
  , LengthTag tblur
  , LengthTag tspread
  ) =>
  IsShadow (Measure txo ~ Measure tyo ~ Measure tblur ~ Measure tspread)

else instance
  ( IsColor color
  , LengthTag txo
  , LengthTag tyo
  , LengthTag tblur
  ) =>
  IsShadow (color ~ Measure txo ~ Measure tyo ~ Measure tblur)

instance
  ( LengthTag txo
  , LengthTag tyo
  , LengthTag tblur
  ) =>
  IsShadow (Measure txo ~ Measure tyo ~ Measure tblur ~ Proxy "inset")

else instance
  ( IsColor color
  , LengthTag txo
  , LengthTag tyo
  ) =>
  IsShadow (color ~ Measure txo ~ Measure tyo ~ Proxy "inset")

instance
  ( LengthTag txo
  , LengthTag tyo
  , LengthTag tblur
  ) =>
  IsShadow (Measure txo ~ Measure tyo ~ Measure tblur)

else instance
  ( LengthTag txo
  , LengthTag tyo
  ) =>
  IsShadow (Measure txo ~ Measure tyo ~ Proxy "inset")

else instance
  ( IsColor color
  , LengthTag txo
  , LengthTag tyo
  ) =>
  IsShadow (color ~ Measure txo ~ Measure tyo)

instance (LengthTag txo, LengthTag tyo) => IsShadow (Measure txo ~ Measure tyo)

instance (IsShadow x, IsShadow xs) => IsShadow (x /\ xs)

instance declarationBoxShadowNone :: Declaration "box-shadow" (Proxy "none") where
  pval = const val

else instance declarationBoxShadowIsShadow ::
  ( IsShadow a
  , MultiVal a
  ) =>
  Declaration "box-shadow" a where
  pval = const $ intercalateMultiVal $ val "," <> Val _.separator

--------------------------------------------------------------------------------

-- Box Model
-- https://www.w3.org/TR/css-box-3/

-- https://www.w3.org/tr/css-box-3/#propdef-margin-top

marginTop = Proxy :: Proxy "margin-top"

instance Property "margin-top"
instance Animatable "margin-top"

class IsSingleMargin (a :: Type)

instance LengthPercentageTag t => IsSingleMargin (Measure t)
instance IsSingleMargin (Proxy "auto")

instance declarationMarginTop ::
  ( IsSingleMargin a
  , ToVal a
  ) =>
  Declaration "margin-top" a where
  pval = const val

-- https://www.w3.org/tr/css-box-3/#propdef-margin-right

marginRight = Proxy :: Proxy "margin-right"

instance Property "margin-right"
instance Animatable "margin-right"

instance declarationMarginRight ::
  Declaration "margin-top" a =>
  Declaration "margin-right" a where
  pval = const $ pval marginTop

-- https://www.w3.org/tr/css-box-3/#propdef-margin-bottom

marginBottom = Proxy :: Proxy "margin-bottom"

instance Property "margin-bottom"
instance Animatable "margin-bottom"

instance declarationMarginBottom ::
  Declaration "margin-top" a =>
  Declaration "margin-bottom" a where
  pval = const $ pval marginTop

-- https://www.w3.org/tr/css-box-3/#propdef-margin-left

marginLeft = Proxy :: Proxy "margin-left"

instance Property "margin-left"
instance Animatable "margin-left"

instance declarationMarginLeft ::
  Declaration "margin-top" a =>
  Declaration "margin-left" a where
  pval = const $ pval marginTop

-- https://www.w3.org/tr/css-box-3/#propdef-margin

margin = Proxy :: Proxy "margin"

instance Property "margin"
instance Animatable "margin"

class IsMargin (a :: Type)

instance
  ( IsSingleMargin t
  , IsSingleMargin r
  , IsSingleMargin b
  , IsSingleMargin l
  ) =>
  IsMargin (t ~ r ~ b ~ l)
else instance
  ( IsSingleMargin t
  , IsSingleMargin x
  , IsSingleMargin b
  ) =>
  IsMargin (t ~ x ~ b)
else instance
  ( IsSingleMargin y
  , IsSingleMargin x
  ) =>
  IsMargin (y ~ x)
else instance IsSingleMargin a => IsMargin a

instance declarationMargin ::
  ( IsMargin a
  , ToVal a
  ) =>
  Declaration "margin" a where
  pval = const val

-- https://www.w3.org/TR/css-box-3/#propdef-padding-top

paddingTop = Proxy :: Proxy "padding-top"

instance Property "padding-top"
instance Animatable "padding-top"

instance declarationPaddingTop ::
  LengthPercentageTag t =>
  Declaration "padding-top" (Measure t) where
  pval = const val

-- https://www.w3.org/TR/css-box-3/#propdef-padding-right

paddingRight = Proxy :: Proxy "padding-right"

instance Property "padding-right"
instance Animatable "padding-right"

instance declarationPaddingRight ::
  Declaration "padding-top" a =>
  Declaration "padding-right" a where
  pval = const $ pval paddingTop

-- https://www.w3.org/TR/css-box-3/#propdef-padding-bottom

paddingBottom = Proxy :: Proxy "padding-bottom"

instance Property "padding-bottom"
instance Animatable "padding-bottom"

instance declarationPaddingBottom ::
  Declaration "padding-top" a =>
  Declaration "padding-bottom" a where
  pval = const $ pval paddingTop

-- https://www.w3.org/TR/css-box-3/#propdef-padding-left

paddingLeft = Proxy :: Proxy "padding-left"

instance Property "padding-left"
instance Animatable "padding-left"

instance declarationPaddingLeft ::
  Declaration "padding-top" a =>
  Declaration "padding-left" a where
  pval = const $ pval paddingTop

-- https://www.w3.org/TR/css-box-3/#propdef-padding

padding = Proxy :: Proxy "padding"

instance Property "padding"
instance Animatable "padding"

class IsPadding (a :: Type)

instance
  ( LengthPercentageTag tt
  , LengthPercentageTag tr
  , LengthPercentageTag tb
  , LengthPercentageTag tl
  ) =>
  IsPadding (Measure tt ~ Measure tr ~ Measure tb ~ Measure tl)

instance
  ( LengthPercentageTag tt
  , LengthPercentageTag tx
  , LengthPercentageTag tb
  ) =>
  IsPadding (Measure tt ~ Measure tx ~ Measure tb)

instance
  ( LengthPercentageTag ty
  , LengthPercentageTag tx
  ) =>
  IsPadding (Measure ty ~ Measure tx)

instance LengthPercentageTag t => IsPadding (Measure t)

instance declarationPadding ::
  ( IsPadding a
  , ToVal a
  ) =>
  Declaration "padding" a where
  pval = const val

--------------------------------------------------------------------------------

-- Color
-- https://www.w3.org/TR/css-color-4/

-- https://www.w3.org/TR/css-color-4/#propdef-color

color = Proxy :: Proxy "color"

instance Property "color"
instance Animatable "color"

instance declarationColorIsColor ::
  ( IsColor a
  , ToVal a
  ) =>
  Declaration "color" a where
  pval = const val

-- https://www.w3.org/TR/css-color-4/#propdef-opacity

opacity = Proxy :: Proxy "opacity"

instance Property "opacity"
instance Animatable "opacity"

instance declarationOpacityNumber ::
  ToNumber a =>
  Declaration "opacity" a where
  pval = const $ val <<< number

-- https://www.w3.org/TR/css-color-4/#typedef-color

newtype CSSColor = CSSColor String

derive newtype instance ToVal CSSColor

currentColor :: CSSColor
currentColor = CSSColor "currentColor"

transparent :: CSSColor
transparent = CSSColor "transparent"

instance ToVal Color where
  val c = Val \cfg -> cfg.color c

class IsColor (a :: Type)

instance IsColor Color
instance IsColor CSSColor

--------------------------------------------------------------------------------

-- Generated Content
-- https://www.w3.org/TR/css-content-3/

-- https://www.w3.org/TR/css-content-3/#propdef-content

content = Proxy :: Proxy "content"

instance Property "content"

contents = Proxy :: Proxy "contents"

class ContentKeyword (s :: Symbol)

instance ContentKeyword "normal"
instance ContentKeyword "none"
instance ContentKeyword "contents"

instance declarationContentString :: Declaration "content" String where
  pval = const $ val <<< quote

else instance declarationContentImageWithAltText ::
  ( IsImage a
  , ToVal a
  ) =>
  Declaration "content" (a /\ String) where
  pval _ (img' /\ alt') =
    val img'
      <> Val (\c -> c.separator <> "/" <> c.separator)
      <> val (quote alt')

else instance declarationContentKeyword ::
  ( ContentKeyword s
  , ToVal (Proxy s)
  ) =>
  Declaration "content" (Proxy s) where
  pval = const val

else instance declarationContentImage ::
  ( IsImage a
  , ToVal a
  ) =>
  Declaration "content" a where
  pval = const val

--------------------------------------------------------------------------------

-- Display
-- https://www.w3.org/TR/css-display-3/

-- https://www.w3.org/TR/css-display-3/#propdef-display

display = Proxy :: Proxy "display"

instance Property "display"

block = Proxy :: Proxy "block"
inline = Proxy :: Proxy "inline"
flowRoot = Proxy :: Proxy "flow-root"
flex = Proxy :: Proxy "flex"
grid = Proxy :: Proxy "grid"
listItem = Proxy :: Proxy "list-item"
tableRowGroup = Proxy :: Proxy "table-row-group"
tableHeaderGroup = Proxy :: Proxy "table-header-group"
tableFooterGroup = Proxy :: Proxy "table-footer-group"
tableRow = Proxy :: Proxy "table-row"
tableCell = Proxy :: Proxy "table-cell"
tableColumnGroup = Proxy :: Proxy "table-column-group"
tableColumn = Proxy :: Proxy "table-column"
tableCaption = Proxy :: Proxy "table-caption"
inlineBlock = Proxy :: Proxy "inline-block"
inlineTable = Proxy :: Proxy "inline-table"
inlineFlex = Proxy :: Proxy "inline-flex"
inlineGrid = Proxy :: Proxy "inline-grid"

class DisplayKeyword (s :: Symbol)

instance DisplayKeyword "block"
instance DisplayKeyword "inline"
instance DisplayKeyword "flow-root"
instance DisplayKeyword "table"
instance DisplayKeyword "flex"
instance DisplayKeyword "grid"
instance DisplayKeyword "list-item"
instance DisplayKeyword "table-row-group"
instance DisplayKeyword "table-header-group"
instance DisplayKeyword "table-footer-group"
instance DisplayKeyword "table-row"
instance DisplayKeyword "table-cell"
instance DisplayKeyword "table-column-group"
instance DisplayKeyword "table-column"
instance DisplayKeyword "table-caption"
instance DisplayKeyword "contents"
instance DisplayKeyword "none"
instance DisplayKeyword "inline-block"
instance DisplayKeyword "inline-table"
instance DisplayKeyword "inline-flex"
instance DisplayKeyword "inline-grid"

instance declarationDisplay ::
  ( DisplayKeyword s
  , ToVal (Proxy s)
  ) =>
  Declaration "display" (Proxy s) where
  pval = const val

--------------------------------------------------------------------------------

-- Easing Functions
-- https://www.w3.org/TR/css-easing-1/

newtype EasingFunction = EasingFunction Val

derive newtype instance ToVal EasingFunction

-- https://www.w3.org/TR/css-easing-1/#valdef-easing-function-linear

linear :: EasingFunction
linear = EasingFunction $ val "linear"

-- https://www.w3.org/TR/css-easing-1/#valdef-cubic-bezier-easing-function-ease

ease :: EasingFunction
ease = EasingFunction $ val "ease"

-- https://www.w3.org/TR/css-easing-1/#valdef-cubic-bezier-easing-function-ease-in

easeIn :: EasingFunction
easeIn = EasingFunction $ val "ease-in"

-- https://www.w3.org/TR/css-easing-1/#valdef-cubic-bezier-easing-function-ease-out

easeOut :: EasingFunction
easeOut = EasingFunction $ val "ease-out"

-- https://www.w3.org/TR/css-easing-1/#valdef-cubic-bezier-easing-function-ease-out

easeInOut :: EasingFunction
easeInOut = EasingFunction $ val "ease-in-out"

-- https://www.w3.org/TR/css-easing-1/#funcdef-cubic-bezier-easing-function-cubic-bezier

cubicBezier
  :: forall x1 y1 x2 y2
   . ToNumber x1
  => ToNumber y1
  => ToNumber x2
  => ToNumber y2
  => x1
  -> y1
  -> x2
  -> y2
  -> EasingFunction
cubicBezier x1 y1 x2 y2 =
  EasingFunction
    $ fn "cubic-bezier"
    $ number x1 /\ number y1 /\ number x2 /\ number y2

-- https://www.w3.org/TR/css-easing-1/#typedef-step-position

jumpStart = Proxy :: Proxy "jump-start"
jumpEnd = Proxy :: Proxy "jump-end"
jumpNone = Proxy :: Proxy "jump-none"
jumpBoth = Proxy :: Proxy "jump-both"

class StepPosition (s :: Symbol)

instance StepPosition "jump-start"
instance StepPosition "jump-end"
instance StepPosition "jump-none"
instance StepPosition "jump-both"
instance StepPosition "start"
instance StepPosition "end"

steps
  :: forall s
   . StepPosition s
  => IsSymbol s
  => Int
  -> Proxy s
  -> EasingFunction
steps n f = EasingFunction $ fn "steps" $ val n /\ val f

stepStart :: EasingFunction
stepStart = EasingFunction $ val "step-start"

stepEnd :: EasingFunction
stepEnd = EasingFunction $ val "step-end"

--------------------------------------------------------------------------------

-- Flexible Box Layout
-- https://www.w3.org/TR/css-flexbox-1/

-- https://www.w3.org/TR/css-flexbox-1/#propdef-flex-direction

flexDirection = Proxy :: Proxy "flex-direction"

instance Property "flex-direction"

row = Proxy :: Proxy "row"
rowReverse = Proxy :: Proxy "row-reverse"
column = Proxy :: Proxy "column"
columnReverse = Proxy :: Proxy "column-reverse"

class FlexDirectionKeyword (s :: Symbol)

instance FlexDirectionKeyword "row"
instance FlexDirectionKeyword "row-reverse"
instance FlexDirectionKeyword "column"
instance FlexDirectionKeyword "column-reverse"

instance declarationFlexDirection ::
  ( FlexDirectionKeyword s
  , ToVal (Proxy s)
  ) =>
  Declaration "flex-direction" (Proxy s) where
  pval = const val

-- https://www.w3.org/TR/css-flexbox-1/#propdef-flex-wrap

flexWrap = Proxy :: Proxy "flex-wrap"

instance Property "flex-wrap"

nowrap = Proxy :: Proxy "nowrap"
wrapReverse = Proxy :: Proxy "wrap-reverse"

class FlexWrapKeyword (s :: Symbol)

instance FlexWrapKeyword "nowrap"
instance FlexWrapKeyword "wrap"
instance FlexWrapKeyword "wrap-reverse"

instance declarationFlexWrap ::
  ( FlexWrapKeyword s
  , ToVal (Proxy s)
  ) =>
  Declaration "flex-wrap" (Proxy s) where
  pval = const val

-- https://www.w3.org/TR/css-flexbox-1/#propdef-order

order = Proxy :: Proxy "order"

instance Property "order"
instance Animatable "order"

instance declarationOrder :: Declaration "order" Int where
  pval = const val

-- https://www.w3.org/TR/css-flexbox-1/#propdef-flex

-- Note that flex shorthand is included because the spec recommends it over its
-- constituent properties for "common uses".

instance Property "flex"
instance Animatable "flex"

instance declarationFlexGrowShrinkBasis ::
  ( Declaration "flex-grow" grow
  , Declaration "flex-shrink" shrink
  , Declaration "flex-basis" basis
  ) =>
  Declaration "flex" (grow ~ shrink ~ basis) where
  pval _ (grow ~ shrink ~ basis) =
    val $ pval flexGrow grow ~ pval flexShrink shrink ~ pval flexBasis basis

else instance declarationFlexGrowNumberShrinkNumber ::
  Declaration "flex" (Number ~ Number) where
  pval = const val

else instance declarationFlexGrowNumberShrinkInt ::
  Declaration "flex" (Number ~ Int) where
  pval = const val

else instance declarationFlexGrowIntShrinkNumber ::
  Declaration "flex" (Int ~ Number) where
  pval = const val

else instance declarationFlexGrowIntShrinkInt :: Declaration "flex" (Int ~ Int) where
  pval = const val

else instance declarationFlexGrowNumberBasis ::
  ( Declaration "flex-grow" grow
  , Declaration "flex-basis" basis
  ) =>
  Declaration "flex" (grow ~ basis) where
  pval _ (grow ~ basis) = pval flexGrow grow <> val " " <> pval flexBasis basis

else instance declarationFlexNone :: Declaration "flex" (Proxy "none") where
  pval = const val

else instance declarationFlexNumber :: Declaration "flex" Number where
  pval = const val

else instance declarationFlexInt :: Declaration "flex" Int where
  pval = const val

else instance declarationFlexWidth ::
  Declaration "width" a =>
  Declaration "flex" a where
  pval = const $ pval width

-- https://www.w3.org/TR/css-flexbox-1/#propdef-flex-grow

flexGrow = Proxy :: Proxy "flex-grow"

instance Property "flex-grow"
instance Animatable "flex-grow"

instance declarationFlexGrow :: ToNumber a => Declaration "flex-grow" a where
  pval = const $ val <<< number

-- https://www.w3.org/TR/css-flexbox-1/#propdef-flex-shrink

flexShrink = Proxy :: Proxy "flex-shrink"

instance Property "flex-shrink"
instance Animatable "flex-shrink"

instance declarationFlexShrinkNumber ::
  ToNumber a =>
  Declaration "flex-shrink" a where
  pval = const $ val <<< number

-- https://www.w3.org/TR/css-flexbox-1/#propdef-flex-basis

flexBasis = Proxy :: Proxy "flex-basis"

instance Property "flex-basis"
instance Animatable "flex-basis"

instance declarationFlexBasisContent ::
  Declaration "flex-basis" (Proxy "content") where
  pval = const val

else instance declarationFlexBasisWidth ::
  Declaration "width" a =>
  Declaration "flex-basis" a where
  pval = const $ pval width

--------------------------------------------------------------------------------

-- Fonts
-- https://www.w3.org/TR/css-fonts-4/

class FontFaceDeclaration (d :: Symbol) (v :: Type) where
  fdval :: Proxy d -> v -> Val

data FontFace = FontFace

fontFace :: FontFace
fontFace = FontFace

-- https://www.w3.org/TR/css-fonts-4/#descdef-font-face-font-family

fontFamily = Proxy :: Proxy "font-family"

instance fontFaceDeclarationFontFamily ::
  FontFaceDeclaration "font-family" String where
  fdval = const $ mapVal quote <<< val

-- https://www.w3.org/TR/css-fonts-4/#descdef-font-face-src

src = Proxy :: Proxy "src"

newtype LocalFunction = LocalFunction Val

derive newtype instance ToVal LocalFunction

local :: String -> LocalFunction
local = LocalFunction <<< fn "local" <<< val <<< quote

collection = Proxy :: Proxy "collection"
embeddedOpentype = Proxy :: Proxy "embedded-opentype"
opentype = Proxy :: Proxy "opentype"
truetype = Proxy :: Proxy "truetype"
woff = Proxy :: Proxy "woff"
woff2 = Proxy :: Proxy "woff2"

class FontFormatKeyword (s :: Symbol)

instance FontFormatKeyword "collection"
instance FontFormatKeyword "embedded-opentype"
instance FontFormatKeyword "opentype"
instance FontFormatKeyword "svg"
instance FontFormatKeyword "truetype"
instance FontFormatKeyword "woff"
instance FontFormatKeyword "woff2"

newtype FontFaceFormatFunction = FontFaceFormatFunction Val

derive newtype instance ToVal FontFaceFormatFunction

format
  :: forall s
   . FontFormatKeyword s
  => IsSymbol s
  => Proxy s
  -> FontFaceFormatFunction
format =
  FontFaceFormatFunction <<< fn "format" <<< mapVal quote <<< val

class IsFontFaceSrcList (a :: Type)

instance IsFontFaceSrcList xs => IsFontFaceSrcList (LocalFunction /\ xs)
instance IsFontFaceSrcList xs => IsFontFaceSrcList (URL /\ xs)
instance
  IsFontFaceSrcList xs =>
  IsFontFaceSrcList (URL ~ FontFaceFormatFunction /\ xs)

instance IsFontFaceSrcList LocalFunction
instance IsFontFaceSrcList URL
instance IsFontFaceSrcList (URL ~ FontFaceFormatFunction)

instance fontFaceDeclarationSrc ::
  ( IsFontFaceSrcList a
  , MultiVal a
  ) =>
  FontFaceDeclaration "src" a where
  fdval = const $ intercalateMultiVal $ val "," <> Val _.separator

-- https://www.w3.org/TR/css-fonts-4/#descdef-font-face-font-style

fontStyle = Proxy :: Proxy "font-style"

auto = Proxy :: Proxy "auto"
italic = Proxy :: Proxy "italic"
oblique = Proxy :: Proxy "oblique"

class FontFaceFontStyleKeyword (s :: Symbol)

instance FontFaceFontStyleKeyword "auto"
instance FontFaceFontStyleKeyword "normal"
instance FontFaceFontStyleKeyword "italic"
instance FontFaceFontStyleKeyword "oblique"

class IsFontFaceFontStyle (a :: Type)

instance FontFaceFontStyleKeyword s => IsFontFaceFontStyle (Proxy s)
instance AngleTag t => IsFontFaceFontStyle (Proxy "oblique" ~ Measure t)
instance
  ( AngleTag tmin
  , AngleTag tmax
  ) =>
  IsFontFaceFontStyle (Proxy "oblique" ~ Measure tmin ~ Measure tmax)

instance fontFaceDeclarationFontStyle ::
  ( IsFontFaceFontStyle a
  , ToVal a
  ) =>
  FontFaceDeclaration "font-style" a where
  fdval = const val

-- https://www.w3.org/TR/css-fonts-4/#descdef-font-face-font-weight

fontWeight = Proxy :: Proxy "font-weight"

bold = Proxy :: Proxy "bold"

class FontFaceFontWeightKeyword (s :: Symbol)

instance FontFaceFontWeightKeyword "normal"
instance FontFaceFontWeightKeyword "bold"

class IsFontFaceFontWeight (a :: Type)

instance IsFontFaceFontWeight (Proxy "auto")
else instance FontFaceFontWeightKeyword s => IsFontFaceFontWeight (Proxy s)

instance IsFontFaceFontWeight Int
instance IsFontFaceFontWeight (Int ~ Int)
instance FontFaceFontWeightKeyword s => IsFontFaceFontWeight (Int ~ Proxy s)
instance FontFaceFontWeightKeyword s => IsFontFaceFontWeight (Proxy s ~ Int)
instance
  ( FontFaceFontWeightKeyword smin
  , FontFaceFontWeightKeyword smax
  ) =>
  IsFontFaceFontWeight (Proxy smin ~ Proxy smax)

instance fontFaceDeclarationFontWeight ::
  ( IsFontFaceFontWeight a
  , ToVal a
  ) =>
  FontFaceDeclaration "font-weight" a where
  fdval = const val

-- https://www.w3.org/TR/css-fonts-4/#descdef-font-face-font-stretch

fontStretch = Proxy :: Proxy "font-stretch"

instance fontFaceDeclarationFontStretchAuto ::
  FontFaceDeclaration "font-stretch" (Proxy "auto") where
  fdval = const val

else instance fontFaceDeclarationFontStretchRange ::
  ( Declaration "font-stretch" from
  , Declaration "font-stretch" to
  ) =>
  FontFaceDeclaration "font-stretch" (from ~ to) where
  fdval _ (from ~ to) = val $ pval fontStretch from ~ pval fontStretch to

else instance fontFaceDeclarationFontStretchSingle ::
  Declaration "font-stretch" a =>
  FontFaceDeclaration "font-stretch" a where
  fdval = pval

-- https://www.w3.org/TR/css-fonts-4/#propdef-font-family

instance Property "font-family"

cursive = Proxy :: Proxy "cursive"
emoji = Proxy :: Proxy "emoji"
fangsong = Proxy :: Proxy "fangsong"
fantasy = Proxy :: Proxy "fantasy"
math = Proxy :: Proxy "math"
monospace = Proxy :: Proxy "monospace"
sansSerif = Proxy :: Proxy "sans-serif"
serif = Proxy :: Proxy "serif"
systemUI = Proxy :: Proxy "system-ui"
uiMonospace = Proxy :: Proxy "ui-monospace"
uiRounded = Proxy :: Proxy "ui-rounded"
uiSansSerif = Proxy :: Proxy "ui-sans-serif"
uiSerif = Proxy :: Proxy "ui-serif"

class GenericFontFamilyKeyword (s :: Symbol)

instance GenericFontFamilyKeyword "cursive"
instance GenericFontFamilyKeyword "emoji"
instance GenericFontFamilyKeyword "fangsong"
instance GenericFontFamilyKeyword "fantasy"
instance GenericFontFamilyKeyword "math"
instance GenericFontFamilyKeyword "monospace"
instance GenericFontFamilyKeyword "sans-serif"
instance GenericFontFamilyKeyword "serif"
instance GenericFontFamilyKeyword "system-ui"
instance GenericFontFamilyKeyword "ui-monospace"
instance GenericFontFamilyKeyword "ui-rounded"
instance GenericFontFamilyKeyword "ui-sans-serif"
instance GenericFontFamilyKeyword "ui-serif"

class IsFontFamilyList (a :: Type)

instance GenericFontFamilyKeyword s => IsFontFamilyList (Proxy s)
instance IsFontFamilyList xs => IsFontFamilyList (String /\ xs)

instance declarationFontFamily ::
  ( IsFontFamilyList a
  , MultiVal a
  ) =>
  Declaration "font-family" a where
  pval =
    const $ buildFontFamilyList false <<< foldlMultiVal (flip (:)) Nil
    where
    buildFontFamilyList quoted =
      case _ of
        (x : xs) ->
          let
            sep =
              case xs of
                Nil ->
                  mempty
                _ ->
                  val "," <> Val _.separator
          in
            buildFontFamilyList true xs
              <> sep
              <> if quoted then mapVal quote x else x
        Nil ->
          mempty

-- https://www.w3.org/TR/css-fonts-4/#propdef-font-weight

instance Property "font-weight"
instance Animatable "font-weight"

bolder = Proxy :: Proxy "bolder"
lighter = Proxy :: Proxy "lighter"

class FontWeightKeyword (s :: Symbol)

instance FontWeightKeyword "normal"
instance FontWeightKeyword "bold"
instance FontWeightKeyword "bolder"
instance FontWeightKeyword "lighter"

class IsFontWeight (a :: Type)

instance IsFontWeight Int
instance FontWeightKeyword s => IsFontWeight (Proxy s)

instance declarationFontWeight ::
  ( IsFontWeight a
  , ToVal a
  ) =>
  Declaration "font-weight" a where
  pval = const val

-- https://www.w3.org/TR/css-fonts-4/#propdef-font-stretch

instance Property "font-stretch"
instance Animatable "font-stretch"

ultraCondensed = Proxy :: Proxy "ultra-condensed"
extraCondensed = Proxy :: Proxy "extra-condensed"
condensed = Proxy :: Proxy "condensed"
semiCondensed = Proxy :: Proxy "semi-condensed"
semiExpanded = Proxy :: Proxy "semi-expanded"
expanded = Proxy :: Proxy "expanded"
extraExpanded = Proxy :: Proxy "extra-expanded"
ultraExpanded = Proxy :: Proxy "ultra-expanded"

class FontStretchKeyword (s :: Symbol)

instance FontStretchKeyword "normal"
instance FontStretchKeyword "ultra-condensed"
instance FontStretchKeyword "extra-condensed"
instance FontStretchKeyword "condensed"
instance FontStretchKeyword "semi-condensed"
instance FontStretchKeyword "semi-expanded"
instance FontStretchKeyword "expanded"
instance FontStretchKeyword "extra-expanded"
instance FontStretchKeyword "ultra-expanded"

class IsFontStretch (a :: Type)

instance FontStretchKeyword s => IsFontStretch (Proxy s)
instance PercentageTag t => IsFontStretch (Measure t)

instance declarationFontStretch ::
  ( IsFontStretch a
  , ToVal a
  ) =>
  Declaration "font-stretch" a where
  pval = const val

-- https://www.w3.org/TR/css-fonts-4/#propdef-font-style

instance Property "font-style"

class FontStyleKeyword (s :: Symbol)

instance FontStyleKeyword "normal"
instance FontStyleKeyword "italic"
instance FontStyleKeyword "oblique"

class IsFontStyle (a :: Type)

instance FontStyleKeyword s => IsFontStyle (Proxy s)
instance AngleTag t => IsFontStyle (Proxy "oblique" ~ Measure t)

instance declarationFontStyle ::
  ( IsFontStyle a
  , ToVal a
  ) =>
  Declaration "font-style" a where
  pval = const val

-- https://www.w3.org/TR/css-fonts-4/#propdef-font-size

fontSize = Proxy :: Proxy "font-size"

instance Property "font-size"
instance Animatable "font-size"

xxSmall = Proxy :: Proxy "xx-small"
xSmall = Proxy :: Proxy "x-small"
small = Proxy :: Proxy "small"
large = Proxy :: Proxy "large"
xLarge = Proxy :: Proxy "x-large"
xxLarge = Proxy :: Proxy "xx-large"
larger = Proxy :: Proxy "larger"
smaller = Proxy :: Proxy "smaller"

class FontSizeKeyword (s :: Symbol)

instance FontSizeKeyword "xx-small"
instance FontSizeKeyword "x-small"
instance FontSizeKeyword "small"
instance FontSizeKeyword "medium"
instance FontSizeKeyword "large"
instance FontSizeKeyword "x-large"
instance FontSizeKeyword "xx-large"
instance FontSizeKeyword "larger"
instance FontSizeKeyword "smaller"

class IsFontSize (a :: Type)

instance FontSizeKeyword s => IsFontSize (Proxy s)
instance LengthPercentageTag t => IsFontSize (Measure t)

instance declarationFontSize ::
  ( IsFontSize a
  , ToVal a
  ) =>
  Declaration "font-size" a where
  pval = const val

-- https://www.w3.org/TR/css-fonts-4/#propdef-font-size-adjust

fontSizeAdjust = Proxy :: Proxy "font-size-adjust"

instance Property "font-size-adjust"
instance Animatable "font-size-adjust"

instance declarationFontSizeAdjust :: Declaration "font-size-adjust" Number where
  pval = const val

--------------------------------------------------------------------------------

-- Grid
-- https://www.w3.org/TR/css-grid-1/

-- Tags

data Track
data Fixed
data Names
data Auto
data NoAuto

-- https://www.w3.org/TR/css-grid-1/#typedef-flex

newtype Flex = Flex Number

instance ToVal Flex where
  val (Flex n) = val $ Number.toString n <> "fr"

fr :: forall n. ToNumber n => n -> Flex
fr = Flex <<< number

-- https://www.w3.org/TR/css-grid-1/#line-name

newtype LineName = LineName String

derive newtype instance ToVal LineName

lineName
  :: Warn
       ( Text
           "`lineName` is deprecated. Use the `LineName` constructor instead."
       )
  => String
  -> LineName
lineName = LineName

instance ToVal (List LineName) where
  val = (_ <> val "]") <<< val <<< go mempty
    where
    go acc Nil = "[" <> acc
    go "" (LineName x : xs) = go x xs
    go acc (LineName x : xs) = go (x <> " " <> acc) xs

-- https://www.w3.org/TR/css-grid-1/#typedef-track-breadth

class TrackBreadthKeyword (s :: Symbol)

instance TrackBreadthKeyword "min-content"
instance TrackBreadthKeyword "max-content"
instance TrackBreadthKeyword "auto"

-- https://www.w3.org/TR/css-grid-1/#valdef-grid-template-columns-minmax

newtype Minmax' (compat :: Type) = Minmax' Val

derive newtype instance ToVal (Minmax' a)

class Minmax (min :: Type) (max :: Type) (compat :: Type) | min max -> compat

instance
  ( TrackBreadthKeyword s
  , LengthPercentageTag t
  ) =>
  Minmax (Proxy s) (Measure t) (Track \/ Fixed)

instance
  ( LengthPercentageTag t
  , TrackBreadthKeyword s
  ) =>
  Minmax (Measure t) (Proxy s) (Track \/ Fixed)

instance LengthPercentageTag t => Minmax (Measure t) Flex (Track \/ Fixed)
instance
  ( LengthPercentageTag tmin
  , LengthPercentageTag tmax
  ) =>
  Minmax (Measure tmin) (Measure tmax) (Track \/ Fixed)

instance TrackBreadthKeyword s => Minmax (Proxy s) Flex Track
instance
  ( TrackBreadthKeyword smin
  , TrackBreadthKeyword smax
  ) =>
  Minmax (Proxy smin) (Proxy smax) Track

minmax
  :: forall min max compat
   . Minmax min max compat
  => MultiVal (min /\ max)
  => min
  -> max
  -> Minmax' compat
minmax m n = Minmax' $ fn "minmax" $ m /\ n

-- https://www.w3.org/TR/css-grid-1/#track-list

class
  TrackCompat (this :: Type) (previous :: Type) (out :: Type)
  | this previous -> out

instance TrackCompat (Track \/ Fixed) (Track \/ Fixed) (Track \/ Fixed)
instance TrackCompat (Track \/ Fixed) Track Track
instance TrackCompat (Track \/ Fixed) Fixed Fixed
instance TrackCompat Track (Track \/ Fixed) Track
instance TrackCompat Fixed (Track \/ Fixed) Fixed
instance TrackCompat Track Track Track
instance TrackCompat Names Names Names
else instance TrackCompat Names a a
else instance TrackCompat a Names a

-- https://www.w3.org/TR/css-grid-1/#repeat-notation

autoFill = Proxy :: Proxy "auto-fill"
autoFit = Proxy :: Proxy "auto-fit"

class AutoRepeatKeyword (s :: Symbol)

instance AutoRepeatKeyword "auto-fill"
instance AutoRepeatKeyword "auto-fit"

class FoldLineNames (i :: Type) (o :: Type) | i -> o where
  foldLineNames :: i -> o

instance
  FoldLineNames (List LineName /\ xsin) xsout =>
  FoldLineNames (LineName /\ LineName /\ xsin) xsout where
  foldLineNames (a' /\ b' /\ xs) = foldLineNames $ (b' : a' : Nil) /\ xs

else instance
  FoldLineNames (List LineName /\ xsin) xsout =>
  FoldLineNames (List LineName /\ LineName /\ xsin) xsout where
  foldLineNames (a' /\ b' /\ xs) = foldLineNames $ (b' : a') /\ xs

else instance FoldLineNames (List LineName /\ LineName) (List LineName) where
  foldLineNames (a' /\ b') = foldLineNames $ b' : a'

else instance
  FoldLineNames (List LineName /\ xsin) xsout =>
  FoldLineNames (LineName /\ xsin) xsout where
  foldLineNames (x /\ xs) = foldLineNames $ (x : Nil) /\ xs

else instance FoldLineNames LineName (List LineName) where
  foldLineNames = pure

else instance FoldLineNames xsin xsout => FoldLineNames (x /\ xsin) (x /\ xsout) where
  foldLineNames (x /\ xs) = x /\ foldLineNames xs

else instance FoldLineNames x x where
  foldLineNames = identity

class RepeatTrackList (xs :: Type) (compat :: Type) | xs -> compat

instance LengthPercentageTag t => RepeatTrackList (Measure t) (Track \/ Fixed)
instance RepeatTrackList Flex Track
instance TrackBreadthKeyword s => RepeatTrackList (Proxy s) Track
instance RepeatTrackList (Minmax' compat) compat
instance RepeatTrackList FitContent Track
instance RepeatTrackList LineName Names

instance
  ( RepeatTrackList xs previouscompat
  , TrackCompat (Track \/ Fixed) previouscompat compat
  , LengthPercentageTag t
  ) =>
  RepeatTrackList (Measure t /\ xs) compat

instance
  ( RepeatTrackList xs previouscompat
  , TrackCompat Track previouscompat compat
  ) =>
  RepeatTrackList (Flex /\ xs) compat

instance
  ( RepeatTrackList xs previouscompat
  , TrackCompat Track previouscompat compat
  , TrackBreadthKeyword s
  ) =>
  RepeatTrackList (Proxy s /\ xs) compat

instance
  ( RepeatTrackList xs previouscompat
  , TrackCompat thiscompat previouscompat compat
  ) =>
  RepeatTrackList (Minmax' thiscompat /\ xs) compat

instance
  ( RepeatTrackList xs previouscompat
  , TrackCompat Track previouscompat compat
  ) =>
  RepeatTrackList (FitContent /\ xs) compat

instance RepeatTrackList xs compat => RepeatTrackList (LineName /\ xs) compat

newtype Repeat' (compat :: Type) = Repeat' Val

derive newtype instance ToVal (Repeat' a)

class Repeat (n :: Type) (tracks :: Type) (compat :: Type) | n tracks -> compat

instance
  ( RepeatTrackList xs compat
  , TrackCompat Fixed compat Fixed
  , AutoRepeatKeyword s
  ) =>
  Repeat (Proxy s) xs Auto

instance RepeatTrackList xs compat => Repeat Int xs compat

repeat
  :: forall n tracks tracks' compat
   . Repeat n tracks compat
  => MultiVal (n /\ Val)
  => FoldLineNames tracks tracks'
  => MultiVal tracks'
  => n
  -> tracks
  -> Repeat' compat
repeat n = Repeat' <<< fn "repeat" <<< (n /\ _) <<< intercalateMultiVal " " <<<
  foldLineNames

-- https://www.w3.org/TR/css-grid-1/#typedef-track-list

class TrackList (xs :: Type) (auto :: Type) (compat :: Type) | xs -> auto compat

instance LengthPercentageTag t => TrackList (Measure t) NoAuto (Track \/ Fixed)
instance TrackList Flex NoAuto Track
instance TrackBreadthKeyword s => TrackList (Proxy s) NoAuto Track
instance TrackList (Minmax' compat) NoAuto compat
instance TrackList FitContent NoAuto compat
instance TrackList (Repeat' Auto) Auto Fixed
else instance TrackList (Repeat' compat) NoAuto compat

instance TrackList LineName NoAuto (Track \/ Fixed)

instance
  ( TrackList xs auto tailcompat
  , TrackCompat (Track \/ Fixed) tailcompat compat
  , LengthPercentageTag t
  ) =>
  TrackList (Measure t /\ xs) auto compat

instance
  ( TrackList xs auto tailcompat
  , TrackCompat Track tailcompat compat
  ) =>
  TrackList (Flex /\ xs) auto compat

instance
  ( TrackList xs auto tailcompat
  , TrackCompat Track tailcompat compat
  , TrackBreadthKeyword s
  ) =>
  TrackList (Proxy s /\ xs) auto compat

instance
  ( TrackList xs auto tailcompat
  , TrackCompat minmaxcompat tailcompat compat
  ) =>
  TrackList (Minmax' minmaxcompat /\ xs) auto compat

instance
  ( TrackList xs auto tailcompat
  , TrackCompat Track tailcompat compat
  ) =>
  TrackList (FitContent /\ xs) auto compat

instance
  ( TrackList xs NoAuto tailcompat
  , TrackCompat Fixed tailcompat compat
  ) =>
  TrackList (Repeat' Auto /\ xs) Auto compat
else instance
  ( TrackList xs auto tailcompat
  , TrackCompat repeatcompat tailcompat compat
  ) =>
  TrackList (Repeat' repeatcompat /\ xs) auto compat

instance TrackList xs auto compat => TrackList (LineName /\ xs) auto compat

-- https://www.w3.org/TR/css-grid-1/#propdef-grid-template-columns

gridTemplateColumns = Proxy :: Proxy "grid-template-columns"

instance Property "grid-template-columns"
instance Animatable "grid-template-columns"

instance declarationGridTemplateColumnsNone ::
  Declaration "grid-template-columns" (Proxy "none") where
  pval = const val

else instance declarationGridTemplateColumnsTrackList ::
  ( TrackList tracks auto compat
  , FoldLineNames tracks tracks'
  , MultiVal tracks'
  ) =>
  Declaration "grid-template-columns" tracks where
  pval = const $ intercalateMultiVal " " <<< foldLineNames

-- https://www.w3.org/TR/css-grid-1/#propdef-grid-template-rows

gridTemplateRows = Proxy :: Proxy "grid-template-rows"

instance Property "grid-template-rows"
instance Animatable "grid-template-rows"

instance declarationGridTemplateRowsNone ::
  Declaration "grid-template-rows" (Proxy "none") where
  pval = const val

else instance declarationGridTemplateRowsTrackList ::
  ( TrackList tracks auto compat
  , FoldLineNames tracks tracks'
  , MultiVal tracks'
  ) =>
  Declaration "grid-template-rows" tracks where
  pval = const $ intercalateMultiVal " " <<< foldLineNames

-- https://www.w3.org/TR/css-grid-1/#propdef-grid-auto-columns

gridAutoColumns = Proxy :: Proxy "grid-auto-columns"

instance Property "grid-auto-columns"

instance declarationGridAutoColumnsLengthPercentage ::
  LengthPercentageTag t =>
  Declaration "grid-auto-columns" (Measure t) where
  pval = const val

instance declarationGridAutoColumnsFlex :: Declaration "grid-auto-columns" Flex where
  pval = const val

instance declarationGridAutoColumnsTrackBreadthKeyword ::
  ( IsSymbol s
  , TrackBreadthKeyword s
  ) =>
  Declaration "grid-auto-columns" (Proxy s) where
  pval = const val

instance declarationGridAutoColumnsMinmax ::
  TrackCompat Track compat' compat =>
  Declaration "grid-auto-columns" (Minmax' compat') where
  pval = const val

instance declarationGridAutioColumnsFitContent ::
  Declaration "grid-auto-columns" FitContent where
  pval = const val

instance declarationGridAutoColumnsList ::
  ( Declaration "grid-auto-columns" x
  , Declaration "grid-auto-columns" xs
  ) =>
  Declaration "grid-auto-columns" (x /\ xs) where
  pval p' (x /\ xs) = pval p' x <> val " " <> pval p' xs

-- https://www.w3.org/TR/css-grid-1/#propdef-grid-auto-rows

gridAutoRows = Proxy :: Proxy "grid-auto-rows"

instance Property "grid-auto-rows"

instance declarationGridAutoRowsGridAutoColumns ::
  Declaration "grid-auto-columns" v =>
  Declaration "grid-auto-rows" v where
  pval = const $ pval (Proxy :: Proxy "grid-auto-columns")

-- https://www.w3.org/TR/css-grid-1/#propdef-grid-auto-flow

gridAutoFlow = Proxy :: Proxy "grid-auto-flow"

instance Property "grid-auto-flow"

dense = Proxy :: Proxy "dense"

instance declarationGridAutoFlowRow ::
  Declaration "grid-auto-flow" (Proxy "row") where
  pval = const val

instance declarationGridAutoFlowColumn ::
  Declaration "grid-auto-flow" (Proxy "column") where
  pval = const val

instance declarationGridAutoFlowDense ::
  Declaration "grid-auto-flow" (Proxy "dense") where
  pval = const val

instance declarationGridAutoFlowRowDense ::
  Declaration "grid-auto-flow" (Proxy "row" ~ Proxy "dense") where
  pval = const val

instance declarationGridAutoFlowColumnDense ::
  Declaration "grid-auto-flow" (Proxy "column" ~ Proxy "dense") where
  pval = const val

-- https://www.w3.org/TR/css-grid-1/#propdef-grid-row-start

gridRowStart = Proxy :: Proxy "grid-row-start"

instance Property "grid-row-start"

instance declarationGridRowStartAuto ::
  Declaration "grid-row-start" (Proxy "auto") where
  pval = const val

instance declarationGridRowStartLineName ::
  Declaration "grid-row-start" LineName where
  pval = const val

instance declarationGridRowStartInt :: Declaration "grid-row-start" Int where
  pval = const val

instance declarationGridRowStartIntLineName ::
  Declaration "grid-row-start" (Int ~ LineName) where
  pval = const val

instance declarationGridRowStartSpanInt ::
  Declaration "grid-row-start" (Proxy "span" ~ Int) where
  pval = const val

instance declarationGridRowStartSpanLineName ::
  Declaration "grid-row-start" (Proxy "span" ~ LineName) where
  pval = const val

instance declarationGridRowStartSpanIntLineName ::
  Declaration "grid-row-start" (Proxy "span" ~ Int ~ LineName) where
  pval = const val

-- https://www.w3.org/TR/css-grid-1/#propdef-grid-column-start

gridColumnStart = Proxy :: Proxy "grid-column-start"

instance Property "grid-column-start"

instance declarationGridColumnStartGridRowStart ::
  Declaration "grid-row-start" a =>
  Declaration "grid-column-start" a where
  pval = const $ pval (Proxy :: Proxy "grid-row-start")

-- https://www.w3.org/TR/css-grid-1/#propdef-grid-row-end

gridRowEnd = Proxy :: Proxy "grid-row-end"

instance Property "grid-row-end"

instance declarationGridRowEndGridRowStart ::
  Declaration "grid-row-start" a =>
  Declaration "grid-row-end" a where
  pval = const $ pval (Proxy :: Proxy "grid-row-start")

-- https://www.w3.org/TR/css-grid-1/#propdef-grid-column-end

gridColumnEnd = Proxy :: Proxy "grid-column-end"

instance Property "grid-column-end"

instance declarationGridColumnEndGridRowStart ::
  Declaration "grid-row-start" a =>
  Declaration "grid-column-end" a where
  pval = const $ pval (Proxy :: Proxy "grid-row-start")

--------------------------------------------------------------------------------

-- Images
-- https://www.w3.org/TR/css-images-3/

-- https://www.w3.org/TR/css-images-3/#typedef-image

class IsImage (a :: Type)

instance IsImage URL

class IsColorStopListHead (a :: Type)

instance
  ( IsColor color
  , LengthPercentageTag tpos
  ) =>
  IsColorStopListHead (color ~ Measure tpos)
else instance IsColor color => IsColorStopListHead color

class IsColorStopListTail (a :: Type)

instance
  ( LengthPercentageTag thint
  , IsColor color
  , LengthPercentageTag tpos
  , IsColorStopListTail tail
  ) =>
  IsColorStopListTail (Measure thint /\ color ~ Measure tpos /\ tail)
else instance
  ( LengthPercentageTag thint
  , IsColor color
  , IsColorStopListTail tail
  ) =>
  IsColorStopListTail (Measure thint /\ color /\ tail)
else instance
  ( IsColor color
  , LengthPercentageTag tpos
  , IsColorStopListTail tail
  ) =>
  IsColorStopListTail (color ~ Measure tpos /\ tail)
else instance
  ( LengthPercentageTag thint
  , IsColor color
  , LengthPercentageTag tpos
  ) =>
  IsColorStopListTail (Measure thint /\ color ~ Measure tpos)
else instance
  ( LengthPercentageTag thint
  , IsColor color
  ) =>
  IsColorStopListTail (Measure thint /\ color)
else instance
  ( IsColor color
  , LengthPercentageTag tpos
  ) =>
  IsColorStopListTail (color ~ Measure tpos)
else instance
  ( IsColor color
  , IsColorStopListTail tail
  ) =>
  IsColorStopListTail (color /\ tail)
else instance IsColor color => IsColorStopListTail color

newtype Gradient (repeating :: Type) = Gradient Val

instance IsImage (Gradient a)

instance ToVal (Gradient a) where
  val (Gradient g) = g

data Repeating

repeating :: Gradient Unit -> Gradient Repeating
repeating (Gradient g) = Gradient $ val "repeating-" <> g

-- https://www.w3.org/TR/css-images-3/#linear-gradient-syntax

linearGradient
  :: forall t csh cst
   . AngleTag t
  => IsColorStopListHead csh
  => IsColorStopListTail cst
  => MultiVal (csh /\ cst)
  => Measure t
  -> (csh /\ cst)
  -> Gradient Unit
linearGradient angle colorStops =
  Gradient $ fn "linear-gradient" $ angle /\ colorStops

-- https://www.w3.org/TR/css-images-3/#radial-gradient-syntax

closestCorner = Proxy :: Proxy "closest-corner"
closestSide = Proxy :: Proxy "closest-side"
farthestCorner = Proxy :: Proxy "farthest-corner"
farthestSide = Proxy :: Proxy "farthest-side"

class ExtentKeyword (s :: Symbol)

instance ExtentKeyword "closest-corner"
instance ExtentKeyword "closest-side"
instance ExtentKeyword "farthest-corner"
instance ExtentKeyword "farthest-side"

circle = Proxy :: Proxy "circle"
ellipse = Proxy :: Proxy "ellipse"

class ShapeKeyword (s :: Symbol)

instance ShapeKeyword "circle"
instance ShapeKeyword "ellipse"

class IsRadialGradientDimensions (a :: Type)

instance ShapeKeyword s => IsRadialGradientDimensions (Proxy s)
instance
  ( ShapeKeyword sshape
  , ExtentKeyword sextent
  ) =>
  IsRadialGradientDimensions (Proxy sshape ~ Proxy sextent)

instance LengthTag t => IsRadialGradientDimensions (Measure t)
instance
  ( LengthPercentageTag tx
  , LengthPercentageTag ty
  ) =>
  IsRadialGradientDimensions (Measure tx ~ Measure ty)

radialGradient
  :: forall dimensions pos csh cst
   . IsRadialGradientDimensions dimensions
  => ToVal dimensions
  => IsPosition pos
  => ToVal pos
  => IsColorStopListHead csh
  => IsColorStopListTail cst
  => MultiVal (csh /\ cst)
  => dimensions
  -> pos
  -> (csh /\ cst)
  -> Gradient Unit
radialGradient dimensions pos colorStops =
  Gradient
    $ fn "radial-gradient"
    $ (val dimensions <> val " at " <> val pos) /\ colorStops

--------------------------------------------------------------------------------

-- Inline Layout
-- https://www.w3.org/TR/css-inline-3/

-- https://www.w3.org/TR/css-inline-3/#propdef-dominant-baseline

dominantBaseline = Proxy :: Proxy "dominant-baseline"

instance Property "dominant-baseline"

textBottom = Proxy :: Proxy "text-bottom"
alphabetic = Proxy :: Proxy "alphabetic"
ideographic = Proxy :: Proxy "ideographic"
middle = Proxy :: Proxy "middle"
central = Proxy :: Proxy "central"
mathematical = Proxy :: Proxy "mathematical"
hanging = Proxy :: Proxy "hanging"
textTop = Proxy :: Proxy "text-top"

class DominantBaselineKeyword (s :: Symbol)

instance DominantBaselineKeyword "auto"
instance DominantBaselineKeyword "text-bottom"
instance DominantBaselineKeyword "alphabetic"
instance DominantBaselineKeyword "ideographic"
instance DominantBaselineKeyword "middle"
instance DominantBaselineKeyword "central"
instance DominantBaselineKeyword "mathematical"
instance DominantBaselineKeyword "hanging"
instance DominantBaselineKeyword "text-top"

instance declarationDominantBaseline ::
  ( DominantBaselineKeyword s
  , ToVal (Proxy s)
  ) =>
  Declaration "dominant-baseline" (Proxy s) where
  pval = const val

-- https://www.w3.org/TR/css-inline-3/#propdef-vertical-align

verticalAlign = Proxy :: Proxy "vertical-align"

instance Property "vertical-align"
instance Animatable "vertical-align"

class AlignmentBaselineOrBaselineShiftKeyword (s :: Symbol)

instance AlignmentBaselineOrBaselineShiftKeyword "baseline"
instance AlignmentBaselineOrBaselineShiftKeyword "text-bottom"
instance AlignmentBaselineOrBaselineShiftKeyword "alphabetic"
instance AlignmentBaselineOrBaselineShiftKeyword "ideographic"
instance AlignmentBaselineOrBaselineShiftKeyword "middle"
instance AlignmentBaselineOrBaselineShiftKeyword "central"
instance AlignmentBaselineOrBaselineShiftKeyword "mathematical"
instance AlignmentBaselineOrBaselineShiftKeyword "text-top"
instance AlignmentBaselineOrBaselineShiftKeyword "sub"
instance AlignmentBaselineOrBaselineShiftKeyword "super"
instance AlignmentBaselineOrBaselineShiftKeyword "top"
instance AlignmentBaselineOrBaselineShiftKeyword "center"
instance AlignmentBaselineOrBaselineShiftKeyword "bottom"

class IsVerticalAlign (a :: Type)

instance
  ( AlignmentBaselineKeyword sa
  , BaselineShiftKeyword sb
  ) =>
  IsVerticalAlign (Proxy "first" ~ Proxy sa ~ Proxy sb)

instance
  ( AlignmentBaselineKeyword sa
  , LengthPercentageTag tb
  ) =>
  IsVerticalAlign (Proxy "first" ~ Proxy sa ~ Measure tb)

instance
  ( AlignmentBaselineKeyword sa
  , BaselineShiftKeyword sb
  ) =>
  IsVerticalAlign (Proxy "last" ~ Proxy sa ~ Proxy sb)

instance
  ( AlignmentBaselineKeyword sa
  , LengthPercentageTag tb
  ) =>
  IsVerticalAlign (Proxy "last" ~ Proxy sa ~ Measure tb)

instance
  AlignmentBaselineOrBaselineShiftKeyword s =>
  IsVerticalAlign (Proxy "first" ~ Proxy s)
else instance
  AlignmentBaselineOrBaselineShiftKeyword s =>
  IsVerticalAlign (Proxy "last" ~ Proxy s)
else instance
  ( AlignmentBaselineKeyword sa
  , BaselineShiftKeyword sb
  ) =>
  IsVerticalAlign (Proxy sa ~ Proxy sb)

instance LengthPercentageTag t => IsVerticalAlign (Proxy "first" ~ Measure t)
else instance
  LengthPercentageTag t =>
  IsVerticalAlign (Proxy "last" ~ Measure t)
else instance
  ( AlignmentBaselineKeyword s
  , LengthPercentageTag t
  ) =>
  IsVerticalAlign (Proxy s ~ Measure t)

instance IsVerticalAlign (Proxy "first")
else instance IsVerticalAlign (Proxy "last")
else instance
  AlignmentBaselineOrBaselineShiftKeyword s =>
  IsVerticalAlign (Proxy s)

instance LengthPercentageTag t => IsVerticalAlign (Measure t)

instance declarationVerticalAlign ::
  ( IsVerticalAlign a
  , ToVal a
  ) =>
  Declaration "vertical-align" a where
  pval = const val

-- https://www.w3.org/TR/css-inline-3/#propdef-baseline-source

baselineSource = Proxy :: Proxy "baseline-source"

instance Property "baseline-source"

first = Proxy :: Proxy "first"
last = Proxy :: Proxy "last"

class BaselineSourceKeyword (s :: Symbol)

instance BaselineSourceKeyword "auto"
instance BaselineSourceKeyword "first"
instance BaselineSourceKeyword "last"

instance declarationBaselineSource ::
  ( BaselineSourceKeyword s
  , ToVal (Proxy s)
  ) =>
  Declaration "baseline-source" (Proxy s) where
  pval = const val

-- https://www.w3.org/TR/css-inline-3/#propdef-alignment-baseline

alignmentBaseline = Proxy :: Proxy "alignment-baseline"

instance Property "alignment-baseline"

class AlignmentBaselineKeyword (s :: Symbol)

instance AlignmentBaselineKeyword "baseline"
instance AlignmentBaselineKeyword "text-bottom"
instance AlignmentBaselineKeyword "alphabetic"
instance AlignmentBaselineKeyword "ideographic"
instance AlignmentBaselineKeyword "middle"
instance AlignmentBaselineKeyword "central"
instance AlignmentBaselineKeyword "mathematical"
instance AlignmentBaselineKeyword "text-top"

instance declarationAlignmentBaseline ::
  ( AlignmentBaselineKeyword s
  , ToVal (Proxy s)
  ) =>
  Declaration "alignment-baseline" (Proxy s) where
  pval = const val

-- https://www.w3.org/TR/css-inline-3/#propdef-baseline-shift

baselineShift = Proxy :: Proxy "baseline-shift"

instance Property "baseline-shift"

sub = Proxy :: Proxy "sub"
super = Proxy :: Proxy "super"
top = Proxy :: Proxy "top"
bottom = Proxy :: Proxy "bottom"

class BaselineShiftKeyword (s :: Symbol)

instance BaselineShiftKeyword "sub"
instance BaselineShiftKeyword "super"
instance BaselineShiftKeyword "top"
instance BaselineShiftKeyword "center"
instance BaselineShiftKeyword "bottom"

class IsBaselineShift (a :: Type)

instance LengthPercentageTag t => IsBaselineShift (Measure t)
instance BaselineShiftKeyword s => IsBaselineShift (Proxy s)

instance declarationBaselineShift ::
  ( IsBaselineShift a
  , ToVal a
  ) =>
  Declaration "baseline-shift" a where
  pval = const val

-- https://www.w3.org/TR/css-inline-3/#propdef-line-height

lineHeight = Proxy :: Proxy "line-height"

instance Property "line-height"
instance Animatable "line-height"

class IsLineHeight (a :: Type)

instance IsLineHeight (Proxy "normal")
instance LengthPercentageTag t => IsLineHeight (Measure t)

instance declarationLineHeightNormal ::
  Declaration "line-height" (Proxy "normal") where
  pval = const val

else instance declarationLineHeightLengthPercentage ::
  LengthPercentageTag t =>
  Declaration "line-height" (Measure t) where
  pval = const val

else instance declarationLineHeightNumber ::
  ToNumber a =>
  Declaration "line-height" a where
  pval = const $ val <<< number

--------------------------------------------------------------------------------

-- Lists and Counters
-- https://www.w3.org/TR/css-lists-3/

-- https://www.w3.org/TR/css-lists-3/#propdef-list-style-image

listStyleImage = Proxy :: Proxy "list-style-image"

instance Property "list-style-image"

class IsListStyleImage (a :: Type)

instance IsListStyleImage (Proxy "none")
else instance IsImage a => IsListStyleImage a

instance declarationListStyleImage ::
  ( IsListStyleImage a
  , ToVal a
  ) =>
  Declaration "list-style-image" a where
  pval = const val

-- https://www.w3.org/TR/css-lists-3/#propdef-list-style-type

listStyleType = Proxy :: Proxy "list-style-type"

instance Property "list-style-type"

decimal = Proxy :: Proxy "decimal"
decimalLeadingZero = Proxy :: Proxy "decimal-leading-zero"
arabicIndic = Proxy :: Proxy "arabic-indic"
armenian = Proxy :: Proxy "armenian"
upperArmenian = Proxy :: Proxy "upper-armenian"
lowerArmenian = Proxy :: Proxy "lower-armenian"
bengali = Proxy :: Proxy "bengali"
cambodian = Proxy :: Proxy "cambodian"
khmer = Proxy :: Proxy "khmer"
cjkDecimal = Proxy :: Proxy "cjk-decimal"
devanagari = Proxy :: Proxy "devanagari"
georgian = Proxy :: Proxy "georgian"
gujarati = Proxy :: Proxy "gujarati"
gurmukhi = Proxy :: Proxy "gurmukhi"
hebrew = Proxy :: Proxy "hebrew"
kannada = Proxy :: Proxy "kannada"
lao = Proxy :: Proxy "lao"
malayalam = Proxy :: Proxy "malayalam"
mongolian = Proxy :: Proxy "mongolian"
myanmar = Proxy :: Proxy "myanmar"
oriya = Proxy :: Proxy "oriya"
persian = Proxy :: Proxy "persian"
lowerRoman = Proxy :: Proxy "lower-roman"
upperRoman = Proxy :: Proxy "upper-roman"
tamil = Proxy :: Proxy "tamil"
telugu = Proxy :: Proxy "telugu"
thai = Proxy :: Proxy "thai"
tibetan = Proxy :: Proxy "tibetan"
lowerAlpha = Proxy :: Proxy "lower-alpha"
lowerLatin = Proxy :: Proxy "lower-latin"
upperAlpha = Proxy :: Proxy "upper-alpha"
upperLatin = Proxy :: Proxy "upper-latin"
lowerGreek = Proxy :: Proxy "lower-greek"
hiragana = Proxy :: Proxy "hiragana"
hiraganaIroha = Proxy :: Proxy "hiragana-iroha"
katakana = Proxy :: Proxy "katakana"
katakanaIroha = Proxy :: Proxy "katakana-iroha"
disc = Proxy :: Proxy "disc"
square = Proxy :: Proxy "square"
disclosureOpen = Proxy :: Proxy "disclosure-open"
disclosureClosed = Proxy :: Proxy "disclosure-closed"
cjkEarthlyBranch = Proxy :: Proxy "cjk-earthly-branch"
cjkHeavenlyStem = Proxy :: Proxy "cjk-heavenly-stem"

class CounterStyleKeyword (s :: Symbol)

instance CounterStyleKeyword "decimal"
instance CounterStyleKeyword "decimal-leading-zero"
instance CounterStyleKeyword "arabic-indic"
instance CounterStyleKeyword "armenian"
instance CounterStyleKeyword "upper-armenian"
instance CounterStyleKeyword "lower-armenian"
instance CounterStyleKeyword "bengali"
instance CounterStyleKeyword "cambodian"
instance CounterStyleKeyword "khmer"
instance CounterStyleKeyword "cjk-decimal"
instance CounterStyleKeyword "devanagari"
instance CounterStyleKeyword "georgian"
instance CounterStyleKeyword "gujarati"
instance CounterStyleKeyword "gurmukhi"
instance CounterStyleKeyword "hebrew"
instance CounterStyleKeyword "kannada"
instance CounterStyleKeyword "lao"
instance CounterStyleKeyword "malayalam"
instance CounterStyleKeyword "mongolian"
instance CounterStyleKeyword "myanmar"
instance CounterStyleKeyword "oriya"
instance CounterStyleKeyword "persian"
instance CounterStyleKeyword "lower-roman"
instance CounterStyleKeyword "upper-roman"
instance CounterStyleKeyword "tamil"
instance CounterStyleKeyword "telugu"
instance CounterStyleKeyword "thai"
instance CounterStyleKeyword "tibetan"
instance CounterStyleKeyword "lower-alpha"
instance CounterStyleKeyword "lower-latin"
instance CounterStyleKeyword "upper-alpha"
instance CounterStyleKeyword "upper-latin"
instance CounterStyleKeyword "lower-greek"
instance CounterStyleKeyword "hiragana"
instance CounterStyleKeyword "hiragana-iroha"
instance CounterStyleKeyword "katakana"
instance CounterStyleKeyword "katakana-iroha"
instance CounterStyleKeyword "disc"
instance CounterStyleKeyword "circle"
instance CounterStyleKeyword "square"
instance CounterStyleKeyword "disclosure-open"
instance CounterStyleKeyword "disclosure-closed"
instance CounterStyleKeyword "cjk-earthly-branch"
instance CounterStyleKeyword "cjk-heavenly-stem"

class IsListStyleType (a :: Type)

instance IsListStyleType (Proxy "none")
else instance CounterStyleKeyword s => IsListStyleType (Proxy s)

instance declarationListStyleType ::
  ( IsListStyleType a
  , ToVal a
  ) =>
  Declaration "list-style-type" a where
  pval = const val

-- https://www.w3.org/TR/css-lists-3/#propdef-list-style-position

listStylePosition = Proxy :: Proxy "list-style-position"

instance Property "list-style-position"

inside = Proxy :: Proxy "inside"
outside = Proxy :: Proxy "outside"

class ListStylePositionKeyword (s :: Symbol)

instance ListStylePositionKeyword "inside"
instance ListStylePositionKeyword "outside"

instance declarationListStylePosition ::
  ( ListStylePositionKeyword s
  , ToVal (Proxy s)
  ) =>
  Declaration "list-style-position" (Proxy s) where
  pval = const val

--------------------------------------------------------------------------------

-- Masking
-- https://www.w3.org/TR/css-masking-1/

-- https://www.w3.org/TR/css-masking-1/#propdef-mask-image

maskImage = Proxy :: Proxy "mask-image"

instance Property "mask-image"

class IsMaskReferenceList (a :: Type)

instance IsMaskReferenceList (Proxy "none")
else instance IsMaskReferenceList xs => IsMaskReferenceList (Proxy "none" /\ xs)
else instance
  ( IsImage x
  , IsMaskReferenceList xs
  ) =>
  IsMaskReferenceList (x /\ xs)
else instance IsImage a => IsMaskReferenceList a

instance declarationMaskImage ::
  ( IsMaskReferenceList a
  , MultiVal a
  ) =>
  Declaration "mask-image" a where
  pval = const $ intercalateMultiVal $ val "," <> Val _.separator

--------------------------------------------------------------------------------

-- Media Queries
-- https://www.w3.org/TR/mediaqueries-3/

print = Proxy :: Proxy "print"
screen = Proxy :: Proxy "screen"

class MediaTypeKeyword (s :: Symbol)

instance MediaTypeKeyword "all"
instance MediaTypeKeyword "print"
instance MediaTypeKeyword "screen"

data MediaQuery = MediaQuery Val (List (Val /\ Val))

instance ToVal MediaQuery where
  val (MediaQuery mt features) =
    let
      fq =
        foldl
          ( \acc (f /\ v) ->
              val " and ("
                <> f
                <> val ":"
                <> Val _.separator
                <> v
                <> val ")"
                <> acc
          )
          mempty
          features
    in
      val "media " <> val mt <> fq

class MediaFeature (f :: Symbol) (v :: Type)

class CollectMediaFeatures (rl :: RowList Type) (r :: Row Type) where
  collectMediaFeatures
    :: Proxy rl
    -> Record r
    -> List (Val /\ Val)
    -> List (Val /\ Val)

instance CollectMediaFeatures RL.Nil r where
  collectMediaFeatures _ _ = identity

instance
  ( IsSymbol f
  , MediaFeature f v
  , ToVal v
  , CollectMediaFeatures tailRowList row
  , Row.Cons f v tailRow row
  ) =>
  CollectMediaFeatures (RL.Cons f v tailRowList) row where
  collectMediaFeatures _ rec acc =
    let
      field = Proxy :: _ f
      feature = mapVal camelToKebab (val field) /\ val (Record.get field rec)
    in
      collectMediaFeatures (Proxy :: _ tailRowList) rec (feature : acc)

media
  :: forall smt r rl
   . MediaTypeKeyword smt
  => IsSymbol smt
  => RowToList r rl
  => CollectMediaFeatures rl r
  => Proxy smt
  -> Record r
  -> MediaQuery
media mt features =
  MediaQuery (val mt) $ collectMediaFeatures (Proxy :: _ rl) features Nil

-- https://www.w3.org/TR/mediaqueries-3/#width

instance LengthTag t => MediaFeature "width" (Measure t)
instance LengthTag t => MediaFeature "minWidth" (Measure t)
instance LengthTag t => MediaFeature "maxWidth" (Measure t)

-- https://www.w3.org/TR/mediaqueries-3/#height

instance LengthTag t => MediaFeature "height" (Measure t)
instance LengthTag t => MediaFeature "minHeight" (Measure t)
instance LengthTag t => MediaFeature "maxHeight" (Measure t)

-- https://www.w3.org/TR/mediaqueries-3/#device-width

instance LengthTag t => MediaFeature "deviceWidth" (Measure t)
instance LengthTag t => MediaFeature "minDeviceWidth" (Measure t)
instance LengthTag t => MediaFeature "maxDeviceWidth" (Measure t)

-- https://www.w3.org/TR/mediaqueries-3/#device-height

instance LengthTag t => MediaFeature "deviceHeight" (Measure t)
instance LengthTag t => MediaFeature "minDeviceHeight" (Measure t)
instance LengthTag t => MediaFeature "maxDeviceHeight" (Measure t)

-- https://www.w3.org/TR/mediaqueries-3/#orientation

newtype Orientation = Orientation String

derive newtype instance ToVal Orientation

portrait :: Orientation
portrait = Orientation "portrait"

landscape :: Orientation
landscape = Orientation "landscape"

instance MediaFeature "orientation" Orientation

-- https://www.w3.org/TR/mediaqueries-3/#aspect-ratio

data Ratio = Ratio Int Int

instance ToVal Ratio where
  val (Ratio num den) = val num <> val "/" <> val den

infix 5 Ratio as :/

instance MediaFeature "aspectRatio" Ratio
instance MediaFeature "minAspectRatio" Ratio
instance MediaFeature "maxAspectRatio" Ratio

-- https://www.w3.org/TR/mediaqueries-3/#device-aspect-ratio

instance MediaFeature "deviceAspectRatio" Ratio
instance MediaFeature "minDeviceAspectRatio" Ratio
instance MediaFeature "maxDeviceAspectRatio" Ratio

-- https://www.w3.org/TR/mediaqueries-3/#color

instance MediaFeature "color" Int
instance MediaFeature "minColor" Int
instance MediaFeature "maxColor" Int

-- https://www.w3.org/TR/mediaqueries-3/#color-index

instance MediaFeature "colorIndex" Int
instance MediaFeature "minColorIndex" Int
instance MediaFeature "maxColorIndex" Int

-- https://www.w3.org/TR/mediaqueries-3/#monochrome

instance MediaFeature "monochrome" Int
instance MediaFeature "minMonochrome" Int
instance MediaFeature "maxMonochrome" Int

-- https://www.w3.org/TR/mediaqueries-3/#resolution

newtype Resolution = Resolution String

derive newtype instance ToVal Resolution

dpi :: Int -> Resolution
dpi x = Resolution $ show x <> "dpi"

dpcm :: Int -> Resolution
dpcm x = Resolution $ show x <> "dpcm"

instance MediaFeature "resolution" Resolution
instance MediaFeature "minResolution" Resolution
instance MediaFeature "maxResolution" Resolution

--------------------------------------------------------------------------------

-- Overflow
-- https://www.w3.org/TR/css-overflow-3/

-- https://www.w3.org/TR/css-overflow-3/#propdef-overflow-x

overflowX = Proxy :: Proxy "overflow-x"

instance Property "overflow-x"

visible = Proxy :: Proxy "visible"
clip = Proxy :: Proxy "clip"
scroll = Proxy :: Proxy "scroll"

class OverflowKeyword (s :: Symbol)

instance OverflowKeyword "visible"
instance OverflowKeyword "hidden"
instance OverflowKeyword "clip"
instance OverflowKeyword "scroll"
instance OverflowKeyword "auto"

instance declarationOverflowX ::
  ( OverflowKeyword s
  , ToVal (Proxy s)
  ) =>
  Declaration "overflow-x" (Proxy s) where
  pval = const val

-- https://www.w3.org/TR/css-overflow-3/#propdef-overflow-y

overflowY = Proxy :: Proxy "overflow-y"

instance Property "overflow-y"

instance declarationOverflowY ::
  Declaration "overflow-x" a =>
  Declaration "overflow-y" a where
  pval = const $ pval overflowX

-- https://www.w3.org/TR/css-overflow-3/#propdef-overflow

overflow = Proxy :: Proxy "overflow"

instance Property "overflow"

class IsOverflow (a :: Type)

instance
  ( OverflowKeyword sx
  , OverflowKeyword sy
  ) =>
  IsOverflow (Proxy sx ~ Proxy sy)

instance OverflowKeyword s => IsOverflow (Proxy s)

instance declarationOverflow ::
  ( IsOverflow a
  , ToVal a
  ) =>
  Declaration "overflow" a where
  pval = const val

-- https://www.w3.org/TR/css-overflow-3/#propdef-text-overflow

textOverflow = Proxy :: Proxy "text-overflow"

instance Property "text-overflow"

ellipsis = Proxy :: Proxy "ellipsis"

class TextOverflowKeyword (s :: Symbol)

instance TextOverflowKeyword "clip"
instance TextOverflowKeyword "ellipsis"

instance declarationTextOverflow ::
  ( TextOverflowKeyword s
  , ToVal (Proxy s)
  ) =>
  Declaration "text-overflow" (Proxy s) where
  pval = const val

--------------------------------------------------------------------------------

-- Positioned Layout
-- https://www.w3.org/TR/css-position-3/

-- https://www.w3.org/TR/css-position-3/#propdef-position

position = Proxy :: Proxy "position"

instance Property "position"

static = Proxy :: Proxy "static"
relative = Proxy :: Proxy "relative"
absolute = Proxy :: Proxy "absolute"
sticky = Proxy :: Proxy "sticky"
fixed = Proxy :: Proxy "fixed"

class PositionKeyword (s :: Symbol)

instance PositionKeyword "static"
instance PositionKeyword "relative"
instance PositionKeyword "absolute"
instance PositionKeyword "sticky"
instance PositionKeyword "fixed"

instance declarationPosition ::
  ( PositionKeyword s
  , ToVal (Proxy s)
  ) =>
  Declaration "position" (Proxy s) where
  pval = const val

-- https://www.w3.org/TR/css-position-3/#propdef-top

instance Property "top"
instance Animatable "top"

class IsTop (a :: Type)

instance IsTop (Proxy "auto")
instance LengthPercentageTag t => IsTop (Measure t)

instance declarationTop :: (IsTop a, ToVal a) => Declaration "top" a where
  pval = const val

-- https://www.w3.org/TR/css-position-3/#propdef-right

right = Proxy :: Proxy "right"

instance Property "right"
instance Animatable "right"

instance declarationRight :: Declaration "top" a => Declaration "right" a where
  pval = const $ pval top

-- https://www.w3.org/TR/css-position-3/#propdef-bottom

instance Property "bottom"
instance Animatable "bottom"

instance declarationBottom ::
  Declaration "top" a =>
  Declaration "bottom" a where
  pval = const $ pval top

-- https://www.w3.org/TR/css-position-3/#propdef-left

left = Proxy :: Proxy "left"

instance Property "left"
instance Animatable "left"

instance declarationLeft :: Declaration "top" a => Declaration "left" a where
  pval = const $ pval top

-- https://www.w3.org/TR/css-position-3/#propdef-inset-block-start

insetBlockStart = Proxy :: Proxy "inset-block-start"

instance Property "inset-block-start"
instance Animatable "inset-block-start"

instance declarationInsetBlockStart ::
  Declaration "top" a =>
  Declaration "inset-block-start" a where
  pval = const $ pval top

-- https://www.w3.org/TR/css-position-3/#propdef-inset-inline-start

insetInlineStart = Proxy :: Proxy "inset-inline-start"

instance Property "inset-inline-start"
instance Animatable "inset-inline-start"

instance declarationInsetInlineStart ::
  Declaration "top" a =>
  Declaration "inset-inline-start" a where
  pval = const $ pval top

-- https://www.w3.org/TR/css-position-3/#propdef-inset-block-end

insetBlockEnd = Proxy :: Proxy "inset-block-end"

instance Property "inset-block-end"
instance Animatable "inset-block-end"

instance declarationInsetBlockEnd ::
  Declaration "top" a =>
  Declaration "inset-block-end" a where
  pval = const $ pval top

-- https://www.w3.org/TR/css-position-3/#propdef-inset-inline-end

insetInlineEnd = Proxy :: Proxy "inset-inline-end"

instance Property "inset-inline-end"
instance Animatable "inset-inline-end"

instance declarationInsetInlineEnd ::
  Declaration "top" a =>
  Declaration "inset-inline-end" a where
  pval = const $ pval top

-- https://www.w3.org/TR/css-position-3/#propdef-inset-block

insetBlock = Proxy :: Proxy "inset-block"

instance Property "inset-block"
instance Animatable "inset-block"

class IsInsetBlock (a :: Type)

instance (IsTop start, IsTop end) => IsInsetBlock (start ~ end)
else instance IsTop both => IsInsetBlock both

instance declarationInsetBlock ::
  ( IsInsetBlock a
  , ToVal a
  ) =>
  Declaration "inset-block" a where
  pval = const val

-- https://www.w3.org/TR/css-position-3/#propdef-inset-inline

insetInline = Proxy :: Proxy "inset-inline"

instance Property "inset-inline"
instance Animatable "inset-inline"

instance declarationInsetInline ::
  Declaration "inset-block" a =>
  Declaration "inset-inline" a where
  pval = const $ pval insetBlock

-- https://www.w3.org/TR/css-position-3/#propdef-inset

instance Property "inset"
instance Animatable "inset"

class IsInset (a :: Type)

instance (IsTop t, IsTop r, IsTop b, IsTop l) => IsInset (t ~ r ~ b ~ l)
else instance (IsTop t, IsTop x, IsTop b) => IsInset (t ~ x ~ b)
else instance (IsTop y, IsTop x) => IsInset (y ~ x)
else instance IsTop a => IsInset a

instance declarationInset :: (IsInset a, ToVal a) => Declaration "inset" a where
  pval = const val

--------------------------------------------------------------------------------

-- Selectors
-- https://www.w3.org/TR/selectors-4/

data Extensible
data Inextensible

newtype Selector (status :: Type) = Selector Val

derive newtype instance ToVal (Selector status)

class IsSelector (a :: Type)

instance IsSelector (Selector status)
else instance IsExtensibleSelector a => IsSelector a

class IsSelectorList (a :: Type)

instance (IsSelector x, IsSelectorList xs) => IsSelectorList (x /\ xs)
else instance IsSelector x => IsSelectorList x

class IsExtensibleSelector (a :: Type)

instance IsExtensibleSelector (Selector Extensible)

-- https://www.w3.org/TR/selectors-4/#the-universal-selector

universal :: Selector Extensible
universal = Selector $ val "*"

-- https://www.w3.org/TR/selectors-4/#combinators

class Combine (b :: Type) (c :: Type) | b -> c where
  combine
    :: forall a
     . IsExtensibleSelector a
    => ToVal a
    => String
    -> a
    -> b
    -> c

instance Combine (Selector Inextensible) (Selector Inextensible) where
  combine s a' b'
    | s == " " =
        Selector $ val a' <> val " " <> val b'
    | otherwise =
        Selector $
          val a' <> (Val \c -> c.separator <> s <> c.separator) <> val b'

else instance (IsSelector b, ToVal b) => Combine b (Selector Extensible) where
  combine s a' b'
    | s == " " =
        Selector $ val a' <> val " " <> val b'
    | otherwise =
        Selector $
          val a' <> (Val \c -> c.separator <> s <> c.separator) <> val b'

-- https://www.w3.org/TR/selectors-4/#descendant-combinators

descendant
  :: forall a b c
   . IsExtensibleSelector a
  => ToVal a
  => Combine b c
  => a
  -> b
  -> c
descendant = combine " "

infixl 7 descendant as |*

-- https://www.w3.org/TR/selectors-4/#child-combinators

child
  :: forall a b c
   . IsExtensibleSelector a
  => ToVal a
  => Combine b c
  => a
  -> b
  -> c
child = combine ">"

infixl 7 child as |>

-- https://www.w3.org/TR/selectors-4/#adjacent-sibling-combinators

adjacentSibling
  :: forall a b c
   . IsExtensibleSelector a
  => ToVal a
  => Combine b c
  => a
  -> b
  -> c
adjacentSibling = combine "+"

infixl 7 adjacentSibling as |+

-- https://www.w3.org/TR/selectors-4/#general-sibling-combinators

generalSibling
  :: forall a b c
   . IsExtensibleSelector a
  => ToVal a
  => Combine b c
  => a
  -> b
  -> c
generalSibling = combine "~"

infixl 7 generalSibling as |~

-- Element selectors

a = Proxy :: Proxy "a"
abbr = Proxy :: Proxy "abbr"
acronym = Proxy :: Proxy "acronym"
article = Proxy :: Proxy "article"
aside = Proxy :: Proxy "aside"
audio = Proxy :: Proxy "audio"
b = Proxy :: Proxy "b"
blockquote = Proxy :: Proxy "blockquote"
body = Proxy :: Proxy "body"
button = Proxy :: Proxy "button"
canvas = Proxy :: Proxy "canvas"
caption = Proxy :: Proxy "caption"
code = Proxy :: Proxy "code"
col = Proxy :: Proxy "col"
colgroup = Proxy :: Proxy "colgroup"
dd = Proxy :: Proxy "dd"
details = Proxy :: Proxy "details"
div = Proxy :: Proxy "div"
dl = Proxy :: Proxy "dl"
dt = Proxy :: Proxy "dt"
em' = Proxy :: Proxy "em"
fieldset = Proxy :: Proxy "fieldset"
footer = Proxy :: Proxy "footer"
form = Proxy :: Proxy "form"
h1 = Proxy :: Proxy "h1"
h2 = Proxy :: Proxy "h2"
h3 = Proxy :: Proxy "h3"
h4 = Proxy :: Proxy "h4"
h5 = Proxy :: Proxy "h5"
h6 = Proxy :: Proxy "h6"
header = Proxy :: Proxy "header"
hr = Proxy :: Proxy "hr"
html = Proxy :: Proxy "html"
i = Proxy :: Proxy "i"
img = Proxy :: Proxy "img"
input = Proxy :: Proxy "input"
label = Proxy :: Proxy "label"
legend = Proxy :: Proxy "legend"
li = Proxy :: Proxy "li"
line = Proxy :: Proxy "line"
main' = Proxy :: Proxy "main"
mark = Proxy :: Proxy "mark"
menu = Proxy :: Proxy "menu"
nav = Proxy :: Proxy "nav"
ol = Proxy :: Proxy "ol"
optgroup = Proxy :: Proxy "optgroup"
option = Proxy :: Proxy "option"
p = Proxy :: Proxy "p"
path = Proxy :: Proxy "path"
polygon = Proxy :: Proxy "polygon"
polyline = Proxy :: Proxy "polyline"
pre = Proxy :: Proxy "pre"
progress = Proxy :: Proxy "progress"
q = Proxy :: Proxy "q"
rect = Proxy :: Proxy "rect"
section = Proxy :: Proxy "section"
select = Proxy :: Proxy "select"
span = Proxy :: Proxy "span"
strong = Proxy :: Proxy "strong"
summary = Proxy :: Proxy "summary"
sup = Proxy :: Proxy "sup"
svg = Proxy :: Proxy "svg"
table = Proxy :: Proxy "table"
tbody = Proxy :: Proxy "tbody"
td = Proxy :: Proxy "td"
textarea = Proxy :: Proxy "textarea"
tfoot = Proxy :: Proxy "tfoot"
th = Proxy :: Proxy "th"
thead = Proxy :: Proxy "thead"
time = Proxy :: Proxy "time"
tr = Proxy :: Proxy "tr"
ul = Proxy :: Proxy "ul"
video = Proxy :: Proxy "video"

class Element (s :: Symbol)

instance Element "a"
instance Element "abbr"
instance Element "acronym"
instance Element "article"
instance Element "aside"
instance Element "audio"
instance Element "b"
instance Element "blockquote"
instance Element "body"
instance Element "button"
instance Element "canvas"
instance Element "caption"
instance Element "circle"
instance Element "code"
instance Element "col"
instance Element "colgroup"
instance Element "dd"
instance Element "details"
instance Element "div"
instance Element "dl"
instance Element "dt"
instance Element "ellipse"
instance Element "em"
instance Element "fieldset"
instance Element "footer"
instance Element "form"
instance Element "h1"
instance Element "h2"
instance Element "h3"
instance Element "h4"
instance Element "h5"
instance Element "h6"
instance Element "header"
instance Element "hr"
instance Element "html"
instance Element "i"
instance Element "img"
instance Element "input"
instance Element "label"
instance Element "legend"
instance Element "li"
instance Element "line"
instance Element "main"
instance Element "mark"
instance Element "menu"
instance Element "nav"
instance Element "ol"
instance Element "optgroup"
instance Element "option"
instance Element "p"
instance Element "path"
instance Element "polygon"
instance Element "polyline"
instance Element "pre"
instance Element "progress"
instance Element "q"
instance Element "rect"
instance Element "section"
instance Element "select"
instance Element "small"
instance Element "span"
instance Element "strong"
instance Element "sub"
instance Element "summary"
instance Element "sup"
instance Element "svg"
instance Element "table"
instance Element "tbody"
instance Element "td"
instance Element "textarea"
instance Element "tfoot"
instance Element "th"
instance Element "thead"
instance Element "time"
instance Element "tr"
instance Element "ul"
instance Element "video"

instance Element s => IsExtensibleSelector (Proxy s)

-- https://www.w3.org/TR/selectors-3/#attribute-selectors

accept = Proxy :: Proxy "accept"
acceptCharset = Proxy :: Proxy "accept-charset"
accesskey = Proxy :: Proxy "accesskey"
action = Proxy :: Proxy "action"
alt = Proxy :: Proxy "alt"
async = Proxy :: Proxy "async"
autocomplete = Proxy :: Proxy "autocomplete"
autofocus = Proxy :: Proxy "autofocus"
autoplay = Proxy :: Proxy "autoplay"
charset = Proxy :: Proxy "charset"
checked = Proxy :: Proxy "checked"
cite = Proxy :: Proxy "cite"
class' = Proxy :: Proxy "class"
cols = Proxy :: Proxy "cols"
colspan = Proxy :: Proxy "colspan"
contenteditable = Proxy :: Proxy "contenteditable"
controls = Proxy :: Proxy "controls"
coords = Proxy :: Proxy "coords"
data' = Proxy :: Proxy "data"
datetime = Proxy :: Proxy "datetime"
default = Proxy :: Proxy "default"
defer = Proxy :: Proxy "defer"
dir = Proxy :: Proxy "dir"
dirname = Proxy :: Proxy "dirname"
disabled = Proxy :: Proxy "disabled"
download = Proxy :: Proxy "download"
draggable = Proxy :: Proxy "draggable"
enctype = Proxy :: Proxy "enctype"
for = Proxy :: Proxy "for"
formaction = Proxy :: Proxy "formaction"
headers = Proxy :: Proxy "headers"
height = Proxy :: Proxy "height"
high = Proxy :: Proxy "high"
href = Proxy :: Proxy "href"
hreflang = Proxy :: Proxy "hreflang"
httpEquiv = Proxy :: Proxy "http-equiv"
id = Proxy :: Proxy "id"
ismap = Proxy :: Proxy "ismap"
kind = Proxy :: Proxy "kind"
lang' = Proxy :: Proxy "lang"
list = Proxy :: Proxy "list"
loop = Proxy :: Proxy "loop"
low = Proxy :: Proxy "low"
max = Proxy :: Proxy "max"
maxlength = Proxy :: Proxy "maxlength"
media' = Proxy :: Proxy "media"
method = Proxy :: Proxy "method"
min = Proxy :: Proxy "min"
multiple = Proxy :: Proxy "multiple"
muted = Proxy :: Proxy "muted"
name = Proxy :: Proxy "name"
novalidate = Proxy :: Proxy "novalidate"
onabort = Proxy :: Proxy "onabort"
onafterprint = Proxy :: Proxy "onafterprint"
onbeforeprint = Proxy :: Proxy "onbeforeprint"
onbeforeunload = Proxy :: Proxy "onbeforeunload"
onblur = Proxy :: Proxy "onblur"
oncanplay = Proxy :: Proxy "oncanplay"
oncanplaythrough = Proxy :: Proxy "oncanplaythrough"
onchange = Proxy :: Proxy "onchange"
onclick = Proxy :: Proxy "onclick"
oncontextmenu = Proxy :: Proxy "oncontextmenu"
oncopy = Proxy :: Proxy "oncopy"
oncuechange = Proxy :: Proxy "oncuechange"
oncut = Proxy :: Proxy "oncut"
ondblclick = Proxy :: Proxy "ondblclick"
ondrag = Proxy :: Proxy "ondrag"
ondragend = Proxy :: Proxy "ondragend"
ondragenter = Proxy :: Proxy "ondragenter"
ondragleave = Proxy :: Proxy "ondragleave"
ondragover = Proxy :: Proxy "ondragover"
ondragstart = Proxy :: Proxy "ondragstart"
ondrop = Proxy :: Proxy "ondrop"
ondurationchange = Proxy :: Proxy "ondurationchange"
onemptied = Proxy :: Proxy "onemptied"
onended = Proxy :: Proxy "onended"
onerror = Proxy :: Proxy "onerror"
onfocus = Proxy :: Proxy "onfocus"
onhashchange = Proxy :: Proxy "onhashchange"
oninput = Proxy :: Proxy "oninput"
oninvalid = Proxy :: Proxy "oninvalid"
onkeydown = Proxy :: Proxy "onkeydown"
onkeypress = Proxy :: Proxy "onkeypress"
onkeyup = Proxy :: Proxy "onkeyup"
onload = Proxy :: Proxy "onload"
onloadeddata = Proxy :: Proxy "onloadeddata"
onloadedmetadata = Proxy :: Proxy "onloadedmetadata"
onloadstart = Proxy :: Proxy "onloadstart"
onmousedown = Proxy :: Proxy "onmousedown"
onmousemove = Proxy :: Proxy "onmousemove"
onmouseout = Proxy :: Proxy "onmouseout"
onmouseover = Proxy :: Proxy "onmouseover"
onmouseup = Proxy :: Proxy "onmouseup"
onmousewheel = Proxy :: Proxy "onmousewheel"
onoffline = Proxy :: Proxy "onoffline"
ononline = Proxy :: Proxy "ononline"
onpagehide = Proxy :: Proxy "onpagehide"
onpageshow = Proxy :: Proxy "onpageshow"
onpaste = Proxy :: Proxy "onpaste"
onpause = Proxy :: Proxy "onpause"
onplay = Proxy :: Proxy "onplay"
onplaying = Proxy :: Proxy "onplaying"
onpopstate = Proxy :: Proxy "onpopstate"
onprogress = Proxy :: Proxy "onprogress"
onratechange = Proxy :: Proxy "onratechange"
onreset = Proxy :: Proxy "onreset"
onresize = Proxy :: Proxy "onresize"
onscroll = Proxy :: Proxy "onscroll"
onsearch = Proxy :: Proxy "onsearch"
onseeked = Proxy :: Proxy "onseeked"
onseeking = Proxy :: Proxy "onseeking"
onselect = Proxy :: Proxy "onselect"
onstalled = Proxy :: Proxy "onstalled"
onstorage = Proxy :: Proxy "onstorage"
onsubmit = Proxy :: Proxy "onsubmit"
onsuspend = Proxy :: Proxy "onsuspend"
ontimeupdate = Proxy :: Proxy "ontimeupdate"
ontoggle = Proxy :: Proxy "ontoggle"
onunload = Proxy :: Proxy "onunload"
onvolumechange = Proxy :: Proxy "onvolumechange"
onwaiting = Proxy :: Proxy "onwaiting"
onwheel = Proxy :: Proxy "onwheel"
open = Proxy :: Proxy "open"
optimum = Proxy :: Proxy "optimum"
pattern = Proxy :: Proxy "pattern"
placeholder = Proxy :: Proxy "placeholder"
poster = Proxy :: Proxy "poster"
preload = Proxy :: Proxy "preload"
readonly = Proxy :: Proxy "readonly"
rel = Proxy :: Proxy "rel"
required = Proxy :: Proxy "required"
reversed = Proxy :: Proxy "reversed"
role = Proxy :: Proxy "role"
rows = Proxy :: Proxy "rows"
rowspan = Proxy :: Proxy "rowspan"
sandbox = Proxy :: Proxy "sandbox"
scope = Proxy :: Proxy "scope"
selected = Proxy :: Proxy "selected"
shape = Proxy :: Proxy "shape"
size = Proxy :: Proxy "size"
sizes = Proxy :: Proxy "sizes"
spellcheck = Proxy :: Proxy "spellcheck"
srcdoc = Proxy :: Proxy "srcdoc"
srclang = Proxy :: Proxy "srclang"
srcset = Proxy :: Proxy "srcset"
start = Proxy :: Proxy "start"
step = Proxy :: Proxy "step"
style = Proxy :: Proxy "style"
tabindex = Proxy :: Proxy "tabindex"
target = Proxy :: Proxy "target"
title = Proxy :: Proxy "title"
translate' = Proxy :: Proxy "translate"
type' = Proxy :: Proxy "type"
usemap = Proxy :: Proxy "usemap"
value = Proxy :: Proxy "value"
width = Proxy :: Proxy "width"
wrap = Proxy :: Proxy "wrap"

class AttrName (s :: Symbol)

instance AttrName "accept"
instance AttrName "accept-charset"
instance AttrName "accesskey"
instance AttrName "action"
instance AttrName "alt"
instance AttrName "async"
instance AttrName "autocomplete"
instance AttrName "autofocus"
instance AttrName "autoplay"
instance AttrName "charset"
instance AttrName "checked"
instance AttrName "cite"
instance AttrName "class"
instance AttrName "cols"
instance AttrName "colspan"
instance AttrName "content"
instance AttrName "contenteditable"
instance AttrName "controls"
instance AttrName "coords"
instance AttrName "data"
instance AttrName "datetime"
instance AttrName "default"
instance AttrName "defer"
instance AttrName "dir"
instance AttrName "dirname"
instance AttrName "disabled"
instance AttrName "download"
instance AttrName "draggable"
instance AttrName "enctype"
instance AttrName "for"
instance AttrName "form"
instance AttrName "formaction"
instance AttrName "headers"
instance AttrName "height"
instance AttrName "hidden"
instance AttrName "high"
instance AttrName "href"
instance AttrName "hreflang"
instance AttrName "http-equiv"
instance AttrName "id"
instance AttrName "ismap"
instance AttrName "kind"
instance AttrName "label"
instance AttrName "lang"
instance AttrName "list"
instance AttrName "loop"
instance AttrName "low"
instance AttrName "max"
instance AttrName "maxlength"
instance AttrName "media"
instance AttrName "method"
instance AttrName "min"
instance AttrName "multiple"
instance AttrName "muted"
instance AttrName "name"
instance AttrName "novalidate"
instance AttrName "onabort"
instance AttrName "onafterprint"
instance AttrName "onbeforeprint"
instance AttrName "onbeforeunload"
instance AttrName "onblur"
instance AttrName "oncanplay"
instance AttrName "oncanplaythrough"
instance AttrName "onchange"
instance AttrName "onclick"
instance AttrName "oncontextmenu"
instance AttrName "oncopy"
instance AttrName "oncuechange"
instance AttrName "oncut"
instance AttrName "ondblclick"
instance AttrName "ondrag"
instance AttrName "ondragend"
instance AttrName "ondragenter"
instance AttrName "ondragleave"
instance AttrName "ondragover"
instance AttrName "ondragstart"
instance AttrName "ondrop"
instance AttrName "ondurationchange"
instance AttrName "onemptied"
instance AttrName "onended"
instance AttrName "onerror"
instance AttrName "onfocus"
instance AttrName "onhashchange"
instance AttrName "oninput"
instance AttrName "oninvalid"
instance AttrName "onkeydown"
instance AttrName "onkeypress"
instance AttrName "onkeyup"
instance AttrName "onload"
instance AttrName "onloadeddata"
instance AttrName "onloadedmetadata"
instance AttrName "onloadstart"
instance AttrName "onmousedown"
instance AttrName "onmousemove"
instance AttrName "onmouseout"
instance AttrName "onmouseover"
instance AttrName "onmouseup"
instance AttrName "onmousewheel"
instance AttrName "onoffline"
instance AttrName "ononline"
instance AttrName "onpagehide"
instance AttrName "onpageshow"
instance AttrName "onpaste"
instance AttrName "onpause"
instance AttrName "onplay"
instance AttrName "onplaying"
instance AttrName "onpopstate"
instance AttrName "onprogress"
instance AttrName "onratechange"
instance AttrName "onreset"
instance AttrName "onresize"
instance AttrName "onscroll"
instance AttrName "onsearch"
instance AttrName "onseeked"
instance AttrName "onseeking"
instance AttrName "onselect"
instance AttrName "onstalled"
instance AttrName "onstorage"
instance AttrName "onsubmit"
instance AttrName "onsuspend"
instance AttrName "ontimeupdate"
instance AttrName "ontoggle"
instance AttrName "onunload"
instance AttrName "onvolumechange"
instance AttrName "onwaiting"
instance AttrName "onwheel"
instance AttrName "open"
instance AttrName "optimum"
instance AttrName "pattern"
instance AttrName "placeholder"
instance AttrName "poster"
instance AttrName "preload"
instance AttrName "readonly"
instance AttrName "rel"
instance AttrName "required"
instance AttrName "reversed"
instance AttrName "role"
instance AttrName "rows"
instance AttrName "rowspan"
instance AttrName "sandbox"
instance AttrName "scope"
instance AttrName "selected"
instance AttrName "shape"
instance AttrName "size"
instance AttrName "sizes"
instance AttrName "span"
instance AttrName "spellcheck"
instance AttrName "src"
instance AttrName "srcdoc"
instance AttrName "srclang"
instance AttrName "srcset"
instance AttrName "start"
instance AttrName "step"
instance AttrName "style"
instance AttrName "tabindex"
instance AttrName "target"
instance AttrName "time"
instance AttrName "title"
instance AttrName "translate"
instance AttrName "type"
instance AttrName "usemap"
instance AttrName "value"
instance AttrName "width"
instance AttrName "wrap"

class IsAttrName (a :: Type)

instance AttrName s => IsAttrName (Proxy s)
instance IsAttrName AttrName

instance ToVal AttrName where
  val (AttrName x) = val x

data AttributePredicate = AttributePredicate Val String String

instance ToVal AttributePredicate where
  val (AttributePredicate k op v) = k <> val op <> val (quote v)

attEq :: forall a. IsAttrName a => ToVal a => a -> String -> AttributePredicate
attEq k v = AttributePredicate (val k) "=" v

infixr 8 attEq as @=

attElemWhitespace
  :: forall a
   . IsAttrName a
  => ToVal a
  => a
  -> String
  -> AttributePredicate
attElemWhitespace k v = AttributePredicate (val k) "~=" v

infixr 8 attElemWhitespace as ~=

attStartsWithHyphen
  :: forall a
   . IsAttrName a
  => ToVal a
  => a
  -> String
  -> AttributePredicate
attStartsWithHyphen k v = AttributePredicate (val k) "|=" v

infixr 8 attStartsWithHyphen as |=

attStartsWith
  :: forall a
   . IsAttrName a
  => ToVal a
  => a
  -> String
  -> AttributePredicate
attStartsWith k v = AttributePredicate (val k) "^=" v

infixr 8 attStartsWith as ^=

attEndsWith
  :: forall a
   . IsAttrName a
  => ToVal a
  => a
  -> String
  -> AttributePredicate
attEndsWith k v = AttributePredicate (val k) "$=" v

infixr 8 attEndsWith as $=

attContains
  :: forall a
   . IsAttrName a
  => ToVal a
  => a
  -> String
  -> AttributePredicate
attContains k v = AttributePredicate (val k) "*=" v

infixr 8 attContains as *=

class ToVal a <= ByAtt (a :: Type)

instance ByAtt AttributePredicate
instance ByAtt AttrName
instance (IsSymbol s, AttrName s) => ByAtt (Proxy s)

byAtt
  :: forall selector att
   . IsExtensibleSelector selector
  => ToVal selector
  => ByAtt att
  => selector
  -> att
  -> Selector Extensible
byAtt s a' = Selector $ val s <> val "[" <> val a' <> val "]"

infixl 7 byAtt as &@

-- https://www.w3.org/TR/selectors-3/#class-html

byClass
  :: forall selector
   . IsExtensibleSelector selector
  => ToVal selector
  => selector
  -> ClassName
  -> Selector Extensible
byClass s (ClassName c) = Selector $ val s <> val "." <> val c

infixl 7 byClass as &.

-- https://www.w3.org/TR/selectors-3/#id-selectors

newtype ElementId = ElementId String

derive newtype instance ToVal ElementId

byId
  :: forall selector
   . IsExtensibleSelector selector
  => ToVal selector
  => selector
  -> ElementId
  -> Selector Extensible
byId s i' = Selector $ val s <> val "#" <> val i'

infixl 7 byId as &#

data PseudoClass = PseudoClass String | PseudoClassVal Val

instance ToVal PseudoClass where
  val (PseudoClass x) = val x
  val (PseudoClassVal x) = x

class IsPseudoClass (a :: Type)

instance IsPseudoClass PseudoClass
instance IsPseudoClass (Proxy "checked")
instance IsPseudoClass (Proxy "disabled")
instance IsPseudoClass (Proxy "target")

byPseudoClass
  :: forall pseudo selector
   . IsPseudoClass pseudo
  => IsExtensibleSelector selector
  => ToVal selector
  => ToVal pseudo
  => selector
  -> pseudo
  -> Selector Extensible
byPseudoClass s p' = Selector $ val s <> val ":" <> val p'

infixl 7 byPseudoClass as &:

newtype PseudoElement = PseudoElement String

derive newtype instance ToVal PseudoElement

class IsPseudoElement (a :: Type)

instance IsPseudoElement PseudoElement
instance IsPseudoElement (Proxy "placeholder")

byPseudoElement
  :: forall pseudo selector
   . IsPseudoElement pseudo
  => IsExtensibleSelector selector
  => ToVal selector
  => ToVal pseudo
  => selector
  -> pseudo
  -> Selector Inextensible
byPseudoElement s p' = Selector $ val s <> val "::" <> val p'

infixl 7 byPseudoElement as &::

-- https://www.w3.org/TR/selectors-3/#sel-link

link :: PseudoClass
link = PseudoClass "link"

-- https://www.w3.org/TR/selectors-3/#sel-visited

visited :: PseudoClass
visited = PseudoClass "visited"

-- https://www.w3.org/TR/selectors-3/#sel-hover

hover :: PseudoClass
hover = PseudoClass "hover"

-- https://www.w3.org/TR/selectors-3/#sel-active

active :: PseudoClass
active = PseudoClass "active"

-- https://www.w3.org/TR/selectors-3/#sel-focus

focus :: PseudoClass
focus = PseudoClass "focus"

-- https://www.w3.org/TR/selectors-3/#lang-pseudo

lang :: String -> PseudoClass
lang c = PseudoClassVal $ fn "lang" c

-- https://www.w3.org/TR/selectors-3/#sel-enabled

enabled :: PseudoClass
enabled = PseudoClass "enabled"

-- https://www.w3.org/TR/selectors-3/#sel-indeterminate

indeterminate :: PseudoClass
indeterminate = PseudoClass "indeterminate"

-- https://www.w3.org/TR/selectors-3/#sel-root

root :: PseudoClass
root = PseudoClass "root"

-- https://www.w3.org/TR/selectors-3/#sel-nth-child

data AnPlusB = AnPlusB Int Int

instance ToVal AnPlusB where
  val (AnPlusB a' b') =
    Val \{ separator } ->
      let
        an
          | a' == -1 = "-n"
          | a' == 1 = "n"
          | a' /= 0 = show a' <> "n"
          | otherwise = mempty
        op
          | a' == 0 || b' == 0 = mempty
          | b' < 0 = separator <> "-" <> separator
          | otherwise = separator <> "+" <> separator
        b''
          | b' == 0 = mempty
          | a' == 0 = show b'
          | otherwise = show $ abs b'
      in
        an <> op <> b''

even :: AnPlusB
even = AnPlusB 2 0

odd :: AnPlusB
odd = AnPlusB 2 1

infixl 9 AnPlusB as #+

anminusb :: Int -> Int -> AnPlusB
anminusb a' b' = AnPlusB a' (-b')

infixl 9 anminusb as #-

nthChild :: AnPlusB -> PseudoClass
nthChild formula = PseudoClassVal $ fn "nth-child" formula

-- https://www.w3.org/TR/selectors-3/#sel-nth-last-child

nthLastChild :: AnPlusB -> PseudoClass
nthLastChild formula = PseudoClassVal $ fn "nth-last-child" formula

-- https://www.w3.org/TR/selectors-3/#sel-nth-of-type

nthOfType :: AnPlusB -> PseudoClass
nthOfType formula = PseudoClassVal $ fn "nth-of-type" formula

-- https://www.w3.org/TR/selectors-3/#sel-first-child

firstChild :: PseudoClass
firstChild = PseudoClass "first-child"

-- https://www.w3.org/TR/selectors-3/#sel-last-child

lastChild :: PseudoClass
lastChild = PseudoClass "last-child"

-- https://www.w3.org/TR/selectors-3/#sel-first-of-type

firstOfType :: PseudoClass
firstOfType = PseudoClass "first-of-type"

-- https://www.w3.org/TR/selectors-3/#sel-last-of-type

lastOfType :: PseudoClass
lastOfType = PseudoClass "last-of-type"

-- https://www.w3.org/TR/selectors-3/#sel-only-child

onlyChild :: PseudoClass
onlyChild = PseudoClass "only-child"

-- https://www.w3.org/TR/selectors-3/#sel-only-of-type

onlyOfType :: PseudoClass
onlyOfType = PseudoClass "only-of-type"

-- https://www.w3.org/TR/selectors-3/#sel-empty

empty :: PseudoClass
empty = PseudoClass "empty"

-- https://www.w3.org/TR/selectors-4/#negation-pseudo

not :: forall s. IsSelectorList s => MultiVal s => s -> PseudoClass
not s = PseudoClassVal $ fn "not" s

-- https://www.w3.org/TR/selectors-4/#focus-within-pseudo

focusWithin :: PseudoClass
focusWithin = PseudoClass "focus-within"

-- https://www.w3.org/TR/selectors-3/#sel-first-line

firstLine :: PseudoElement
firstLine = PseudoElement "first-line"

-- https://www.w3.org/TR/selectors-3/#sel-first-letter

firstLetter :: PseudoElement
firstLetter = PseudoElement "first-letter"

-- https://www.w3.org/TR/selectors-3/#sel-before

before :: PseudoElement
before = PseudoElement "before"

-- https://www.w3.org/TR/selectors-3/#sel-after

after :: PseudoElement
after = PseudoElement "after"

-- https://www.w3.org/TR/css-lists-3/#marker-pseudo

marker :: PseudoElement
marker = PseudoElement "marker"

-- https://www.w3.org/TR/css-pseudo-4/#selectordef-selection

selection :: PseudoElement
selection = PseudoElement "selection"

--------------------------------------------------------------------------------

-- Box Sizing
-- https://www.w3.org/TR/css-sizing-3/

-- https://www.w3.org/TR/css-sizing-3/#propdef-width

instance Property "width"
instance Animatable "width"

newtype FitContent = FitContent Val

derive newtype instance ToVal FitContent

fitContent :: forall t. LengthPercentageTag t => Measure t -> FitContent
fitContent v = FitContent $ fn "fit-content" v

minContent = Proxy :: Proxy "min-content"
maxContent = Proxy :: Proxy "max-content"

class WidthKeyword (s :: Symbol)

instance WidthKeyword "auto"
instance WidthKeyword "min-content"
instance WidthKeyword "max-content"

class IsWidth (a :: Type)

instance WidthKeyword s => IsWidth (Proxy s)
instance IsWidth FitContent
instance LengthPercentageTag t => IsWidth (Measure t)

instance declarationWidth :: (IsWidth a, ToVal a) => Declaration "width" a where
  pval = const val

-- https://www.w3.org/TR/css-sizing-3/#propdef-height

instance Property "height"
instance Animatable "height"

instance declarationHeight ::
  Declaration "width" a =>
  Declaration "height" a where
  pval = const $ pval width

-- https://www.w3.org/TR/css-sizing-3/#propdef-min-width

minWidth = Proxy :: Proxy "min-width"

instance Property "min-width"
instance Animatable "min-width"

class MinWidthKeyword (s :: Symbol)

instance MinWidthKeyword "auto"
instance MinWidthKeyword "min-content"
instance MinWidthKeyword "max-content"

class IsMinWidth (a :: Type)

instance MinWidthKeyword s => IsMinWidth (Proxy s)
instance LengthPercentageTag t => IsMinWidth (Measure t)
instance IsMinWidth FitContent

instance declarationMinWidth ::
  ( IsMinWidth a
  , ToVal a
  ) =>
  Declaration "min-width" a where
  pval = const val

-- https://www.w3.org/TR/css-sizing-3/#propdef-min-height

minHeight = Proxy :: Proxy "min-height"

instance Property "min-height"
instance Animatable "min-height"

instance declarationMinHeight ::
  Declaration "min-width" a =>
  Declaration "min-height" a where
  pval = const $ pval minWidth

-- https://www.w3.org/TR/css-sizing-3/#propdef-max-width

maxWidth = Proxy :: Proxy "max-width"

instance Property "max-width"
instance Animatable "max-width"

class MaxWidthKeyword (s :: Symbol)

instance MaxWidthKeyword "none"
instance MaxWidthKeyword "min-content"
instance MaxWidthKeyword "max-content"

class IsMaxWidth (a :: Type)

instance MaxWidthKeyword s => IsMaxWidth (Proxy s)
instance LengthPercentageTag t => IsMaxWidth (Measure t)
instance IsMaxWidth FitContent

instance declarationMaxWidth ::
  ( IsMaxWidth a
  , ToVal a
  ) =>
  Declaration "max-width" a where
  pval = const val

-- https://www.w3.org/TR/css-sizing-3/#propdef-max-height

maxHeight = Proxy :: Proxy "max-height"

instance Property "max-height"
instance Animatable "max-height"

instance declarationMaxHeight ::
  Declaration "max-width" a =>
  Declaration "max-height" a where
  pval = const $ pval maxWidth

-- https://www.w3.org/TR/css-sizing-3/#propdef-box-sizing

boxSizing = Proxy :: Proxy "box-sizing"

instance Property "box-sizing"

class BoxSizingKeyword (s :: Symbol)

instance BoxSizingKeyword "content-box"
instance BoxSizingKeyword "border-box"

instance
  ( BoxSizingKeyword s
  , ToVal (Proxy s)
  ) =>
  Declaration "box-sizing" (Proxy s) where
  pval = const val

--------------------------------------------------------------------------------

-- Text
-- https://www.w3.org/TR/css-text-3/

-- https://www.w3.org/TR/css-text-3/#propdef-text-transform

textTransform = Proxy :: Proxy "text-transform"

instance Property "text-transform"

capitalize = Proxy :: Proxy "capitalize"
uppercase = Proxy :: Proxy "uppercase"
lowercase = Proxy :: Proxy "lowercase"

class TextTransformCapitalizationKeyword (s :: Symbol)

instance TextTransformCapitalizationKeyword "capitalize"
instance TextTransformCapitalizationKeyword "uppercase"
instance TextTransformCapitalizationKeyword "lowercase"

fullWidth = Proxy :: Proxy "full-width"
fullSizeKana = Proxy :: Proxy "full-size-kana"

class IsTextTransform (a :: Type)

instance IsTextTransform (Proxy "none")
else instance IsTextTransform (Proxy "full-width")
else instance IsTextTransform (Proxy "full-size-kana")
else instance TextTransformCapitalizationKeyword s => IsTextTransform (Proxy s)

instance
  TextTransformCapitalizationKeyword s =>
  IsTextTransform (Proxy s ~ Proxy "full-width")

instance IsTextTransform (Proxy "full-width" ~ Proxy "full-size-kana")
else instance
  TextTransformCapitalizationKeyword s =>
  IsTextTransform (Proxy s ~ Proxy "full-size-kana")

instance
  TextTransformCapitalizationKeyword s =>
  IsTextTransform (Proxy s ~ Proxy "full-width" ~ Proxy "full-size-kana")

instance declarationTextTransform ::
  ( IsTextTransform a
  , ToVal a
  ) =>
  Declaration "text-transform" a where
  pval = const val

-- https://www.w3.org/TR/css-text-3/#propdef-white-space

whiteSpace = Proxy :: Proxy "white-space"

instance Property "white-space"

preWrap = Proxy :: Proxy "pre-wrap"
breakSpaces = Proxy :: Proxy "break-spaces"
preLine = Proxy :: Proxy "pre-line"

class WhiteSpaceKeyword (s :: Symbol)

instance WhiteSpaceKeyword "normal"
instance WhiteSpaceKeyword "pre"
instance WhiteSpaceKeyword "nowrap"
instance WhiteSpaceKeyword "pre-wrap"
instance WhiteSpaceKeyword "break-spaces"
instance WhiteSpaceKeyword "pre-line"

instance declarationWhiteSpace ::
  ( WhiteSpaceKeyword s
  , ToVal (Proxy s)
  ) =>
  Declaration "white-space" (Proxy s) where
  pval = const val

-- https://www.w3.org/TR/css-text-3/#propdef-word-break

wordBreak = Proxy :: Proxy "word-break"

instance Property "word-break"

keepAll = Proxy :: Proxy "keep-all"

breakAll = Proxy :: Proxy "break-all"

breakWord = Proxy :: Proxy "break-word"

class WordBreakKeyword (s :: Symbol)

instance WordBreakKeyword "normal"
instance WordBreakKeyword "keep-all"
instance WordBreakKeyword "break-all"
instance WordBreakKeyword "break-word"

instance declarationWordBreak ::
  ( WordBreakKeyword s
  , ToVal (Proxy s)
  ) =>
  Declaration "word-break" (Proxy s) where
  pval = const val

-- https://www.w3.org/TR/css-text-3/#propdef-text-align

textAlign = Proxy :: Proxy "text-align"

instance Property "text-align"

end = Proxy :: Proxy "end"
justify = Proxy :: Proxy "justify"
matchParent = Proxy :: Proxy "match-parent"
justifyAll = Proxy :: Proxy "justify-all"

class TextAlignKeyword (s :: Symbol)

instance TextAlignKeyword "start"
instance TextAlignKeyword "end"
instance TextAlignKeyword "left"
instance TextAlignKeyword "right"
instance TextAlignKeyword "center"
instance TextAlignKeyword "justify"
instance TextAlignKeyword "match-parent"
instance TextAlignKeyword "justify-all"

instance declarationTextAlign ::
  ( TextAlignKeyword s
  , ToVal (Proxy s)
  ) =>
  Declaration "text-align" (Proxy s) where
  pval = const val

-- https://www.w3.org/TR/css-text-3/#propdef-word-spacing

wordSpacing = Proxy :: Proxy "word-spacing"

instance Property "word-spacing"
instance Animatable "word-spacing"

class IsWordSpacing (a :: Type)

instance IsWordSpacing (Proxy "normal")
instance LengthTag t => IsWordSpacing (Measure t)

instance declarationWordSpacing ::
  ( IsWordSpacing a
  , ToVal a
  ) =>
  Declaration "word-spacing" a where
  pval = const val

-- https://www.w3.org/TR/css-text-3/#propdef-letter-spacing

letterSpacing = Proxy :: Proxy "letter-spacing"

instance Property "letter-spacing"
instance Animatable "letter-spacing"

class IsLetterSpacing (a :: Type)

instance IsLetterSpacing (Proxy "normal")
instance LengthTag t => IsLetterSpacing (Measure t)

instance declarationLetterSpacing ::
  ( IsLetterSpacing a
  , ToVal a
  ) =>
  Declaration "letter-spacing" a where
  pval = const val

-- https://www.w3.org/TR/css-text-3/#propdef-text-indent

textIndent = Proxy :: Proxy "text-indent"

instance Property "text-indent"
instance Animatable "text-indent"

instance declarationTextIndent ::
  LengthPercentageTag t =>
  Declaration "text-indent" (Measure t) where
  pval = const val

--------------------------------------------------------------------------------

-- Text Decoration
-- https://www.w3.org/TR/css-text-decor-3/

-- https://www.w3.org/TR/css-text-decor-3/#propdef-text-decoration-line

textDecorationLine = Proxy :: Proxy "text-decoration-line"

instance Property "text-decoration-line"

underline = Proxy :: Proxy "underline"
overline = Proxy :: Proxy "overline"
lineThrough = Proxy :: Proxy "line-through"
blink = Proxy :: Proxy "blink"

class IsTextDecorationLine (a :: Type)

instance IsTextDecorationLine (Proxy "none")
instance
  IsTextDecorationLine ( Proxy "underline" ~ Proxy "overline"
        ~ Proxy "line-through"
        ~ Proxy "blink"
    )

instance
  IsTextDecorationLine (Proxy "overline" ~ Proxy "line-through" ~ Proxy "blink")

instance
  IsTextDecorationLine ( Proxy "underline" ~ Proxy "line-through" ~ Proxy
        "blink"
    )

instance
  IsTextDecorationLine (Proxy "underline" ~ Proxy "overline" ~ Proxy "blink")

instance
  IsTextDecorationLine ( Proxy "underline" ~ Proxy "overline" ~ Proxy
        "line-through"
    )

instance IsTextDecorationLine (Proxy "underline" ~ Proxy "overline")
instance IsTextDecorationLine (Proxy "underline" ~ Proxy "line-through")
instance IsTextDecorationLine (Proxy "underline" ~ Proxy "blink")
instance IsTextDecorationLine (Proxy "overline" ~ Proxy "line-through")
instance IsTextDecorationLine (Proxy "overline" ~ Proxy "blink")
instance IsTextDecorationLine (Proxy "line-through" ~ Proxy "blink")
instance IsTextDecorationLine (Proxy "underline")
instance IsTextDecorationLine (Proxy "overline")
instance IsTextDecorationLine (Proxy "line-through")
instance IsTextDecorationLine (Proxy "blink")

instance declarationTextDecorationLine ::
  ( IsTextDecorationLine a
  , ToVal a
  ) =>
  Declaration "text-decoration-line" a where
  pval = const val

-- https://www.w3.org/TR/css-text-decor-3/#propdef-text-decoration-style

textDecorationStyle = Proxy :: Proxy "text-decoration-style"

instance Property "text-decoration-style"

wavy = Proxy :: Proxy "wavy"

class TextDecorationStyleKeyword (s :: Symbol)

instance TextDecorationStyleKeyword "solid"
instance TextDecorationStyleKeyword "double"
instance TextDecorationStyleKeyword "dotted"
instance TextDecorationStyleKeyword "dashed"
instance TextDecorationStyleKeyword "wavy"

instance declarationTextDecorationStyle ::
  ( TextDecorationStyleKeyword s
  , ToVal (Proxy s)
  ) =>
  Declaration "text-decoration-style" (Proxy s) where
  pval = const val

-- https://www.w3.org/TR/css-text-decor-3/#propdef-text-decoration-color

textDecorationColor = Proxy :: Proxy "text-decoration-color"

instance Property "text-decoration-color"
instance Animatable "text-decoration-color"

instance declarationTextDecorationColor ::
  ( IsColor a
  , ToVal a
  ) =>
  Declaration "text-decoration-color" a where
  pval = const val

-- https://www.w3.org/TR/css-text-decor-3/#propdef-text-shadow

textShadow = Proxy :: Proxy "text-shadow"

instance Property "text-shadow"
instance Animatable "text-shadow"

class IsTextShadow (a :: Type)

instance
  ( IsColor color
  , LengthTag txo
  , LengthTag tyo
  , LengthTag tblur
  ) =>
  IsTextShadow (color ~ Measure txo ~ Measure tyo ~ Measure tblur)

instance
  ( LengthTag txo
  , LengthTag tyo
  , LengthTag tblur
  ) =>
  IsTextShadow (Measure txo ~ Measure tyo ~ Measure tblur)

else instance
  ( IsColor color
  , LengthTag txo
  , LengthTag tyo
  ) =>
  IsTextShadow (color ~ Measure txo ~ Measure tyo)

instance
  ( LengthTag txo
  , LengthTag tyo
  ) =>
  IsTextShadow (Measure txo ~ Measure tyo)

class IsTextShadowList (a :: Type)

instance (IsTextShadow x, IsTextShadowList xs) => IsTextShadowList (x /\ xs)
else instance IsTextShadow a => IsTextShadowList a

instance declarationTextShadowNone :: Declaration "text-shadow" (Proxy "none") where
  pval = const val

else instance declarationTextShadowList ::
  ( IsTextShadowList a
  , MultiVal a
  ) =>
  Declaration "text-shadow" a where
  pval = const $ intercalateMultiVal $ val "," <> Val _.separator

--------------------------------------------------------------------------------

-- Transforms
-- https://www.w3.org/TR/css-transforms-1/
-- https://www.w3.org/TR/css-transforms-2/

newtype TransformFunction = TransformFunction Val

derive newtype instance ToVal TransformFunction

-- https://www.w3.org/TR/css-transforms-1/#funcdef-transform-matrix

matrix
  :: forall a b c d e f
   . ToNumber a
  => ToNumber b
  => ToNumber c
  => ToNumber d
  => ToNumber e
  => ToNumber f
  => a
  -> b
  -> c
  -> d
  -> e
  -> f
  -> TransformFunction
matrix a' b' c d e f =
  TransformFunction
    $ fn "matrix"
    $ number a' /\ number b' /\ number c /\ number d /\ number e /\ number f

-- https://www.w3.org/TR/css-transforms-1/#funcdef-transform-translate

translate
  :: forall tx ty
   . LengthPercentageTag tx
  => LengthPercentageTag ty
  => Measure tx
  -> Measure ty
  -> TransformFunction
translate tx ty = TransformFunction $ fn "translate" $ tx /\ ty

-- https://www.w3.org/TR/css-transforms-1/#funcdef-transform-translatex

translateX :: forall t. LengthPercentageTag t => Measure t -> TransformFunction
translateX tx = TransformFunction $ fn "translateX" tx

-- https://www.w3.org/TR/css-transforms-1/#funcdef-transform-translatey

translateY :: forall t. LengthPercentageTag t => Measure t -> TransformFunction
translateY ty = TransformFunction $ fn "translateY" ty

-- https://www.w3.org/TR/css-transforms-1/#funcdef-transform-scale

scale :: Number -> Number -> TransformFunction
scale sx sy = TransformFunction $ fn "scale" $ sx /\ sy

-- https://www.w3.org/TR/css-transforms-1/#funcdef-transform-scalex

scaleX :: Number -> TransformFunction
scaleX sx = TransformFunction $ fn "scaleX" sx

-- https://www.w3.org/TR/css-transforms-1/#funcdef-transform-scaley

scaleY :: Number -> TransformFunction
scaleY sy = TransformFunction $ fn "scaleY" sy

-- https://www.w3.org/TR/css-transforms-1/#funcdef-transform-rotate

rotate :: forall t. AngleTag t => Measure t -> TransformFunction
rotate angle = TransformFunction $ fn "rotate" angle

-- https://www.w3.org/TR/css-transforms-1/#funcdef-transform-skew

-- `skew` has been omitted because, per spec, it "should not be used" anymore.

-- https://www.w3.org/TR/css-transforms-1/#funcdef-transform-skewx

skewX :: forall t. AngleTag t => Measure t -> TransformFunction
skewX angle = TransformFunction $ fn "skewX" angle

-- https://www.w3.org/TR/css-transforms-1/#funcdef-transform-skewy

skewY :: forall t. AngleTag t => Measure t -> TransformFunction
skewY angle = TransformFunction $ fn "skewY" angle

-- https://www.w3.org/TR/css-transforms-2/#funcdef-matrix3d

matrix3d
  :: forall a1 b1 c1 d1 a2 b2 c2 d2 a3 b3 c3 d3 a4 b4 c4 d4
   . ToNumber a1
  => ToNumber b1
  => ToNumber c1
  => ToNumber d1
  => ToNumber a2
  => ToNumber b2
  => ToNumber c2
  => ToNumber d2
  => ToNumber a3
  => ToNumber b3
  => ToNumber c3
  => ToNumber d3
  => ToNumber a4
  => ToNumber b4
  => ToNumber c4
  => ToNumber d4
  => a1
  -> b1
  -> c1
  -> d1
  -> a2
  -> b2
  -> c2
  -> d2
  -> a3
  -> b3
  -> c3
  -> d3
  -> a4
  -> b4
  -> c4
  -> d4
  -> TransformFunction
matrix3d a1 b1 c1 d1 a2 b2 c2 d2 a3 b3 c3 d3 a4 b4 c4 d4 =
  TransformFunction
    $ fn "matrix3d"
    $ number a1 /\ number b1 /\ number c1 /\ number d1
        /\ number a2
        /\ number b2
        /\ number c2
        /\ number d2
        /\ number a3
        /\ number b3
        /\ number c3
        /\ number d3
        /\ number a4
        /\ number b4
        /\ number c4
        /\ number d4

-- https://www.w3.org/TR/css-transforms-2/#funcdef-translate3d

translate3d
  :: forall tx ty tz
   . LengthPercentageTag tx
  => LengthPercentageTag ty
  => LengthTag tz
  => Measure tx
  -> Measure ty
  -> Measure tz
  -> TransformFunction
translate3d tx ty tz =
  TransformFunction $ fn "translate3d" $ tx /\ ty /\ tz

-- https://www.w3.org/TR/css-transforms-2/#funcdef-translatez

translateZ :: forall t. LengthTag t => Measure t -> TransformFunction
translateZ tz = TransformFunction $ fn "translateZ" tz

-- https://www.w3.org/TR/css-transforms-2/#funcdef-scale3d

scale3d
  :: forall x y z
   . ToNumber x
  => ToNumber y
  => ToNumber z
  => x
  -> y
  -> z
  -> TransformFunction
scale3d sx sy sz =
  TransformFunction $ fn "scale3d" $ number sx /\ number sy /\ number sz

-- https://www.w3.org/TR/css-transforms-2/#funcdef-scalez

scaleZ :: forall a. ToNumber a => a -> TransformFunction
scaleZ sz = TransformFunction $ fn "scaleZ" $ number sz

-- https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function/rotate3d

rotate3d
  :: forall x y z t
   . ToNumber x
  => ToNumber y
  => ToNumber z
  => AngleTag t
  => x
  -> y
  -> z
  -> Measure t
  -> TransformFunction
rotate3d x y z a' =
  TransformFunction $ fn "rotate3d" $ number x /\ number y /\ number z /\ a'

-- https://www.w3.org/TR/css-transforms-2/#funcdef-rotatex

rotateX :: forall t. AngleTag t => Measure t -> TransformFunction
rotateX a' = TransformFunction $ fn "rotateX" a'

-- https://www.w3.org/TR/css-transforms-2/#funcdef-rotatey

rotateY :: forall t. AngleTag t => Measure t -> TransformFunction
rotateY a' = TransformFunction $ fn "rotateY" a'

-- https://www.w3.org/TR/css-transforms-2/#funcdef-rotatez

rotateZ :: forall t. AngleTag t => Measure t -> TransformFunction
rotateZ a' = TransformFunction $ fn "rotateZ" a'

-- https://www.w3.org/TR/css-transforms-2/#funcdef-perspective

class IsPerspective (a :: Type)

instance IsPerspective (Proxy "none")
instance LengthTag t => IsPerspective (Measure t)

perspective :: forall a. IsPerspective a => MultiVal a => a -> TransformFunction
perspective a' = TransformFunction $ fn "perspective" a'

-- https://www.w3.org/TR/css-transforms-1/#propdef-transform

class IsTransformList (a :: Type)

instance IsTransformList TransformFunction
instance IsTransformList xs => IsTransformList (TransformFunction /\ xs)

transform = Proxy :: Proxy "transform"

instance Property "transform"
instance Animatable "transform"

instance declarationTransformNone :: Declaration "transform" (Proxy "none") where
  pval = const val

else instance declarationTransformList ::
  ( IsTransformList a
  , MultiVal a
  ) =>
  Declaration "transform" a where
  pval = const $ intercalateMultiVal $ val " "

-- https://www.w3.org/TR/css-transforms-1/#propdef-transform-origin

transformOrigin = Proxy :: Proxy "transform-origin"

instance Property "transform-origin"
instance Animatable "transform-origin"

class IsTransformOrigin (a :: Type)

instance
  ( IsPositionX x
  , IsPositionY y
  , LengthTag tz
  ) =>
  IsTransformOrigin (x ~ y ~ Measure tz)
else instance IsPosition a => IsTransformOrigin a

instance declarationTransformOrigin ::
  ( IsTransformOrigin a
  , ToVal a
  ) =>
  Declaration "transform-origin" a where
  pval = const val

--------------------------------------------------------------------------------

-- Transitions
-- https://www.w3.org/TR/css-transitions-1

-- https://www.w3.org/TR/css-transitions-1/#propdef-transition-property

transitionProperty = Proxy :: Proxy "transition-property"

instance Property "transition-property"

all = Proxy :: Proxy "all"

class IsSingleTransitionPropertyList (a :: Type)

instance
  ( Animatable s
  , IsSingleTransitionPropertyList xs
  ) =>
  IsSingleTransitionPropertyList (Proxy s /\ xs)

instance Animatable s => IsSingleTransitionPropertyList (Proxy s)

instance declarationTransitionPropertyNone ::
  Declaration "transition-property" (Proxy "none") where
  pval = const val

else instance declarationTransitionPropertyAll ::
  Declaration "transition-property" (Proxy "all") where
  pval = const val

else instance declarationTransitionPropertyList ::
  ( IsSingleTransitionPropertyList a
  , MultiVal a
  ) =>
  Declaration "transition-property" a where
  pval = const $ intercalateMultiVal $ val "," <> Val _.separator

-- https://www.w3.org/TR/css-transitions-1/#propdef-transition-duration

transitionDuration = Proxy :: Proxy "transition-duration"

instance Property "transition-duration"

instance declarationTransitionDuration ::
  ( IsTimeList a
  , MultiVal a
  ) =>
  Declaration "transition-duration" a where
  pval = const $ intercalateMultiVal $ val "," <> Val _.separator

-- https://www.w3.org/TR/css-transitions-1/#propdef-transition-timing-function

transitionTimingFunction = Proxy :: Proxy "transition-timing-function"

instance Property "transition-timing-function"

instance propertyTransitionTimingFunction ::
  ( IsList EasingFunction a
  , MultiVal a
  ) =>
  Declaration "transition-timing-function" a where
  pval = const $ intercalateMultiVal $ val "," <> Val _.separator

-- https://www.w3.org/TR/css-transitions-1/#propdef-transition-delay

transitionDelay = Proxy :: Proxy "transition-delay"

instance Property "transition-delay"

instance declarationTransitionDelay ::
  ( IsTimeList a
  , MultiVal a
  ) =>
  Declaration "transition-delay" a where
  pval = const $ intercalateMultiVal $ val "," <> Val _.separator

--------------------------------------------------------------------------------

-- Basic User Interface
-- https://www.w3.org/TR/css-ui-4/

-- https://www.w3.org/TR/css-ui-4/#propdef-outline-width

outlineWidth = Proxy :: Proxy "outline-width"

instance Property "outline-width"
instance Animatable "outline-width"

instance declarationOutlineWidth ::
  ( IsLineWidth a
  , ToVal a
  ) =>
  Declaration "outline-width" a where
  pval = const val

-- https://www.w3.org/TR/css-ui-4/#propdef-outline-style

outlineStyle = Proxy :: Proxy "outline-style"

instance Property "outline-style"

class OutlineLineStyleKeyword (s :: Symbol)

instance OutlineLineStyleKeyword "auto"
instance OutlineLineStyleKeyword "none"
instance OutlineLineStyleKeyword "dotted"
instance OutlineLineStyleKeyword "dashed"
instance OutlineLineStyleKeyword "solid"
instance OutlineLineStyleKeyword "double"
instance OutlineLineStyleKeyword "groove"
instance OutlineLineStyleKeyword "ridge"
instance OutlineLineStyleKeyword "inset"
instance OutlineLineStyleKeyword "outset"

instance declarationOutlineStyle ::
  ( OutlineLineStyleKeyword s
  , ToVal (Proxy s)
  ) =>
  Declaration "outline-style" (Proxy s) where
  pval = const val

-- https://www.w3.org/TR/css-ui-4/#propdef-outline-color

outlineColor = Proxy :: Proxy "outline-color"

instance Property "outline-color"
instance Animatable "outline-color"

invert = Proxy :: Proxy "invert"

instance declarationOutlineColorInvert ::
  Declaration "outline-color" (Proxy "invert") where
  pval = const val

else instance declarationOutlineColorIsColor ::
  ( IsColor a
  , ToVal a
  ) =>
  Declaration "outline-color" a where
  pval = const val

-- https://www.w3.org/TR/css-ui-4/#propdef-outline-offset

outlineOffset = Proxy :: Proxy "outline-offset"

instance Property "outline-offset"
instance Animatable "outline-offset"

instance declarationOutlineOffset ::
  LengthTag t =>
  Declaration "outline-offset" (Measure t) where
  pval = const val

-- https://www.w3.org/TR/css-ui-4/#propdef-cursor

cursor = Proxy :: Proxy "cursor"

instance Property "cursor"

contextMenu = Proxy :: Proxy "context-menu"
help = Proxy :: Proxy "help"
pointer = Proxy :: Proxy "pointer"
wait = Proxy :: Proxy "wait"
cell = Proxy :: Proxy "cell"
crosshair = Proxy :: Proxy "crosshair"
text = Proxy :: Proxy "text"
verticalText = Proxy :: Proxy "vertical-text"
alias = Proxy :: Proxy "alias"
copy = Proxy :: Proxy "copy"
move = Proxy :: Proxy "move"
noDrop = Proxy :: Proxy "no-drop"
notAllowed = Proxy :: Proxy "not-allowed"
grab = Proxy :: Proxy "grab"
grabbing = Proxy :: Proxy "grabbing"
eResize = Proxy :: Proxy "e-resize"
nResize = Proxy :: Proxy "n-resize"
neResize = Proxy :: Proxy "ne-resize"
nwResize = Proxy :: Proxy "nw-resize"
sResize = Proxy :: Proxy "s-resize"
seResize = Proxy :: Proxy "se-resize"
swResize = Proxy :: Proxy "sw-resize"
wResize = Proxy :: Proxy "w-resize"
ewResize = Proxy :: Proxy "ew-resize"
nsResize = Proxy :: Proxy "ns-resize"
neswResize = Proxy :: Proxy "nesw-resize"
nwseResize = Proxy :: Proxy "nwse-resize"
colResize = Proxy :: Proxy "col-resize"
rowResize = Proxy :: Proxy "row-resize"
allScroll = Proxy :: Proxy "all-scroll"
zoomIn = Proxy :: Proxy "zoom-in"
zoomOut = Proxy :: Proxy "zoom-out"

class GenericCursorKeyword (s :: Symbol)

instance GenericCursorKeyword "auto"
instance GenericCursorKeyword "default"
instance GenericCursorKeyword "none"
instance GenericCursorKeyword "context-menu"
instance GenericCursorKeyword "help"
instance GenericCursorKeyword "pointer"
instance GenericCursorKeyword "progress"
instance GenericCursorKeyword "wait"
instance GenericCursorKeyword "cell"
instance GenericCursorKeyword "crosshair"
instance GenericCursorKeyword "text"
instance GenericCursorKeyword "vertical-text"
instance GenericCursorKeyword "alias"
instance GenericCursorKeyword "copy"
instance GenericCursorKeyword "move"
instance GenericCursorKeyword "no-drop"
instance GenericCursorKeyword "not-allowed"
instance GenericCursorKeyword "grab"
instance GenericCursorKeyword "grabbing"
instance GenericCursorKeyword "e-resize"
instance GenericCursorKeyword "n-resize"
instance GenericCursorKeyword "ne-resize"
instance GenericCursorKeyword "nw-resize"
instance GenericCursorKeyword "s-resize"
instance GenericCursorKeyword "se-resize"
instance GenericCursorKeyword "sw-resize"
instance GenericCursorKeyword "w-resize"
instance GenericCursorKeyword "ew-resize"
instance GenericCursorKeyword "ns-resize"
instance GenericCursorKeyword "nesw-resize"
instance GenericCursorKeyword "nwse-resize"
instance GenericCursorKeyword "col-resize"
instance GenericCursorKeyword "row-resize"
instance GenericCursorKeyword "all-scroll"
instance GenericCursorKeyword "zoom-in"
instance GenericCursorKeyword "zoom-out"

class IsCursorImage (a :: Type)

instance IsCursorImage URL
instance IsCursorImage (URL ~ Int ~ Int)

class IsCursorList (a :: Type)

instance GenericCursorKeyword s => IsCursorList (Proxy s)
instance (IsCursorImage x, IsCursorList xs) => IsCursorList (x /\ xs)

instance declarationCursor ::
  ( IsCursorList xs
  , MultiVal xs
  ) =>
  Declaration "cursor" xs where
  pval = const $ intercalateMultiVal (val "," <> Val _.separator)

-- https://www.w3.org/TR/css-ui-4/#propdef-appearance

appearance = Proxy :: Proxy "appearance"

instance Property "appearance"

textfield = Proxy :: Proxy "textfield"
menulistButton = Proxy :: Proxy "menulist-button"

class AppearanceKeyword (s :: Symbol)

instance AppearanceKeyword "none"
instance AppearanceKeyword "auto"
instance AppearanceKeyword "textfield"
instance AppearanceKeyword "menulist-button"

instance declarationAppearance ::
  ( AppearanceKeyword s
  , ToVal (Proxy s)
  ) =>
  Declaration "appearance" (Proxy s) where
  pval = const val

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/CSS2/visufx.html

visibility = Proxy :: Proxy "visibility"

instance Property "visibility"
instance Animatable "visibility"

collapse = Proxy :: Proxy "collapse"

class VisibilityKeyword (s :: Symbol)

instance VisibilityKeyword "visible"
instance VisibilityKeyword "hidden"
instance VisibilityKeyword "collapse"

instance declarationVisibility ::
  ( VisibilityKeyword s
  , ToVal (Proxy s)
  ) =>
  Declaration "visibility" (Proxy s) where
  pval = const val

--------------------------------------------------------------------------------

-- Visual Formatting
-- https://www.w3.org/TR/CSS2/visuren.html

-- https://www.w3.org/TR/CSS2/visuren.html#propdef-clear

clear = Proxy :: Proxy "clear"

instance Property "clear"

class ClearKeyword (s :: Symbol)

instance ClearKeyword "none"
instance ClearKeyword "left"
instance ClearKeyword "right"
instance ClearKeyword "both"

instance declarationClear ::
  ( ClearKeyword s
  , IsSymbol s
  ) =>
  Declaration "clear" (Proxy s) where
  pval = const val

-- https://www.w3.org/TR/CSS2/visuren.html#propdef-float

float = Proxy :: Proxy "float"

instance Property "float"

class FloatKeyword (s :: Symbol)

instance FloatKeyword "left"
instance FloatKeyword "right"
instance FloatKeyword "none"

instance declarationFloat ::
  ( FloatKeyword s
  , IsSymbol s
  ) =>
  Declaration "float" (Proxy s) where
  pval = const val

-- https://www.w3.org/TR/CSS2/visuren.html#propdef-z-index

zIndex = Proxy :: Proxy "z-index"

instance Property "z-index"
instance Animatable "z-index"

instance declarationZIndexAuto :: Declaration "z-index" (Proxy "auto") where
  pval = const val

instance declarationZIndexInt :: Declaration "z-index" Int where
  pval = const val

--------------------------------------------------------------------------------

-- Writing Modes
-- https://www.w3.org/TR/css-writing-modes-4/

direction = Proxy :: Proxy "direction"

instance Property "direction"

ltr = Proxy :: Proxy "ltr"
rtl = Proxy :: Proxy "rtl"

class DirectionKeyword (s :: Symbol)

instance DirectionKeyword "ltr"
instance DirectionKeyword "rtl"

instance declarationDirectionSymbol ::
  ( DirectionKeyword s
  , IsSymbol s
  ) =>
  Declaration "direction" (Proxy s) where
  pval = const val

--------------------------------------------------------------------------------

-- Common values

fn :: forall args. MultiVal args => String -> args -> Val
fn name' args = Val \c ->
  let
    args' = runVal c $ intercalateMultiVal (val "," <> Val _.separator) args
  in
    name' <> "(" <> args' <> ")"

class ToNumber (a :: Type) where
  number :: a -> Number

instance ToNumber Int where
  number = Int.toNumber

instance ToNumber Number where
  number = identity

data Pair a b = Pair a b

infixr 7 type Pair as ~

infixr 7 Pair as ~

data Measure (t :: Type)
  = MeasureVal Val
  | Measure Number String

instance ToVal (Measure t) where
  val =
    case _ of
      MeasureVal v ->
        v
      Measure n u ->
        val $ Number.toString n <> u

measure :: forall n t. ToNumber n => String -> n -> Measure t
measure u n = Measure (number n) u

data Nil

nil :: Measure Nil
nil = Measure 0.0 mempty

data Length

ch :: forall a. ToNumber a => a -> Measure Length
ch = measure "ch"

em :: forall a. ToNumber a => a -> Measure Length
em = measure "em"

ex :: forall a. ToNumber a => a -> Measure Length
ex = measure "ex"

rem :: forall a. ToNumber a => a -> Measure Length
rem = measure "rem"

vh :: forall a. ToNumber a => a -> Measure Length
vh = measure "vh"

vw :: forall a. ToNumber a => a -> Measure Length
vw = measure "vw"

vmin :: forall a. ToNumber a => a -> Measure Length
vmin = measure "vmin"

vmax :: forall a. ToNumber a => a -> Measure Length
vmax = measure "vmax"

px :: forall a. ToNumber a => a -> Measure Length
px = measure "px"

cm :: forall a. ToNumber a => a -> Measure Length
cm = measure "cm"

mm :: forall a. ToNumber a => a -> Measure Length
mm = measure "mm"

pc :: forall a. ToNumber a => a -> Measure Length
pc = measure "pc"

pt :: forall a. ToNumber a => a -> Measure Length
pt = measure "pt"

inch :: forall a. ToNumber a => a -> Measure Length
inch = measure "in"

class LengthTag (a :: Type)

instance LengthTag Length
instance LengthTag Nil

data Percentage

pct :: forall a. ToNumber a => a -> Measure Percentage
pct = measure "%"

class PercentageTag (a :: Type)

instance PercentageTag Percentage
instance PercentageTag Nil

data LengthPercentage

class LengthPercentageTag (a :: Type)

instance LengthPercentageTag Length
instance LengthPercentageTag Percentage
instance LengthPercentageTag LengthPercentage
instance LengthPercentageTag Nil

data Angle

deg :: forall a. ToNumber a => a -> Measure Angle
deg = measure "deg"

rad :: forall a. ToNumber a => a -> Measure Angle
rad = measure "rad"

turn :: forall a. ToNumber a => a -> Measure Angle
turn = measure "turn"

class AngleTag (a :: Type)

instance AngleTag Angle
instance AngleTag Nil

data Time

ms :: forall a. ToNumber a => a -> Measure Time
ms = measure "ms"

sec :: forall a. ToNumber a => a -> Measure Time
sec = measure "s"

class TimeTag (a :: Type)

instance TimeTag Time
instance TimeTag Nil

data Add = Add
data Subtract = Subtract
data Multiply = Multiply
data Divide = Divide

instance ToVal Add where
  val = const $ val "+"

instance ToVal Subtract where
  val = const $ val "-"

instance ToVal Multiply where
  val = const $ val "*"

instance ToVal Divide where
  val = const $ val "/"

class Calc :: Type -> Type -> Type -> Type -> Constraint
class Calc op a b c | op -> c

instance
  Calc Add (Measure Length) (Measure Percentage) (Measure LengthPercentage)
else instance
  Calc Add
    (Measure Length)
    (Measure LengthPercentage)
    (Measure LengthPercentage)
else instance
  Calc Add
    (Measure Percentage)
    (Measure LengthPercentage)
    (Measure LengthPercentage)
else instance Calc Add a a a
else instance Calc Add a b c => Calc Add b a c
else instance Calc Add a b c => Calc Subtract a b c
else instance ToNumber b => Calc Multiply a b a
else instance Calc Multiply a b c => Calc Divide a b c

calc :: forall op a b. ToVal op => ToVal a => ToVal b => op -> a -> b -> Val
calc op a' b' = fn "calc" $ intercalateMultiVal " " $ val a' /\ val op /\ val b'

add
  :: forall ta tb tc
   . Calc Add (Measure ta) (Measure tb) (Measure tc)
  => Measure ta
  -> Measure tb
  -> Measure tc
add =
  curry $
    case _ of
      Measure an au /\ Measure bn bu | au == bu ->
        Measure (an + bn) au
      a' /\ b' ->
        MeasureVal $ calc Add a' b'

infixl 8 add as @+@

subtract
  :: forall ta tb tc
   . Calc Subtract (Measure ta) (Measure tb) (Measure tc)
  => Measure ta
  -> Measure tb
  -> Measure tc
subtract =
  curry $
    case _ of
      Measure an au /\ Measure bn bu | au == bu ->
        Measure (an - bn) au
      a' /\ b' ->
        MeasureVal $ calc Subtract a' b'

infixl 8 subtract as @-@

multiply
  :: forall ta b
   . ToNumber b
  => ToVal b
  => Calc Multiply (Measure ta) b (Measure ta)
  => Measure ta
  -> b
  -> Measure ta
multiply =
  curry $
    case _ of
      Measure n u /\ b' ->
        Measure (n * number b') u
      a' /\ b' ->
        MeasureVal $ calc Multiply a' b'

infixl 9 multiply as @*

multiplyFlipped
  :: forall ta b
   . ToVal b
  => ToNumber b
  => Calc Multiply (Measure ta) b (Measure ta)
  => b
  -> Measure ta
  -> Measure ta
multiplyFlipped =
  curry $
    case _ of
      b' /\ Measure n u ->
        Measure (n * number b') u
      b' /\ a' ->
        MeasureVal $ calc Multiply b' a'

infixl 9 multiplyFlipped as *@

divide
  :: forall ta b
   . ToNumber b
  => ToVal b
  => Calc Divide (Measure ta) b (Measure ta)
  => Measure ta
  -> b
  -> Measure ta
divide =
  curry $
    case _ of
      Measure n u /\ b' ->
        Measure (n / number b') u
      a' /\ b' ->
        MeasureVal $ calc Divide a' b'

infixl 9 divide as @/

-- https://www.w3.org/TR/css-values-3/#url-value

newtype URL = URL String

instance toValURL :: ToVal URL where
  val (URL x) = val $ fn "url" $ quote x

url :: String -> URL
url = URL

-- https://www.w3.org/TR/css-values-4/#typedef-position

class IsPosition (a :: Type)

instance IsPosition (Proxy "left")
instance IsPosition (Proxy "center")
instance IsPosition (Proxy "right")
instance IsPosition (Proxy "top")
instance IsPosition (Proxy "bottom")
instance LengthPercentageTag t => IsPosition (Measure t)

class IsPositionX (a :: Type)

instance IsPositionX (Proxy "left")
instance IsPositionX (Proxy "center")
instance IsPositionX (Proxy "right")
instance LengthPercentageTag t => IsPositionX (Measure t)

class IsPositionY (a :: Type)

instance IsPositionY (Proxy "top")
instance IsPositionY (Proxy "center")
instance IsPositionY (Proxy "bottom")
instance LengthPercentageTag t => IsPositionY (Measure t)

instance (IsPositionX x, IsPositionY y) => IsPosition (x ~ y)

--------------------------------------------------------------------------------

-- Global keywords

-- https://www.w3.org/TR/css3-values/#common-keywords

newtype CommonKeyword = CommonKeyword String

instance ToVal CommonKeyword where
  val (CommonKeyword x) = val x

inherit :: CommonKeyword
inherit = CommonKeyword "inherit"

initial :: CommonKeyword
initial = CommonKeyword "initial"

unset :: CommonKeyword
unset = CommonKeyword "unset"
