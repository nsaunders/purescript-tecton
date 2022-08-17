module PSCSS where

import Prelude hiding (add)

import Color (Color, cssStringHSLA, toHexString)
import Control.Monad.Writer (Writer, execWriter, tell)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Array (replicate)
import Data.Array as Array
import Data.Either as Either
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex)
import Data.Int as Int
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty)
import Data.NonEmpty as NE
import Data.Number.Format as Number
import Data.Ord (abs)
import Data.String as String
import Data.String.Regex (regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (global)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (curry)
import Data.Tuple.Nested (type (/\), (/\))
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Prelude (class ListToRow)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------

-- Utilities

foreign import quote :: String -> String

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

instance (ToVal a, ToVal b) => ToVal (Pair a b) where
  val (a ~ b) = val a <> val " " <> val b

class MultiVal (a :: Type) where
  multiValImpl :: List Val -> a -> List Val

instance multiValImplXXS
  :: (ToVal x, MultiVal xs) => MultiVal (x /\ xs) where
  multiValImpl acc (x /\ xs) = multiValImpl (val x : acc) xs

else instance multiValImplX :: ToVal x => MultiVal x where
  multiValImpl acc x = val x : acc

multiVal :: forall a. MultiVal a => a -> Array Val
multiVal = Array.reverse <<< Array.fromFoldable <<< multiValImpl Nil

joinVals 
  :: forall s f
   . ToVal s
  => FoldableWithIndex Int f
  => s
  -> f Val
  -> Val
joinVals sep =
  foldlWithIndex
    (\i a b -> a <> (if i > 0 then val sep else mempty) <> b)
    mempty

runVal :: Configuration -> Val -> String
runVal x (Val f) = f x

--------------------------------------------------------------------------------

-- Supported style properties

data Property' = Property'

type SupportedProperties' (v :: Type) =
  ( animationDelay :: v
  , animationDirection :: v
  , animationDuration :: v
  , animationFillMode :: v
  , animationIterationCount :: v
  , animationName :: v
  , animationPlayState :: v
  , animationTimingFunction :: v
  , backgroundAttachment :: v
  , backgroundClip :: v
  , backgroundColor :: v
  , backgroundImage :: v
  , backgroundOrigin :: v
  , backgroundPosition :: v
  , backgroundRepeat :: v
  , backgroundSize :: v
  , border :: v
  , borderBottomColor :: v
  , borderBottomLeftRadius :: v
  , borderBottomRightRadius :: v
  , borderBottomStyle :: v
  , borderBottomWidth :: v
  , borderColor :: v
  , borderLeftColor :: v
  , borderLeftStyle :: v
  , borderLeftWidth :: v
  , borderRadius :: v
  , borderRightColor :: v
  , borderRightStyle :: v
  , borderRightWidth :: v
  , borderStyle :: v
  , borderTopColor :: v
  , borderTopLeftRadius :: v
  , borderTopRightRadius :: v
  , borderTopStyle :: v
  , borderTopWidth :: v
  , borderWidth :: v
  , boxShadow :: v
  , color :: v
  , direction :: v
  , flexDirection :: v
  , flexWrap :: v
  , height :: v
  , letterSpacing :: v
  , margin :: v
  , marginBottom :: v
  , marginLeft :: v
  , marginRight :: v
  , marginTop :: v
  , maxHeight :: v
  , maxWidth :: v
  , minHeight :: v
  , minWidth :: v
  , opacity :: v
  , order :: v
  , overflow :: v
  , overflowX :: v
  , overflowY :: v
  , padding :: v
  , paddingBottom :: v
  , paddingLeft :: v
  , paddingRight :: v
  , paddingTop :: v
  , textAlign :: v
  , textDecorationColor :: v
  , textDecorationLine :: v
  , textDecorationStyle :: v
  , textOverflow :: v
  , textShadow :: v
  , textTransform :: v
  , transform :: v
  , transformOrigin :: v
  , verticalAlign :: v
  , whiteSpace :: v
  , wordSpacing :: v
  , width :: v
  )

defaultDeclarations :: { | SupportedProperties }
defaultDeclarations =
  { animationDelay: v
  , animationDirection: v
  , animationDuration: v
  , animationFillMode: v
  , animationIterationCount: v
  , animationName: v
  , animationPlayState: v
  , animationTimingFunction: v
  , backgroundAttachment: v
  , backgroundClip: v
  , backgroundColor: v
  , backgroundImage: v
  , backgroundOrigin: v
  , backgroundPosition: v
  , backgroundRepeat: v
  , backgroundSize: v
  , border: v
  , borderBottomColor: v
  , borderBottomLeftRadius: v
  , borderBottomRightRadius: v
  , borderBottomStyle: v
  , borderBottomWidth: v
  , borderColor: v
  , borderLeftColor: v
  , borderLeftStyle: v
  , borderLeftWidth: v
  , borderRadius: v
  , borderRightColor: v
  , borderRightStyle: v
  , borderRightWidth: v
  , borderStyle: v
  , borderTopColor: v
  , borderTopLeftRadius: v
  , borderTopRightRadius: v
  , borderTopStyle: v
  , borderTopWidth: v
  , borderWidth: v
  , boxShadow: v
  , color: v
  , direction: v
  , flexDirection: v
  , flexWrap: v
  , height: v
  , letterSpacing: v
  , margin: v
  , marginBottom: v
  , marginLeft: v
  , marginRight: v
  , marginTop: v
  , maxHeight: v
  , maxWidth: v
  , minHeight: v
  , minWidth: v
  , opacity: v
  , order: v
  , overflow: v
  , overflowX: v
  , overflowY: v
  , padding: v
  , paddingBottom: v
  , paddingLeft: v
  , paddingRight: v
  , paddingTop: v
  , textAlign: v
  , textDecorationColor: v
  , textDecorationLine: v
  , textDecorationStyle: v
  , textOverflow: v
  , textShadow: v
  , textTransform: v
  , transform: v
  , transformOrigin: v
  , verticalAlign: v
  , whiteSpace: v
  , wordSpacing: v
  , width: v
  }
  where
    v = Nothing

type SupportedProperties = SupportedProperties' (Maybe Val)

class Property (p :: Symbol) (v :: Type) where
  pval :: Proxy p -> v -> Val

instance ConvertOption Property' p (Maybe CommonKeyword) (Maybe Val) where
  convertOption _ _ = map val

else instance ConvertOption Property' p CommonKeyword (Maybe Val) where
  convertOption _ _ = pure <<< val

else instance Property p v => ConvertOption Property' p (Maybe v) (Maybe Val) where
  convertOption _ = map <<< pval

else instance Property p v => ConvertOption Property' p v (Maybe Val) where
  convertOption _ p = pure <<< pval p

--------------------------------------------------------------------------------

-- Style sheet structures

newtype NestedRule = NestedRule Val

data Closed

newtype Selector (tag :: Type) = Selector Val

data Statement
  = NestedAtRule NestedRule (Array Statement)
  | Ruleset (NonEmpty Array (Selector Closed)) Val

newtype KeyframesName = KeyframesName String

keyframesName :: String -> KeyframesName
keyframesName = KeyframesName

derive newtype instance ToVal KeyframesName

newtype Keyframes = Keyframes KeyframesName

keyframes :: KeyframesName -> Keyframes
keyframes = Keyframes

data KeyframeBlock = KeyframeBlock (NonEmpty Array (Measure Percentage)) Val

class Statement' (a :: Type) (b :: Type) (c :: Type) | a -> c where
  statement :: a -> b -> c

instance Statement' NestedRule (Writer (Array Statement) Unit) (Writer (Array Statement) Unit) where
  statement rule = tell <<< pure <<< NestedAtRule rule <<< execWriter
else instance Statement' Keyframes (Writer (Array KeyframeBlock) Unit) (Writer (Array Statement) Unit) where
  statement (Keyframes (KeyframesName k)) keyframeBlocks =
    tell $ pure $ NestedAtRule (NestedRule $ val $ "keyframes " <> k) $ keyframeBlockToStatement <$> execWriter keyframeBlocks
    where
      keyframeBlockToStatement (KeyframeBlock selectors declarations) =
        Ruleset ((Selector <<< val) <$> selectors) declarations
else instance
  ConvertOptionsWithDefaults
    Property'
    { | SupportedProperties }
    { | providedProperties }
    { | SupportedProperties }
  => Statement' (NonEmpty Array (Measure Percentage)) { | providedProperties } (Writer (Array KeyframeBlock) Unit) where
  statement selectors provided =
    tell $ pure $ KeyframeBlock selectors $ declarationBlock provided
else instance
  ConvertOptionsWithDefaults
    Property'
    { | SupportedProperties }
    { | providedProperties }
    { | SupportedProperties }
  => Statement' (NonEmpty Array (Selector tag)) { | providedProperties } (Writer (Array Statement) Unit) where
  statement selectors provided =
    tell $ pure $ Ruleset (closeSelector <$> selectors) $ declarationBlock provided
else instance Statement' (NonEmpty Array a) b c => Statement' a b c where
  statement selector = statement (NE.singleton selector :: NonEmpty Array a)

infixr 0 statement as ?

class CollectDeclarations (xs :: RowList Type) (row :: Row Type) where
  collectDeclarations :: Proxy xs -> Record row -> List Val

instance CollectDeclarations RL.Nil row where
  collectDeclarations _ _ = mempty

instance
  ( IsSymbol property
  , Row.Cons property (Maybe Val) tailRow row
  , Row.Lacks property tailRow
  , ListToRow tailRowList tailRow
  , CollectDeclarations tailRowList tailRow
  ) => CollectDeclarations (RL.Cons property (Maybe Val) tailRowList) row where
  collectDeclarations _ rec =
    let
      camelToKebab s =
        case regex "[A-Z]" global of
          Either.Left _ ->
            s
          Either.Right caps ->
            Regex.replace' caps (const <<< ("-" <> _) <<< String.toLower) s
      field = Proxy :: _ property
      rest =
        collectDeclarations (Proxy :: _ tailRowList) (Record.delete field rec)
    in
      case Record.get field rec of
        Just v ->
          let
            decl =
              Val \c@{ separator } ->
                runVal c (val $ camelToKebab $ reflectSymbol field)
                <> ":"
                <> separator
                <> runVal c v
          in
            decl : rest
        Nothing ->
          rest

declarationBlock
  :: forall providedProperties rl
   . RowToList SupportedProperties rl
  => CollectDeclarations rl SupportedProperties
  => ConvertOptionsWithDefaults Property'
       { | SupportedProperties }
       { | providedProperties }
       { | SupportedProperties }
  => { | providedProperties }
  -> Val
declarationBlock provided =
  Val \config@{ newline, finalSemicolon } ->
    ( runVal config $
        joinVals
          (";" <> newline)
          ( map
            (\line ->
              Val \config'@{ indentLevel, indentation } ->
                (String.joinWith "" $ replicate indentLevel indentation)
                <>
                runVal config' line
            )
            $ Array.reverse
            $ Array.fromFoldable
            $ collectDeclarations (Proxy :: _ rl) all'
          )
    ) <> if finalSemicolon then ";" else mempty
  where
    all' = convertOptionsWithDefaults Property' defaultDeclarations provided

--------------------------------------------------------------------------------

-- Rendering

class Render (a :: Type) where
  render :: Configuration -> a -> String

instance Render (Writer (Array Statement) Unit) where
  render config =
    runVal config
      <<< joinVals config.newline
      <<< map renderStatement
      <<< execWriter

    where

    renderStatement =
      case _ of
        Ruleset selectors declarations ->
          let
            selector =
              Val \c@{ separator } ->
                String.joinWith ("," <> separator)
                  $ Array.fromFoldable
                  $ (\(Selector s) -> runVal c s) <$> selectors
          in
            nested selector declarations
        NestedAtRule (NestedRule nestedRule) statements ->
          nested (val "@" <> nestedRule) $
            joinVals config.newline (renderStatement <$> statements)

    nested outer inner =
      Val \c@{ indentLevel, indentation, newline, separator } ->
        let
          indent = String.joinWith mempty $ replicate indentLevel indentation
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

else instance
  ConvertOptionsWithDefaults
    Property'
    { | SupportedProperties }
    { | providedProperties }
    { | SupportedProperties }
  => Render (Record providedProperties) where
  render c =
    runVal c { newline = if c.newline /= mempty then " " else mempty }
      <<< declarationBlock

else instance ToVal a => Render a where
  render c = runVal c <<< val

--------------------------------------------------------------------------------

-- Common values

fn :: forall f. FoldableWithIndex Int f => String -> f Val -> Val
fn name' args = Val \c ->
  let
    args' = runVal c $ joinVals ("," <> c.separator) args
  in
    name' <> "(" <> args' <> ")"

class ToNumber (a :: Type) where
  toNumber :: a -> Number

instance ToNumber Int where
  toNumber = Int.toNumber

instance ToNumber Number where
  toNumber = identity

data Pair a b = Pair a b

infixr 7 type Pair as ~

infixr 7 Pair as ~

data Measure (tag :: Type)
  = MeasureVal Val
  | Measure Number String

instance ToVal (Measure tag) where
  val =
    case _ of
      MeasureVal v ->
        v
      Measure n u ->
        val $ Number.toString n <> u

measure :: forall n a. ToNumber n => String -> n -> Measure a
measure u n = Measure (toNumber n) u

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

instance Calc Add (Measure Length) (Measure Percentage) (Measure LengthPercentage)
else instance Calc Add (Measure Length) (Measure LengthPercentage) (Measure LengthPercentage)
else instance Calc Add (Measure Percentage) (Measure LengthPercentage) (Measure LengthPercentage)
else instance Calc Add a a a
else instance Calc Add a b c => Calc Add b a c
else instance Calc Add a b c => Calc Subtract a b c
else instance ToNumber b => Calc Multiply a b a
else instance Calc Multiply a b c => Calc Divide a b c

calc :: forall op a b. ToVal op => ToVal a => ToVal b => op -> a -> b -> Val
calc op a b = fn "calc" [joinVals " " [val a, val op, val b]]

add
  :: forall a b c
   . Calc Add (Measure a) (Measure b) (Measure c)
  => Measure a
  -> Measure b
  -> Measure c
add =
  curry $
    case _ of
      Measure an au /\ Measure bn bu | au == bu ->
        Measure (an + bn) au
      a /\ b ->
        MeasureVal $ calc Add a b

infixl 8 add as @+@

subtract
  :: forall a b c
   . Calc Subtract (Measure a) (Measure b) (Measure c)
  => Measure a
  -> Measure b
  -> Measure c
subtract =
  curry $
    case _ of
      Measure an au /\ Measure bn bu | au == bu ->
        Measure (an - bn) au
      a /\ b ->
        MeasureVal $ calc Subtract a b

infixl 8 subtract as @-@

multiply
  :: forall a b
   . ToNumber b
  => ToVal b
  => Calc Multiply (Measure a) b (Measure a)
  => Measure a
  -> b
  -> Measure a
multiply =
  curry $
    case _ of
      Measure n u /\ b ->
        Measure (n * toNumber b) u
      a /\ b ->
        MeasureVal $ calc Multiply a b

infixl 9 multiply as @*

multiplyFlipped
  :: forall a b
   . ToVal b
  => ToNumber b
  => Calc Multiply (Measure a) b (Measure a)
  => b
  -> Measure a
  -> Measure a
multiplyFlipped =
  curry $
    case _ of
      b /\ Measure n u ->
        Measure (n * toNumber b) u
      b /\ a ->
        MeasureVal $ calc Multiply b a

infixl 9 multiplyFlipped as *@

divide
  :: forall a b
   . ToNumber b
  => ToVal b
  => Calc Divide (Measure a) b (Measure a)
  => Measure a
  -> b
  -> Measure a
divide =
  curry $
    case _ of
      Measure n u /\ b ->
        Measure (n / toNumber b) u
      a /\ b ->
        MeasureVal $ calc Divide a b

infixl 9 divide as @/

-- https://www.w3.org/TR/css-values-3/#url-value

newtype URL = URL String

instance toValURL :: ToVal URL where
  val (URL x) = val $ fn "url" [val $ quote x]

url :: String -> URL
url = URL

-- https://www.w3.org/TR/css-values-4/#typedef-position

class IsPosition (a :: Type)

instance IsPosition Left
instance IsPosition Center
instance IsPosition Right
instance IsPosition Top
instance IsPosition Bottom
instance LengthPercentageTag a => IsPosition (Measure a)

class IsPositionX (a :: Type)
instance IsPositionX Left
instance IsPositionX Center
instance IsPositionX Right
instance LengthPercentageTag a => IsPositionX (Measure a)

class IsPositionY (a :: Type)
instance IsPositionY Top
instance IsPositionY Center
instance IsPositionY Bottom
instance LengthPercentageTag a => IsPositionY (Measure a)

instance isPositionPair
  :: ( IsPositionX x
     , IsPositionY y
     )
  => IsPosition (x ~ y)

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/mediaqueries-3/

media
  :: forall mediaType providedMediaFeatures rl
   . IsMediaType mediaType
  => RowToList SupportedMediaFeatures rl
  => CollectDeclarations rl SupportedMediaFeatures
  => ConvertOptionsWithDefaults MediaFeature' { | SupportedMediaFeatures } { | providedMediaFeatures } { | SupportedMediaFeatures }
  => mediaType
  -> Record providedMediaFeatures
  -> NestedRule
media mediaType providedMediaFeatures =
  let
    features =
      joinVals ""
      $ map (\v -> val " and (" <> v <> val ")")
        $ Array.reverse
        $ Array.fromFoldable
        $ collectDeclarations (Proxy :: _ rl) $
            convertOptionsWithDefaults
              MediaFeature'
              defaultMediaFeatures
              providedMediaFeatures
  in
    NestedRule $ val "media " <> val mediaType <> features

newtype MediaType = MediaType String

derive newtype instance ToVal MediaType

print :: MediaType
print = MediaType "print"

screen :: MediaType
screen = MediaType "screen"

class ToVal a <= IsMediaType (a :: Type)
instance IsMediaType All
instance IsMediaType MediaType

type SupportedMediaFeatures' (v :: Type) =
  ( aspectRatio :: v
  , color :: v
  , colorIndex :: v
  , deviceAspectRatio :: v
  , deviceHeight :: v
  , deviceWidth :: v
  , height :: v
  , maxAspectRatio :: v
  , maxColor :: v
  , maxColorIndex :: v
  , maxDeviceAspectRatio :: v
  , maxDeviceHeight :: v
  , maxDeviceWidth :: v
  , maxHeight :: v
  , maxMonochrome :: v
  , maxResolution :: v
  , maxWidth :: v
  , minAspectRatio :: v
  , minColor :: v
  , minColorIndex :: v
  , minDeviceAspectRatio :: v
  , minDeviceHeight :: v
  , minDeviceWidth :: v
  , minHeight :: v
  , minMonochrome :: v
  , minResolution :: v
  , minWidth :: v
  , monochrome :: v
  , orientation :: v
  , resolution :: v
  , width :: v
  )

defaultMediaFeatures :: { | SupportedMediaFeatures }
defaultMediaFeatures =
  { aspectRatio: v
  , color: v
  , colorIndex: v
  , deviceAspectRatio: v
  , deviceHeight: v
  , deviceWidth: v
  , height: v
  , maxAspectRatio: v
  , maxColor: v
  , maxColorIndex: v
  , maxDeviceAspectRatio: v
  , maxDeviceHeight: v
  , maxDeviceWidth: v
  , maxHeight: v
  , maxMonochrome: v
  , maxResolution: v
  , maxWidth: v
  , minAspectRatio: v
  , minColor: v
  , minColorIndex: v
  , minDeviceAspectRatio: v
  , minDeviceHeight: v
  , minDeviceWidth: v
  , minHeight: v
  , minMonochrome: v
  , minResolution: v
  , minWidth: v
  , monochrome: v
  , orientation: v
  , resolution: v
  , width: v
  }
  where
    v = Nothing

type SupportedMediaFeatures = SupportedMediaFeatures' (Maybe Val)

data MediaFeature' = MediaFeature'

class ToVal v <= MediaFeature (f :: Symbol) (v :: Type)

instance MediaFeature f v => ConvertOption MediaFeature' f (Maybe v) (Maybe Val) where
  convertOption _ _ = map val

else instance MediaFeature f v => ConvertOption MediaFeature' f v (Maybe Val) where
  convertOption _ _ = pure <<< val

-- https://www.w3.org/TR/mediaqueries-3/#width

instance mediaFeatureWidthLength :: LengthTag a => MediaFeature "width" (Measure a)
instance mediaFeatureMinWidthLength :: LengthTag a => MediaFeature "minWidth" (Measure a)
instance mediaFeatureMaxWidthLength :: LengthTag a => MediaFeature "maxWidth" (Measure a)

-- https://www.w3.org/TR/mediaqueries-3/#height

instance mediaFeatureHeightLength :: LengthTag a => MediaFeature "height" (Measure a)
instance mediaFeatureMinHeightLength :: LengthTag a => MediaFeature "minHeight" (Measure a)
instance mediaFeatureMaxHeightLength :: LengthTag a => MediaFeature "maxHeight" (Measure a)

-- https://www.w3.org/TR/mediaqueries-3/#device-width

instance mediaFeatureDeviceWidthLength :: LengthTag a => MediaFeature "deviceWidth" (Measure a)
instance mediaFeatureMinDeviceWidthLength :: LengthTag a => MediaFeature "minDeviceWidth" (Measure a)
instance mediaFeatureMaxDeviceWidthLength :: LengthTag a => MediaFeature "maxDeviceWidth" (Measure a)

-- https://www.w3.org/TR/mediaqueries-3/#device-height

instance mediaFeatureDeviceHeightLength :: LengthTag a => MediaFeature "deviceHeight" (Measure a)
instance mediaFeatureMinDeviceHeightLength :: LengthTag a => MediaFeature "minDeviceHeight" (Measure a)
instance mediaFeatureMaxDeviceHeightLength :: LengthTag a => MediaFeature "maxDeviceHeight" (Measure a)

-- https://www.w3.org/TR/mediaqueries-3/#orientation

newtype Orientation = Orientation String

derive newtype instance ToVal Orientation

portrait = Orientation "portrait" :: Orientation
landscape = Orientation "landscape" :: Orientation

instance mediaFeatureOrientation :: MediaFeature "orientation" Orientation

-- https://www.w3.org/TR/mediaqueries-3/#aspect-ratio

data Ratio = Ratio Int Int
instance ToVal Ratio where val (Ratio num den) = val num <> val "/" <> val den
infix 5 Ratio as :/

instance mediaFeatureAspectRatio :: MediaFeature "aspectRatio" Ratio
instance mediaFeatureMinAspectRatio :: MediaFeature "minAspectRatio" Ratio
instance mediaFeatureMaxAspectRatio :: MediaFeature "maxAspectRatio" Ratio

-- https://www.w3.org/TR/mediaqueries-3/#device-aspect-ratio

instance mediaFeatureDeviceAspectRatio :: MediaFeature "deviceAspectRatio" Ratio
instance mediaFeatureMinDeviceAspectRatio :: MediaFeature "minDeviceAspectRatio" Ratio
instance mediaFeatureMaxDeviceAspectRatio :: MediaFeature "maxDeviceAspectRatio" Ratio

-- https://www.w3.org/TR/mediaqueries-3/#color

instance mediaFeatureColor :: MediaFeature "color" Int
instance mediaFeatureMinColor :: MediaFeature "minColor" Int
instance mediaFeatureMaxColor :: MediaFeature "maxColor" Int

-- https://www.w3.org/TR/mediaqueries-3/#color-index

instance mediaFeatureColorIndex :: MediaFeature "colorIndex" Int
instance mediaFeatureMinColorIndex :: MediaFeature "minColorIndex" Int
instance mediaFeatureMaxColorIndex :: MediaFeature "maxColorIndex" Int

-- https://www.w3.org/TR/mediaqueries-3/#monochrome

instance mediaFeatureMonochrome :: MediaFeature "monochrome" Int
instance mediaFeatureMinMonochrome :: MediaFeature "minMonochrome" Int
instance mediaFeatureMaxMonochrome :: MediaFeature "maxMonochrome" Int

-- https://www.w3.org/TR/mediaqueries-3/#resolution

newtype Resolution = Resolution String

derive newtype instance ToVal Resolution

dpi :: Int -> Resolution
dpi x = Resolution $ show x <> "dpi"

dpcm :: Int -> Resolution
dpcm x = Resolution $ show x <> "dpcm"

instance mediaFeatureResolution :: MediaFeature "resolution" Resolution
instance mediaFeatureMinResolution :: MediaFeature "minResolution" Resolution
instance mediaFeatureMaxResolution :: MediaFeature "maxResolution" Resolution

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/selectors-4/

closeSelector :: forall tag. Selector tag -> Selector Closed
closeSelector (Selector x) = Selector x

appendSelectorDetail :: Val -> Selector Open -> Selector Open
appendSelectorDetail v (Selector s) = Selector $ s <> v

-- https://www.w3.org/TR/selectors-4/#the-universal-selector

universal = Selector (val "*") :: Selector Open

-- https://www.w3.org/TR/selectors-4/#combinators

combine :: String -> Selector Open -> Selector Open -> Selector Open
combine s (Selector a) (Selector b) =
  Selector $
    Val \c@{ separator } ->
      let
        s' | s == " " = " "
        s' | otherwise = separator <> s <> separator
      in
        runVal c a <> s' <> runVal c b

-- https://www.w3.org/TR/selectors-4/#descendant-combinators

descendant :: Selector Open -> Selector Open -> Selector Open
descendant = combine " "
infixl 1 descendant as |*

-- https://www.w3.org/TR/selectors-4/#child-combinators

child :: Selector Open -> Selector Open -> Selector Open
child = combine ">"
infixl 1 child as |>

-- https://www.w3.org/TR/selectors-4/#adjacent-sibling-combinators

adjacentSibling :: Selector Open -> Selector Open -> Selector Open
adjacentSibling = combine "+"
infixl 1 adjacentSibling as |+

-- https://www.w3.org/TR/selectors-4/#general-sibling-combinators

generalSibling :: Selector Open -> Selector Open -> Selector Open
generalSibling = combine "~"
infixl 1 generalSibling as |~

-- https://www.w3.org/TR/selectors-3/#attribute-selectors

newtype Attribute = Attribute String

att :: String -> Attribute
att = Attribute

derive newtype instance ToVal Attribute

class ToVal a <= IsAttribute (a :: Type)
instance IsAttribute Attribute

attCmp 
  :: forall a
   . IsAttribute a
  => String
  -> a
  -> String
  -> Selector Open
  -> Selector Open
attCmp op att' val' =
  appendSelectorDetail $
    val "[" <> val att' <> val op <> val (quote val') <> val "]"

byAtt :: forall a. IsAttribute a => a -> Selector Open -> Selector Open
byAtt a = appendSelectorDetail $ val "[" <> val a <> val "]"

byAttEq
  :: forall a
   . IsAttribute a
  => a
  -> String
  -> Selector Open
  -> Selector Open
byAttEq = attCmp "="
infixl 5 byAttEq as @=

byAttElemWhitespace
  :: forall a
   . IsAttribute a
  => a
  -> String
  -> Selector Open
  -> Selector Open
byAttElemWhitespace = attCmp "~="
infixl 5 byAttElemWhitespace as ~=

byAttStartsWithHyphen
  :: forall a
   . IsAttribute a
  => a
  -> String
  -> Selector Open
  -> Selector Open
byAttStartsWithHyphen = attCmp "|="
infixl 5 byAttStartsWithHyphen as |=

byAttStartsWith
  :: forall a
   . IsAttribute a
  => a
  -> String
  -> Selector Open
  -> Selector Open
byAttStartsWith = attCmp "^="
infixl 5 byAttStartsWith as ^=

byAttEndsWith
  :: forall a
   . IsAttribute a
  => a
  -> String
  -> Selector Open
  -> Selector Open
byAttEndsWith = attCmp "$="
infixl 5 byAttEndsWith as $=

byAttContains
  :: forall a
   . IsAttribute a
  => a
  -> String
  -> Selector Open
  -> Selector Open
byAttContains = attCmp "*="
infixl 5 byAttContains as *=

-- https://www.w3.org/TR/selectors-3/#class-html

byClass :: String -> Selector Open -> Selector Open
byClass c = appendSelectorDetail (val $ "." <> c)

-- https://www.w3.org/TR/selectors-3/#id-selectors

byId :: String -> Selector Open -> Selector Open
byId i = appendSelectorDetail (val $ "#" <> i)

-- https://www.w3.org/TR/selectors-3/#sel-link

byLink :: Selector Open -> Selector Open
byLink = appendSelectorDetail $ val ":link"

-- https://www.w3.org/TR/selectors-3/#sel-visited

byVisited :: Selector Open -> Selector Open
byVisited = appendSelectorDetail $ val ":visited"

-- https://www.w3.org/TR/selectors-3/#sel-hover

byHover :: Selector Open -> Selector Open
byHover = appendSelectorDetail $ val ":hover"

-- https://www.w3.org/TR/selectors-3/#sel-active

byActive :: Selector Open -> Selector Open
byActive = appendSelectorDetail $ val ":active"

-- https://www.w3.org/TR/selectors-3/#sel-focus

byFocus :: Selector Open -> Selector Open
byFocus = appendSelectorDetail $ val ":focus"

-- https://www.w3.org/TR/selectors-3/#sel-target

byTarget :: Selector Open -> Selector Open
byTarget = appendSelectorDetail $ val ":target"

-- https://www.w3.org/TR/selectors-3/#lang-pseudo

byLang :: String -> Selector Open -> Selector Open
byLang code = appendSelectorDetail $ val ":lang(" <> val code <> val ")"

-- https://www.w3.org/TR/selectors-3/#sel-enabled

byEnabled :: Selector Open -> Selector Open
byEnabled = appendSelectorDetail $ val ":enabled"

-- https://www.w3.org/TR/selectors-3/#sel-disabled

byDisabled :: Selector Open -> Selector Open
byDisabled = appendSelectorDetail $ val ":disabled"

-- https://www.w3.org/TR/selectors-3/#sel-checked

byChecked :: Selector Open -> Selector Open
byChecked = appendSelectorDetail $ val ":checked"

-- https://www.w3.org/TR/selectors-3/#sel-indeterminate

byIndeterminate :: Selector Open -> Selector Open
byIndeterminate = appendSelectorDetail $ val ":indeterminate"

-- https://www.w3.org/TR/selectors-3/#sel-root

byRoot :: Selector Open -> Selector Open
byRoot = appendSelectorDetail $ val ":root"

-- https://www.w3.org/TR/selectors-3/#sel-nth-child

data Nth = Even | Odd | Nth Int Int
instance ToVal Nth where
  val Even = val "even"
  val Odd = val "odd"
  val (Nth a b) =
    Val \{ separator } ->
      let
        an
          | a == -1 = "-n"
          | a == 1 = "n"
          | a /= 0 = show a <> "n"
          | otherwise = mempty
        op
          | a == 0 || b == 0 = mempty
          | b < 0 = separator <> "-" <> separator
          | otherwise = separator <> "+" <> separator
        b'
          | b == 0 = mempty
          | a == 0 = show b
          | otherwise = show $ abs b
      in
        an <> op <> b'

even = Even :: Nth
odd = Odd :: Nth

nth :: Int -> Int -> Nth
nth = Nth

byNthChild :: Nth -> Selector Open -> Selector Open
byNthChild formula =
  appendSelectorDetail $ val ":nth-child(" <> val formula <> val ")"

-- https://www.w3.org/TR/selectors-3/#sel-nth-last-child

byNthLastChild :: Nth -> Selector Open -> Selector Open
byNthLastChild formula =
  appendSelectorDetail $ val ":nth-last-child(" <> val formula <> val ")"

-- https://www.w3.org/TR/selectors-3/#sel-nth-of-type

byNthOfType :: Nth -> Selector Open -> Selector Open
byNthOfType formula =
  appendSelectorDetail $ val ":nth-of-type(" <> val formula <> val ")"

-- https://www.w3.org/TR/selectors-3/#sel-first-child

byFirstChild :: Selector Open -> Selector Open
byFirstChild = appendSelectorDetail $ val ":first-child"

-- https://www.w3.org/TR/selectors-3/#sel-last-child

byLastChild :: Selector Open -> Selector Open
byLastChild = appendSelectorDetail $ val ":last-child"

-- https://www.w3.org/TR/selectors-3/#sel-first-of-type

byFirstOfType :: Selector Open -> Selector Open
byFirstOfType = appendSelectorDetail $ val ":first-of-type"

-- https://www.w3.org/TR/selectors-3/#sel-last-of-type

byLastOfType :: Selector Open -> Selector Open
byLastOfType = appendSelectorDetail $ val ":last-of-type"

-- https://www.w3.org/TR/selectors-3/#sel-only-child

byOnlyChild :: Selector Open -> Selector Open
byOnlyChild = appendSelectorDetail $ val ":only-child"

-- https://www.w3.org/TR/selectors-3/#sel-only-of-type

byOnlyOfType :: Selector Open -> Selector Open
byOnlyOfType = appendSelectorDetail $ val ":only-of-type"

-- https://www.w3.org/TR/selectors-3/#sel-empty

byEmpty :: Selector Open -> Selector Open
byEmpty = appendSelectorDetail $ val ":empty"

-- https://www.w3.org/TR/selectors-4/#negation-pseudo

class ByNot (a :: Type) where
  byNot :: a -> Selector Open -> Selector Open

instance ByNot (Selector Open) where
  byNot (Selector x) = appendSelectorDetail $ val ":not(" <> x <> val ")"

instance ByNot (NonEmpty Array (Selector Open)) where
  byNot selectors =
    let
      s =
        Val \c@{ separator } ->
          String.joinWith ("," <> separator)
            $ Array.fromFoldable
            $ (\(Selector x) -> runVal c x) <$> selectors
    in
      appendSelectorDetail $ val ":not(" <> s <> val ")"

-- https://www.w3.org/TR/selectors-3/#sel-first-line

byFirstLine :: Selector Open -> Selector Closed
byFirstLine = closeSelector <<< appendSelectorDetail (val "::first-line")

-- https://www.w3.org/TR/selectors-3/#sel-first-letter

byFirstLetter :: Selector Open -> Selector Closed
byFirstLetter = closeSelector <<< appendSelectorDetail (val "::first-letter")

-- https://www.w3.org/TR/selectors-3/#sel-before

byBefore :: Selector Open -> Selector Closed
byBefore = closeSelector <<< appendSelectorDetail (val "::before")

-- https://www.w3.org/TR/selectors-3/#sel-after

byAfter :: Selector Open -> Selector Closed
byAfter = closeSelector <<< appendSelectorDetail (val "::after")

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/css-animations-1/

-- https://www.w3.org/TR/css-animations-1/#propdef-animation-name

class IsAnimationName (a :: Type)
instance IsAnimationName None
instance IsAnimationName KeyframesName
instance isAnimationNameMultiple
  :: ( IsAnimationName x
     , IsAnimationName xs
     )
  => IsAnimationName (x /\ xs)

instance propertyAnimationNameVal
  :: ( IsAnimationName a
     , MultiVal a
     )
  => Property "animationName" a where
  pval = const $ joinVals (Val \c -> "," <> c.separator) <<< multiVal

-- https://www.w3.org/TR/css-animations-1/#propdef-animation-duration

class IsAnimationDuration (a :: Type)
instance IsAnimationDuration None
instance TimeTag a => IsAnimationDuration (Measure a)
instance isAnimationDurationMultiple
  :: ( IsAnimationDuration x
     , IsAnimationDuration xs
     )
  => IsAnimationDuration (x /\ xs)

instance propertyAnimationDurationIs
  :: ( IsAnimationDuration a
     , MultiVal a
     )
  => Property "animationDuration" a where
  pval = const $ joinVals (Val \c -> "," <> c.separator) <<< multiVal

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/css-animations-1/#propdef-animation-timing-function

class IsAnimationTimingFunction (a :: Type)
instance IsAnimationTimingFunction EasingFunction
instance isAnimationTimingFunctionMultiple
  :: ( IsAnimationTimingFunction x
     , IsAnimationTimingFunction xs
     )
  => IsAnimationTimingFunction (x /\ xs)

instance propertyAnimationTimingFunctionIs
  :: ( IsAnimationTimingFunction a
     , MultiVal a
     )
  => Property "animationTimingFunction" a where
  pval = const $ joinVals (Val \c -> "," <> c.separator) <<< multiVal

-- https://www.w3.org/TR/css-animations-1/#propdef-animation-iteration-count

class IsAnimationIterationCount (a :: Type)
instance IsAnimationIterationCount Infinite
instance IsAnimationIterationCount Int
instance isAnimationIterationCountMultiple
  :: ( IsAnimationIterationCount x
     , IsAnimationIterationCount xs
     )
  => IsAnimationIterationCount (x /\ xs)

instance propertyAnimationIterationCountIs
  :: ( IsAnimationIterationCount a
     , MultiVal a
     )
  => Property "animationIterationCount" a where
  pval = const $ joinVals (Val \c -> "," <> c.separator) <<< multiVal

-- https://www.w3.org/TR/css-animations-1/#propdef-animation-direction

newtype AnimationDirection = AnimationDirection String

derive newtype instance ToVal AnimationDirection

reverse :: AnimationDirection
reverse = AnimationDirection "reverse"

alternate :: AnimationDirection
alternate = AnimationDirection "alternate"

alternateReverse :: AnimationDirection
alternateReverse = AnimationDirection "alternate-reverse"

class IsAnimationDirection (a :: Type)
instance IsAnimationDirection Normal
instance IsAnimationDirection AnimationDirection
instance isAnimationDirectionMultiple
  :: ( IsAnimationDirection x
     , IsAnimationDirection xs
     )
  => IsAnimationDirection (x /\ xs)

instance propertyAnimationDirectionIs
  :: ( IsAnimationDirection a
     , MultiVal a
     )
  => Property "animationDirection" a where
  pval = const $ joinVals (Val \c -> "," <> c.separator) <<< multiVal

-- https://www.w3.org/TR/css-animations-1/#propdef-animation-play-state

newtype AnimationPlayState = AnimationPlayState String

derive newtype instance ToVal AnimationPlayState

running :: AnimationPlayState
running = AnimationPlayState "running"

paused :: AnimationPlayState
paused = AnimationPlayState "paused"

class IsAnimationPlayState (a :: Type)
instance IsAnimationPlayState AnimationPlayState
instance isAnimationPlayStateMultiple
  :: ( IsAnimationPlayState x
     , IsAnimationPlayState xs
     )
  => IsAnimationPlayState (x /\ xs)

instance propertyAnimationPlayStateIs
  :: ( IsAnimationPlayState a
     , MultiVal a
     )
  => Property "animationPlayState" a where
  pval = const $ joinVals (Val \c -> "," <> c.separator) <<< multiVal

-- https://www.w3.org/TR/css-animations-1/#propdef-animation-delay

instance propertyAnimationDelay
  :: Property "animationDuration" a
  => Property "animationDelay" a where
  pval _ = pval (Proxy :: _ "animationDuration")

-- https://www.w3.org/TR/css-animations-1/#propdef-animation-fill-mode

newtype AnimationFillMode = AnimationFillMode String

derive newtype instance ToVal AnimationFillMode

forwards :: AnimationFillMode
forwards = AnimationFillMode "forwards"

backwards :: AnimationFillMode
backwards = AnimationFillMode "backwards"

class IsAnimationFillMode (a :: Type)
instance IsAnimationFillMode AnimationFillMode
instance IsAnimationFillMode None
instance IsAnimationFillMode Both
instance isAnimationFillModeMultiple
  :: ( IsAnimationFillMode x
     , IsAnimationFillMode xs
     )
  => IsAnimationFillMode (x /\ xs)

instance propertyAnimationFillModeIs
  :: ( IsAnimationFillMode a
     , MultiVal a
     )
  => Property "animationFillMode" a where
  pval = const $ joinVals (Val \c -> "," <> c.separator) <<< multiVal

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/css-backgrounds-3/

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-background-color

instance propertyBackgroundColorColor
  :: ( IsColor a
     , ToVal a
     )
  => Property "backgroundColor" a where
  pval = const val

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-background-image

class IsBackgroundImage (a :: Type)

instance isBackgroundImageMultiple
  :: ( IsBackgroundImage x
     , IsBackgroundImage xs
     )
  => IsBackgroundImage (x /\ xs)
else instance IsBackgroundImage None
else instance IsImage a => IsBackgroundImage a

instance propertyBackgroundImageIs
  :: ( IsBackgroundImage a
     , MultiVal a
     )
  => Property "backgroundImage" a where
  pval = const $ joinVals (Val \c -> "," <> c.separator) <<< multiVal

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-background-repeat

newtype RepeatStyle (vertical :: Type) = RepeatStyle String

derive newtype instance ToVal (RepeatStyle a)

repeatX :: RepeatStyle (RepeatStyle Unit)
repeatX = RepeatStyle "repeat-x"

repeatY :: RepeatStyle (RepeatStyle Unit)
repeatY = RepeatStyle "repeat-y"

repeat :: RepeatStyle Unit
repeat = RepeatStyle "repeat"

space :: RepeatStyle Unit
space = RepeatStyle "space"

round :: RepeatStyle Unit
round = RepeatStyle "round"

noRepeat :: RepeatStyle Unit
noRepeat = RepeatStyle "no-repeat"

class IsBackgroundRepeat (a :: Type)
instance IsBackgroundRepeat (RepeatStyle a)
instance isBackgroundRepeatPair
  :: IsBackgroundRepeat (RepeatStyle Unit ~ RepeatStyle Unit)
instance isBackgroundRepeatMultiple
  :: ( IsBackgroundRepeat x
     , IsBackgroundRepeat xs
     )
  => IsBackgroundRepeat (x /\ xs)

instance propertyBackgroundRepeatIs
  :: ( IsBackgroundRepeat a
     , MultiVal a
     )
  => Property "backgroundRepeat" a where
  pval = const $ joinVals (Val \c -> "," <> c.separator) <<< multiVal

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-background-attachment

class IsBackgroundAttachment (a :: Type)
instance IsBackgroundAttachment Fixed
instance IsBackgroundAttachment Local
instance IsBackgroundAttachment Scroll
instance isBackgroundAttachmentMultiple
  :: ( IsBackgroundAttachment x
     , IsBackgroundAttachment xs
     )
  => IsBackgroundAttachment (x /\ xs)

instance propertyBackgrondAttachmentIs
  :: ( IsBackgroundAttachment a
     , MultiVal a
     )
  => Property "backgroundAttachment" a where
  pval = const $ joinVals (Val \c -> "," <> c.separator) <<< multiVal

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-background-position

class IsBackgroundPosition (a :: Type)
instance isBackgroundPositionMultiple
  :: ( IsPosition x
     , IsBackgroundPosition xs
     )
  => IsBackgroundPosition (x /\ xs)
else instance IsPosition a => IsBackgroundPosition a

instance propertyBackgroundPositionVal
  :: ( IsBackgroundPosition a
     , MultiVal a
     )
  => Property "backgroundPosition" a where
  pval = const $ joinVals (Val \c -> "," <> c.separator) <<< multiVal

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-background-clip

class IsBackgroundClip (a :: Type)
instance IsBackgroundClip BorderBox
instance IsBackgroundClip PaddingBox
instance IsBackgroundClip ContentBox
instance isBackgroundClipMultiple
  :: ( IsBackgroundClip x
     , IsBackgroundClip xs
     )
  => IsBackgroundClip (x /\ xs)

instance propertyBackgroundClipIs
  :: ( IsBackgroundClip a
     , MultiVal a
     )
  => Property "backgroundClip" a where
  pval = const $ joinVals (Val \c -> "," <> c.separator) <<< multiVal

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-background-origin

instance propertyBackgroundOriginBackgroundClip
  :: Property "backgroundClip" a
  => Property "backgroundOrigin" a where
  pval = const $ pval (Proxy :: _ "backgroundClip")

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-background-size

class IsBackgroundSize (a :: Type)
instance IsBackgroundSize Cover
instance IsBackgroundSize Contain
instance IsBackgroundSize Auto
instance LengthPercentageTag a => IsBackgroundSize (Measure a)
instance isBackgroundSizeAutoAuto :: IsBackgroundSize (Auto ~ Auto)
instance isBackgroundSizeAutoLengthPercentage
  :: LengthPercentageTag y
  => IsBackgroundSize (Auto ~ Measure y)
instance isBackgroundSizeLengthPercentageAuto
  :: LengthPercentageTag x
  => IsBackgroundSize (Measure x ~ Auto)
instance isBackgroundSizeLengthPercentageLengthPercentage
  :: ( LengthPercentageTag x
     , LengthPercentageTag y
     )
  => IsBackgroundSize (Measure x ~ Measure y)
instance isBackgroundSizeMultiple
  :: ( IsBackgroundSize x
     , IsBackgroundSize xs
     )
  => IsBackgroundSize (x /\ xs)

instance propertyBackgroundSizeVal
  :: ( IsBackgroundSize a
     , MultiVal a
     )
  => Property "backgroundSize" a where
  pval = const $ joinVals (Val \c -> "," <> c.separator) <<< multiVal

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-top-color

instance propertyBorderTopColorColor
  :: ( IsColor a
     , ToVal a
     )
  => Property "borderTopColor" a where
  pval = const val

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-right-color

instance propertyBorderRightColorBorderTopColor
  :: Property "borderTopColor" a
  => Property "borderRightColor" a where
  pval = const $ pval (Proxy :: _ "borderTopColor")

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-bottom-color

instance propertyBorderBottomColorBorderTopColor
  :: Property "borderTopColor" a
  => Property "borderBottomColor" a where
  pval = const $ pval (Proxy :: _ "borderTopColor")

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-left-color

instance propertyBorderLeftColorBorderTopColor
  :: Property "borderTopColor" a
  => Property "borderLeftColor" a where
  pval = const $ pval (Proxy :: _ "borderTopColor")

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-color

instance propertyBorderColor4
  :: ( IsColor t
     , ToVal t
     , IsColor r
     , ToVal r
     , IsColor b
     , ToVal b
     , IsColor l
     , ToVal l
     )
  => Property "borderColor" (t ~ r ~ b ~ l) where
  pval = const val

else instance propertyBorderColor3
  :: ( IsColor t
     , ToVal t
     , IsColor x
     , ToVal x
     , IsColor b
     , ToVal b
     )
  => Property "borderColor" (t ~ x ~ b) where
  pval = const val

else instance propertyBorderColor2
  :: ( IsColor y
     , ToVal y
     , IsColor x
     , ToVal x
     )
  => Property "borderColor" (y ~ x) where
  pval = const val

else instance propertyBorderColor1
  :: ( IsColor a
     , ToVal a
     )
  => Property "borderColor" a where
  pval = const val

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-top-style

newtype LineStyle = LineStyle String

derive newtype instance ToVal LineStyle

groove :: LineStyle
groove = LineStyle "groove"

ridge :: LineStyle
ridge = LineStyle "ridge"

outset :: LineStyle
outset = LineStyle "outset"

class IsBorderTopStyle (a :: Type)

instance IsBorderTopStyle None
instance IsBorderTopStyle Dashed
instance IsBorderTopStyle Dotted
instance IsBorderTopStyle Double
instance IsBorderTopStyle Hidden
instance IsBorderTopStyle Inset
instance IsBorderTopStyle Solid
instance IsBorderTopStyle LineStyle

instance propertyBorderTopStyleIs
  :: ( IsBorderTopStyle a
     , ToVal a
     )
  => Property "borderTopStyle" a where
  pval = const val

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-right-style

instance propertyBorderRightStyleBorderTopStyle
  :: Property "borderTopStyle" a
  => Property "borderRightStyle" a where
  pval = const $ pval (Proxy :: _ "borderTopStyle")

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-bottom-style

instance propertyBorderBottomStyleBorderTopStyle
  :: Property "borderTopStyle" a
  => Property "borderBottomStyle" a where
  pval = const $ pval (Proxy :: _ "borderTopStyle")

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-left-style

instance propertyBorderLeftStyleBorderTopStyle
  :: Property "borderTopStyle" a
  => Property "borderLeftStyle" a where
  pval = const $ pval (Proxy :: _ "borderTopStyle")

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-style

instance propertyBorderStyle4
  :: ( IsBorderTopStyle t
     , ToVal t
     , IsBorderTopStyle r
     , ToVal r
     , IsBorderTopStyle b
     , ToVal b
     , IsBorderTopStyle l
     , ToVal l
     )
  => Property "borderStyle" (t ~ r ~ b ~ l) where
  pval = const val

else instance propertyBorderStyle3
  :: ( IsBorderTopStyle t
     , ToVal t
     , IsBorderTopStyle x
     , ToVal x
     , IsBorderTopStyle b
     , ToVal b
     )
  => Property "borderStyle" (t ~ x ~ b) where
  pval = const val

else instance propertyBorderStyle2
  :: ( IsBorderTopStyle y
     , ToVal y
     , IsBorderTopStyle x
     , ToVal x
     )
  => Property "borderStyle" (y ~ x) where
  pval = const val

else instance propertyBorderStyle1
  :: ( IsBorderTopStyle a
     , ToVal a
     )
  => Property "borderStyle" a where
  pval = const val

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-top-width

newtype LineWidth = LineWidth String

derive newtype instance ToVal LineWidth

thin :: LineWidth
thin = LineWidth "thin"

thick :: LineWidth
thick = LineWidth "thick"

class IsBorderTopWidth (a :: Type)
instance LengthTag a => IsBorderTopWidth (Measure a)
instance IsBorderTopWidth LineWidth
instance IsBorderTopWidth Medium

instance propertyBorderTopWidthIs
  :: ( IsBorderTopWidth a
     , ToVal a
     )
  => Property "borderTopWidth" a where
  pval = const val

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-right-width

instance propertyBorderRightWidthBorderTopWidth
  :: Property "borderTopWidth" a
  => Property "borderRightWidth" a where
  pval = const $ pval (Proxy :: _ "borderTopWidth")

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-bottom-width

instance propertyBorderBottomWidthBorderTopWidth
  :: Property "borderTopWidth" a
  => Property "borderBottomWidth" a where
  pval = const $ pval (Proxy :: _ "borderTopWidth")

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-left-width

instance propertyBorderLeftWidthBorderTopWidth
  :: Property "borderTopWidth" a
  => Property "borderLeftWidth" a where
  pval = const $ pval (Proxy :: _ "borderTopWidth")

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-width

instance propertyBorderWidth4
  :: ( IsBorderTopWidth t
     , ToVal t
     , IsBorderTopWidth r
     , ToVal r
     , IsBorderTopWidth b
     , ToVal b
     , IsBorderTopWidth l
     , ToVal l
     )
  => Property "borderWidth" (t ~ r ~ b ~ l) where
  pval = const val

else instance propertyBorderWidth3
  :: ( IsBorderTopWidth t
     , ToVal t
     , IsBorderTopWidth x
     , ToVal x
     , IsBorderTopWidth b
     , ToVal b
     )
  => Property "borderWidth" (t ~ x ~ b) where
  pval = const val

else instance propertyBorderWidth2
  :: ( IsBorderTopWidth y
     , ToVal y
     , IsBorderTopWidth x
     , ToVal x
     )
  => Property "borderWidth" (y ~ x) where
  pval = const val

else instance propertyBorderWidthBorderTopWidth
  :: ( IsBorderTopWidth a
     , ToVal a
     )
  => Property "borderWidth" a where
  pval = const val

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-top-left-radius

instance propertyBorderTopLeftRadius2
  :: ( LengthPercentageTag x
     , LengthPercentageTag y
     )
  => Property "borderTopLeftRadius" (Measure x ~ Measure y) where
  pval = const val

instance propertyBorderTopLeftRadius1
  :: LengthPercentageTag a
  => Property "borderTopLeftRadius" (Measure a) where
  pval = const val

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-top-right-radius

instance propertyBorderTopRightRadiusBorderTopLeftRadius
  :: Property "borderTopLeftRadius" a
  => Property "borderTopRightRadius" a where
  pval = const $ pval (Proxy :: _ "borderTopLeftRadius")

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-bottom-right-radius

instance propertyBorderBottomRightRadiusBorderTopLeftRadius
  :: Property "borderTopLeftRadius" a
  => Property "borderBottomRightRadius" a where
  pval = const $ pval (Proxy :: _ "borderTopLeftRadius")

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-bottom-left-radius

instance propertyBorderBottomLeftRadiusBorderTopLeftRadius
  :: Property "borderTopLeftRadius" a
  => Property "borderBottomLeftRadius" a where
  pval = const $ pval (Proxy :: _ "borderTopLeftRadius")

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-radius

mkBorderRadius :: forall x y. ToVal x => ToVal y => (x /\ y) -> Val
mkBorderRadius (x /\ y) =
  Val \c@{ separator } ->
    let
      x' = runVal c $ val x
      y' = runVal c $ val y
    in
      if y' == mempty
        then x'
        else x' <> separator <> "/" <> separator <> y'

class IsBorderRadius (a :: Type)
instance isBorderRadius4X4Y
  :: ( LengthPercentageTag tlx
     , LengthPercentageTag trx
     , LengthPercentageTag brx
     , LengthPercentageTag blx
     , LengthPercentageTag tly
     , LengthPercentageTag try
     , LengthPercentageTag bry
     , LengthPercentageTag bly
     )
  => IsBorderRadius (Measure tlx ~ Measure trx ~ Measure brx ~ Measure blx /\ Measure tly ~ Measure try ~ Measure bry ~ Measure bly)
instance isBorderRadius4X3Y
  :: ( LengthPercentageTag tlx
     , LengthPercentageTag trx
     , LengthPercentageTag brx
     , LengthPercentageTag blx
     , LengthPercentageTag tly
     , LengthPercentageTag trbly
     , LengthPercentageTag bry
     )
  => IsBorderRadius (Measure tlx ~ Measure trx ~ Measure brx ~ Measure blx /\ Measure tly ~ Measure trbly ~ Measure bry)
instance isBorderRadius4X2Y
  :: ( LengthPercentageTag tlx
     , LengthPercentageTag trx
     , LengthPercentageTag brx
     , LengthPercentageTag blx
     , LengthPercentageTag tlbry
     , LengthPercentageTag trbly
     )
  => IsBorderRadius (Measure tlx ~ Measure trx ~ Measure brx ~ Measure blx /\ Measure tlbry ~ Measure trbly)
instance isBorderRadius4X1Y
  :: ( LengthPercentageTag tlx
     , LengthPercentageTag trx
     , LengthPercentageTag brx
     , LengthPercentageTag blx
     , LengthPercentageTag y
     )
  => IsBorderRadius (Measure tlx ~ Measure trx ~ Measure brx ~ Measure blx /\ Measure y)
instance isBorderRadius4X0Y
  :: ( LengthPercentageTag tlx
     , LengthPercentageTag trx
     , LengthPercentageTag brx
     , LengthPercentageTag blx
     )
  => IsBorderRadius (Measure tlx ~ Measure trx ~ Measure brx ~ Measure blx)
instance isBorderRadius3X4Y
  :: ( LengthPercentageTag tlx
     , LengthPercentageTag trblx
     , LengthPercentageTag brx
     , LengthPercentageTag tly
     , LengthPercentageTag try
     , LengthPercentageTag bry
     , LengthPercentageTag bly
     )
  => IsBorderRadius (Measure tlx ~ Measure trblx ~ Measure brx /\ Measure tly ~ Measure try ~ Measure bry ~ Measure bly)
instance isBorderRadius3X3Y
  :: ( LengthPercentageTag tlx
     , LengthPercentageTag trblx
     , LengthPercentageTag brx
     , LengthPercentageTag tly
     , LengthPercentageTag trbly
     , LengthPercentageTag bry
     )
  => IsBorderRadius (Measure tlx ~ Measure trblx ~ Measure brx /\ Measure tly ~ Measure trbly ~ Measure bry)
instance isBorderRadius3X2Y
  :: ( LengthPercentageTag tlx
     , LengthPercentageTag trblx
     , LengthPercentageTag brx
     , LengthPercentageTag tlbry
     , LengthPercentageTag trbly
     )
  => IsBorderRadius (Measure tlx ~ Measure trblx ~ Measure brx /\ Measure tlbry ~ Measure trbly)
instance isBorderRadius3X1Y
  :: ( LengthPercentageTag tlx
     , LengthPercentageTag trblx
     , LengthPercentageTag brx
     , LengthPercentageTag y
     )
  => IsBorderRadius (Measure tlx ~ Measure trblx ~ Measure brx /\ Measure y)
instance isBorderRadius3X0Y
  :: ( LengthPercentageTag tlx
     , LengthPercentageTag trblx
     , LengthPercentageTag brx
     )
  => IsBorderRadius (Measure tlx ~ Measure trblx ~ Measure brx)
instance isBorderRadius2X4Y
  :: ( LengthPercentageTag tlbrx
     , LengthPercentageTag trblx
     , LengthPercentageTag tly
     , LengthPercentageTag try
     , LengthPercentageTag bry
     , LengthPercentageTag bly
     )
  => IsBorderRadius (Measure tlbrx ~ Measure trblx /\ Measure tly ~ Measure try ~ Measure bry ~ Measure bly)
instance isBorderRadius2X3Y
  :: ( LengthPercentageTag tlbrx
     , LengthPercentageTag trblx
     , LengthPercentageTag tly
     , LengthPercentageTag trbly
     , LengthPercentageTag bry
     )
  => IsBorderRadius (Measure tlbrx ~ Measure trblx /\ Measure tly ~ Measure trbly ~ Measure bry)
instance isBorderRadius2X2Y
  :: ( LengthPercentageTag tlbrx
     , LengthPercentageTag trblx
     , LengthPercentageTag tlbry
     , LengthPercentageTag trbly
     )
  => IsBorderRadius (Measure tlbrx ~ Measure trblx /\ Measure tlbry ~ Measure trbly)
instance isBorderRadius2X1Y
  :: ( LengthPercentageTag tlbrx
     , LengthPercentageTag trblx
     , LengthPercentageTag y
     )
  => IsBorderRadius (Measure tlbrx ~ Measure trblx /\ Measure y)
instance isBorderRadius2X0Y
  :: ( LengthPercentageTag tlbrx
     , LengthPercentageTag trblx
     )
  => IsBorderRadius (Measure tlbrx ~ Measure trblx)
instance isBorderRadius1X4Y
  :: ( LengthPercentageTag x
     , LengthPercentageTag tly
     , LengthPercentageTag try
     , LengthPercentageTag bry
     , LengthPercentageTag bly
     )
  => IsBorderRadius (Measure x /\ Measure tly ~ Measure try ~ Measure bry ~ Measure bly)
instance isBorderRadius1X3Y
  :: ( LengthPercentageTag x
     , LengthPercentageTag tly
     , LengthPercentageTag trbly
     , LengthPercentageTag bry
     )
  => IsBorderRadius (Measure x /\ Measure tly ~ Measure trbly ~ Measure bry)
instance isBorderRadius1X2Y
  :: ( LengthPercentageTag x
     , LengthPercentageTag tlbry
     , LengthPercentageTag trbly
     )
  => IsBorderRadius (Measure x /\ Measure tlbry ~ Measure trbly)
instance isBorderRadius1X1Y
  :: ( LengthPercentageTag x
     , LengthPercentageTag y
     )
  => IsBorderRadius (Measure x /\ Measure y)
instance isBorderRadius1X0Y
  :: LengthPercentageTag x
  => IsBorderRadius (Measure x)

instance propertyBorderRadiusIs
  :: ( IsBorderRadius a
     , MultiVal a
     )
  => Property "borderRadius" a where
  pval =
    const $ joinVals (Val \c -> c.separator <> "/" <> c.separator) <<< multiVal

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-box-shadow

newtype Shadow = Shadow Val

derive newtype instance ToVal Shadow

class IsShadow (a :: Type)

instance isShadowColor4dInset
  :: ( IsColor color
     , LengthTag xo
     , LengthTag yo
     , LengthTag blur
     , LengthTag spread
     )
  => IsShadow (color ~ Measure xo ~ Measure yo ~ Measure blur ~ Measure spread ~ Inset)

instance isShadowColor4d
  :: ( IsColor color
     , LengthTag xo
     , LengthTag yo
     , LengthTag blur
     , LengthTag spread
     )
  => IsShadow (color ~ Measure xo ~ Measure yo ~ Measure blur ~ Measure spread)

instance isShadow4dInset
  :: ( LengthTag xo
     , LengthTag yo
     , LengthTag blur
     , LengthTag spread
     )
  => IsShadow (Measure xo ~ Measure yo ~ Measure blur ~ Measure spread ~ Inset)

else instance isShadowColor3dInset
  :: ( IsColor color
     , LengthTag xo
     , LengthTag yo
     , LengthTag blur
     )
  => IsShadow (color ~ Measure xo ~ Measure yo ~ Measure blur ~ Inset)

instance isShadow4d
  :: ( LengthTag xo
     , LengthTag yo
     , LengthTag blur
     , LengthTag spread
     )
  => IsShadow (Measure xo ~ Measure yo ~ Measure blur ~ Measure spread)

else instance isShadowColor3d
  :: ( IsColor color
     , LengthTag xo
     , LengthTag yo
     , LengthTag blur
     )
  => IsShadow (color ~ Measure xo ~ Measure yo ~ Measure blur)

instance isShadow3dInset
  :: ( LengthTag xo
     , LengthTag yo
     , LengthTag blur
     )
  => IsShadow (Measure xo ~ Measure yo ~ Measure blur ~ Inset)

else instance isShadowColor2dInset
  :: ( IsColor color
     , LengthTag xo
     , LengthTag yo
     )
  => IsShadow (color ~ Measure xo ~ Measure yo ~ Inset)

instance isShadow3d
  :: ( LengthTag xo
     , LengthTag yo
     , LengthTag blur
     )
  => IsShadow (Measure xo ~ Measure yo ~ Measure blur)

else instance isShadow2dInset
  :: ( LengthTag xo
     , LengthTag yo
     )
  => IsShadow (Measure xo ~ Measure yo ~ Inset)

else instance isShadowColor2d
  :: ( IsColor color
     , LengthTag xo
     , LengthTag yo
     )
  => IsShadow (color ~ Measure xo ~ Measure yo)

instance isShadow2d
  :: ( LengthTag xo
     , LengthTag yo
     )
  => IsShadow (Measure xo ~ Measure yo)

instance isShadowMultiple
  :: ( IsShadow x
     , IsShadow xs
     )
  => IsShadow (x /\ xs)

instance propertyBoxShadowNone
  :: Property "boxShadow" None where
  pval = const val

else instance propertyBoxShadowIsShadow
  :: ( IsShadow a
     , MultiVal a
     )
  => Property "boxShadow" a where
  pval = const $ joinVals (Val \c -> "," <> c.separator) <<< multiVal

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/css-box-3/

-- https://www.w3.org/TR/css-box-3/#propdef-margin-top

class IsMarginTop (a :: Type)
instance LengthPercentageTag a => IsMarginTop (Measure a)
instance IsMarginTop Auto

instance propertyMarginTopIs
  :: ( IsMarginTop a
     , ToVal a
     )
  => Property "marginTop" a where
  pval = const val

-- https://www.w3.org/TR/css-box-3/#propdef-margin-right

instance propertyMarginRightMarginTop
  :: Property "marginTop" a
  => Property "marginRight" a where
  pval = const $ pval (Proxy :: _ "marginTop")

-- https://www.w3.org/TR/css-box-3/#propdef-margin-bottom

instance propertyMarginBottomMarginTop
  :: Property "marginTop" a
  => Property "marginBottom" a where
  pval = const $ pval (Proxy :: _ "marginTop")

-- https://www.w3.org/TR/css-box-3/#propdef-margin-left

instance propertyMarginLeftMarginTop
  :: Property "marginTop" a
  => Property "marginLeft" a where
  pval = const $ pval (Proxy :: _ "marginTop")

-- https://www.w3.org/TR/css-box-3/#propdef-margin

instance propertyMarginIsMarginTop4
  :: ( IsMarginTop t
     , ToVal t
     , IsMarginTop r
     , ToVal r
     , IsMarginTop b
     , ToVal b
     , IsMarginTop l
     , ToVal l
     )
  => Property "margin" (t ~ r ~ b ~ l) where
  pval = const val

else instance propertyMarginIsMarginTop3
  :: ( IsMarginTop t
     , ToVal t
     , IsMarginTop x
     , ToVal x
     , IsMarginTop b
     , ToVal b
     )
  => Property "margin" (t ~ x ~ b) where
  pval = const val

else instance propertyMarginIsMarginTop2
  :: ( IsMarginTop y
     , ToVal y
     , IsMarginTop x
     , ToVal x
     )
  => Property "margin" (y ~ x) where
  pval = const val

else instance propertyMarginMarginTop
  :: Property "marginTop" a
  => Property "margin" a where
  pval = const $ pval (Proxy :: _ "marginTop")

-- https://www.w3.org/TR/css-box-3/#propdef-padding-top

instance propertyPaddingTopLengthPercentage
  :: LengthPercentageTag a
  => Property "paddingTop" (Measure a) where
  pval = const val

-- https://www.w3.org/TR/css-box-3/#propdef-padding-right

instance propertyPaddingRightPaddingTop
  :: Property "paddingTop" a
  => Property "paddingRight" a where
  pval = const $ pval (Proxy :: _ "paddingTop")

-- https://www.w3.org/TR/css-box-3/#propdef-padding-bottom

instance propertyPaddingBottomPaddingTop
  :: Property "paddingTop" a
  => Property "paddingBottom" a where
  pval = const $ pval (Proxy :: _ "paddingTop")

-- https://www.w3.org/TR/css-box-3/#propdef-padding-left

instance propertyPaddingLeftPaddingTop
  :: Property "paddingTop" a
  => Property "paddingLeft" a where
  pval = const $ pval (Proxy :: _ "paddingTop")

-- https://www.w3.org/TR/css-box-3/#propdef-padding

instance paddingLengthPercentage4
  :: ( LengthPercentageTag t
     , LengthPercentageTag r
     , LengthPercentageTag b
     , LengthPercentageTag l
     )
  => Property "padding" (Measure t ~ Measure r ~ Measure b ~ Measure l) where
  pval = const val

else instance paddingLengthPercentage3
  :: ( LengthPercentageTag t
     , LengthPercentageTag x
     , LengthPercentageTag b
     )
  => Property "padding" (Measure t ~ Measure x ~ Measure b) where
  pval = const val

else instance paddingLengthPercentage2
  :: ( LengthPercentageTag y
     , LengthPercentageTag x
     )
  => Property "padding" (Measure y ~ Measure x) where
  pval = const val

else instance paddingPaddingTop
  :: Property "paddingTop" a
  => Property "padding" a where
  pval = const $ pval (Proxy :: _ "paddingTop")

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/css-color-4/

-- https://www.w3.org/TR/css-color-4/#propdef-color

instance propertyColorIsColor
  :: ( IsColor a
     , ToVal a
     )
  => Property "color" a where
  pval = const val

-- https://www.w3.org/TR/css-color-4/#propdef-opacity

instance propertyOpacityNumber :: Property "opacity" Number where
  pval = const val

-- https://www.w3.org/TR/css-color-4/#typedef-color

newtype CSSColor = CSSColor String

derive newtype instance ToVal CSSColor

currentColor :: CSSColor
currentColor = CSSColor "currentColor"

transparent :: CSSColor
transparent = CSSColor "transparent"

instance ToVal Color where val c = Val \cfg -> cfg.color c

class IsColor (a :: Type)
instance IsColor Color
instance IsColor CSSColor

--------------------------------------------------------------------------------

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
  :: forall y1 y2
   . ToNumber y1
  => ToNumber y2
  => Number
  -> y1
  -> Number
  -> y2
  -> EasingFunction
cubicBezier x1 y1 x2 y2 =
  EasingFunction $
    fn "cubic-bezier" [val x1, val $ toNumber y1, val x2, val $ toNumber y2]
     
-- https://www.w3.org/TR/css-easing-1/#typedef-step-position

newtype StepPosition = StepPosition String

derive newtype instance ToVal StepPosition

jumpStart :: StepPosition
jumpStart = StepPosition "jump-start"

jumpEnd :: StepPosition
jumpEnd = StepPosition "jump-end"

jumpNone :: StepPosition
jumpNone = StepPosition "jump-none"

jumpBoth :: StepPosition
jumpBoth = StepPosition "jump-both"

class ToVal a <= IsStepPosition (a :: Type)
instance IsStepPosition StepPosition
instance IsStepPosition Start
instance IsStepPosition End

steps :: forall a. IsStepPosition a => Int -> a -> EasingFunction
steps n f = EasingFunction $ fn "steps" [val n, val f]

stepStart :: EasingFunction
stepStart = EasingFunction $ val "step-start"

stepEnd :: EasingFunction
stepEnd = EasingFunction $ val "step-end"

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/css-flexbox-1/

-- https://www.w3.org/TR/css-flexbox-1/#propdef-flex-direction

newtype FlexDirection = FlexDirection String

derive newtype instance ToVal FlexDirection

row :: FlexDirection
row = FlexDirection "row"

rowReverse :: FlexDirection
rowReverse = FlexDirection "row-reverse"

column :: FlexDirection
column = FlexDirection "column"

columnReverse :: FlexDirection
columnReverse = FlexDirection "column-reverse"

instance propertyFlexDirectionFlexDirection
  :: Property "flexDirection" FlexDirection where
  pval = const val

-- https://www.w3.org/TR/css-flexbox-1/#propdef-flex-wrap

class IsFlexWrap (a :: Type)
instance IsFlexWrap Nowrap
instance IsFlexWrap Wrap
instance IsFlexWrap WrapReverse

instance propertyFlexWrapIs
  :: ( IsFlexWrap a
     , ToVal a
     )
  => Property "flexWrap" a where
  pval = const val

-- https://www.w3.org/TR/css-flexbox-1/#propdef-order

instance propertyOrderInt
  :: Property "order" Int where
  pval = const val

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/css-images-3/

-- https://www.w3.org/TR/css-images-3/#typedef-image

class IsImage (a :: Type)
instance IsImage URL

class IsColorStopListHead (a :: Type)
instance (IsColor color, LengthPercentageTag pos) => IsColorStopListHead (color ~ Measure pos)
else instance IsColor color => IsColorStopListHead color

class IsColorStopListTail (a :: Type)
instance (LengthPercentageTag hint, IsColor color, LengthPercentageTag pos, IsColorStopListTail tail) => IsColorStopListTail (Measure hint /\ color ~ Measure pos /\ tail)
else instance (LengthPercentageTag hint, IsColor color, IsColorStopListTail tail) => IsColorStopListTail (Measure hint /\ color /\ tail)
else instance (IsColor color, LengthPercentageTag pos, IsColorStopListTail tail) => IsColorStopListTail (color ~ Measure pos /\ tail)
else instance (LengthPercentageTag hint, IsColor color, LengthPercentageTag pos) => IsColorStopListTail (Measure hint /\ color ~ Measure pos)
else instance (LengthPercentageTag hint, IsColor color) => IsColorStopListTail (Measure hint /\ color)
else instance (IsColor color, LengthPercentageTag pos) => IsColorStopListTail (color ~ Measure pos)
else instance (IsColor color, IsColorStopListTail tail) => IsColorStopListTail (color /\ tail)
else instance IsColor color => IsColorStopListTail color

newtype Gradient (repeating :: Type) = Gradient (String /\ Array Val)

instance IsImage (Gradient a)

instance ToVal (Gradient a) where
  val (Gradient (ty /\ details)) = fn (ty <> "-gradient") details

data Repeating

repeating :: Gradient Unit -> Gradient Repeating
repeating (Gradient (ty /\ details)) =
  Gradient $ ("repeating-" <> ty) /\ details

-- https://www.w3.org/TR/css-images-3/#linear-gradient-syntax

linearGradient
  :: forall a csh cst
   . AngleTag a
  => IsColorStopListHead csh
  => IsColorStopListTail cst
  => MultiVal (csh /\ cst)
  => Measure a
  -> (csh /\ cst)
  -> Gradient Unit
linearGradient angle colorStops =
  Gradient $ "linear" /\ Array.cons (val angle) (multiVal colorStops)

-- https://www.w3.org/TR/css-images-3/#radial-gradient-syntax

newtype Extent = Extent String

derive newtype instance ToVal Extent

closestCorner :: Extent
closestCorner = Extent "closest-corner"

closestSide :: Extent
closestSide = Extent "closest-side"

farthestCorner :: Extent
farthestCorner = Extent "farthest-corner"

farthestSide :: Extent
farthestSide = Extent "farthest-side"

class IsRadialGradientDimensions (a :: Type)
instance IsRadialGradientDimensions Circle
instance IsRadialGradientDimensions Ellipse
instance IsRadialGradientDimensions (Circle ~ Extent)
instance IsRadialGradientDimensions (Ellipse ~ Extent)
instance LengthTag a => IsRadialGradientDimensions (Measure a)
instance isRadialGradientDimensionsPair
  :: ( LengthPercentageTag x
     , LengthPercentageTag y
     )
  => IsRadialGradientDimensions (Measure x ~ Measure y)

radialGradient
  :: forall dimensions position csh cst
   . IsRadialGradientDimensions dimensions
  => ToVal dimensions
  => IsPosition position
  => ToVal position
  => IsColorStopListHead csh
  => IsColorStopListTail cst
  => MultiVal (csh /\ cst)
  => dimensions
  -> position
  -> (csh /\ cst)
  -> Gradient Unit
radialGradient dimensions position colorStops =
  let
    details =
      Array.cons
        (val dimensions <> val " at " <> val position)
        (multiVal colorStops)
  in
    Gradient $ "radial" /\ details

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/css-overflow-3/

-- https://www.w3.org/TR/css-overflow-3/#propdef-overflow-x

class IsOverflow (a :: Type)
instance IsOverflow Visible
instance IsOverflow Hidden
instance IsOverflow Clip
instance IsOverflow Scroll
instance IsOverflow Auto

instance propertyOverflowXIsOverflow
  :: ( IsOverflow a
     , ToVal a
     )
  => Property "overflowX" a where
  pval = const val

-- https://www.w3.org/TR/css-overflow-3/#propdef-overflow-y

instance propertyOverflowYOverflowX
  :: Property "overflowX" a
  => Property "overflowY" a where
  pval = const $ pval (Proxy :: _ "overflowX")

-- https://www.w3.org/TR/css-overflow-3/#propdef-overflow

instance propertyOverflowIsOverflowPair
  :: ( IsOverflow x
     , ToVal x
     , IsOverflow y
     , ToVal y
     )
  => Property "overflow" (x ~ y) where
  pval = const val

else instance propertyOverflowOverflowX
  :: Property "overflowX" a
  => Property "overflow" a where
  pval = const $ pval (Proxy :: _ "overflowX")

-- https://www.w3.org/TR/css-overflow-3/#propdef-text-overflow

class IsTextOverflow (a :: Type)
instance IsTextOverflow Clip
instance IsTextOverflow Ellipsis

instance propertyTextOverflowIs
  :: ( IsTextOverflow a
     , ToVal a
     )
  => Property "textOverflow" a where
  pval = const val

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/css-sizing-3/

-- https://www.w3.org/TR/css-sizing-3/#propdef-width

instance propertyWidthAuto :: Property "width" Auto where
  pval = const val

instance propertyWidthLengthPercentage
  :: LengthPercentageTag a
  => Property "width" (Measure a) where
  pval = const val

instance propertyWidthContentSizingValue
  :: Property "width" ContentSizingValue where
  pval = const val

-- https://www.w3.org/TR/css-sizing-3/#propdef-height

instance propertyHeightWidth
  :: Property "width" a
  => Property "height" a where
  pval = const $ pval (Proxy :: _ "width")

-- https://www.w3.org/TR/css-sizing-3/#propdef-min-height

instance propertyMinHeightWidth
  :: Property "width" a
  => Property "minHeight" a where
  pval = const $ pval (Proxy :: _ "width")

-- https://www.w3.org/TR/css-sizing-3/#propdef-min-width

instance propertyMinWidthWidth
  :: Property "width" a
  => Property "minWidth" a where
  pval = const $ pval (Proxy :: _ "width")

-- https://www.w3.org/TR/css-sizing-3/#propdef-max-width

instance propertyMaxWidthNone :: Property "maxWidth" None where
  pval = const val

instance propertyMaxWidthLengthPercentage
  :: LengthPercentageTag a
  => Property "maxWidth" (Measure a) where
  pval = const val

instance propertyMaxWidthContentSizingValue
  :: Property "maxWidth" ContentSizingValue where
  pval = const val

-- https://www.w3.org/TR/css-sizing-3/#propdef-max-height

instance propertyMaxHeightMaxWidth
  :: Property "maxWidth" a
  => Property "maxHeight" a where
  pval = const $ pval (Proxy :: _ "maxWidth")

-- https://www.w3.org/TR/css-sizing-3/#sizing-values

newtype ContentSizingValue = ContentSizingValue Val

derive newtype instance ToVal ContentSizingValue

minContent :: ContentSizingValue
minContent = ContentSizingValue $ val "min-content"

maxContent :: ContentSizingValue
maxContent = ContentSizingValue $ val "max-content"

fitContent :: forall a. LengthPercentageTag a => Measure a -> ContentSizingValue
fitContent a = ContentSizingValue $ fn "fit-content" [val a]

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/css-text-4/

-- https://www.w3.org/TR/css-text-4/#text-transform-property

newtype TextTransform = TextTransform String

derive newtype instance ToVal TextTransform

capitalize :: TextTransform
capitalize = TextTransform "capitalize"

uppercase :: TextTransform
uppercase = TextTransform "uppercase"

lowercase :: TextTransform
lowercase = TextTransform "lowercase"

instance propertyTextTransformNone
  :: Property "textTransform" None where
  pval = const val

instance propertyTextTransformTextTransform
  :: Property "textTransform" TextTransform where
  pval = const val

instance propertyTextTransformFullWidth
  :: Property "textTransform" FullWidth where
  pval = const val

instance propertyTextTransformFullSizeKana
  :: Property "textTransform" FullSizeKana where
  pval = const val

instance propertyTextTransformTextTransformFullSizeKana
  :: Property "textTransform" (TextTransform ~ FullSizeKana) where
  pval = const val

instance propertyTextTransformTextTransformFullWidth
  :: Property "textTransform" (TextTransform ~ FullWidth) where
  pval = const val

instance propertyTextTransformFullWidthFullSizeKana
  :: Property "textTransform" (FullWidth ~ FullSizeKana) where
  pval = const val

instance propertyTextTransformTextTransformFullWidthFullSizeKana
  :: Property "textTransform" (TextTransform ~ FullWidth ~ FullSizeKana) where
  pval = const val

-- https://www.w3.org/TR/css-text-4/#propdef-white-space

newtype WhiteSpace = WhiteSpace String

derive newtype instance ToVal WhiteSpace

pre :: WhiteSpace
pre = WhiteSpace "pre"

preWrap :: WhiteSpace
preWrap = WhiteSpace "pre-wrap"

breakSpaces :: WhiteSpace
breakSpaces = WhiteSpace "break-spaces"

preLine :: WhiteSpace
preLine = WhiteSpace "pre-line"

class IsWhiteSpace (a :: Type)
instance IsWhiteSpace WhiteSpace
instance IsWhiteSpace Normal
instance IsWhiteSpace Nowrap

instance propertyWhiteSpaceIs
  :: ( IsWhiteSpace a
     , ToVal a
     )
  => Property "whiteSpace" a where
  pval = const val

-- https://www.w3.org/TR/css-text-3/#propdef-text-align

newtype TextAlign = TextAlign String

derive newtype instance ToVal TextAlign

justify :: TextAlign
justify = TextAlign "justify"

matchParent :: TextAlign
matchParent = TextAlign "match-parent"

justifyAll :: TextAlign
justifyAll = TextAlign "justify-all"

class IsTextAlign (a :: Type)
instance IsTextAlign Start
instance IsTextAlign End
instance IsTextAlign Left
instance IsTextAlign Right
instance IsTextAlign Center
instance IsTextAlign TextAlign

instance propertyTextAlignIs
  :: ( IsTextAlign a
     , ToVal a
     )
  => Property "textAlign" a where
  pval = const val 

-- https://www.w3.org/TR/css-text-3/#propdef-word-spacing

instance propertyWordSpacingNormal
  :: Property "wordSpacing" Normal where
  pval = const val

instance propertyWordSpacingLength
  :: LengthTag a
  => Property "wordSpacing" (Measure a) where
  pval = const val

-- https://www.w3.org/TR/css-text-3/#propdef-letter-spacing

instance propertyLetterSpacingWordSpacing
  :: Property "wordSpacing" a
  => Property "letterSpacing" a where
  pval = const $ pval (Proxy :: _ "wordSpacing")

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/css-text-decor-3/

-- https://www.w3.org/TR/css-text-decor-3/#propdef-text-decoration-line

class IsTextDecorationLine (a :: Type)
instance IsTextDecorationLine None
instance IsTextDecorationLine (Underline ~ Overline ~ LineThrough ~ Blink)
instance IsTextDecorationLine (Overline ~ LineThrough ~ Blink)
instance IsTextDecorationLine (Underline ~ LineThrough ~ Blink)
instance IsTextDecorationLine (Underline ~ Overline ~ Blink)
instance IsTextDecorationLine (Underline ~ Overline ~ LineThrough)
instance IsTextDecorationLine (Underline ~ Overline)
instance IsTextDecorationLine (Underline ~ LineThrough)
instance IsTextDecorationLine (Underline ~ Blink)
instance IsTextDecorationLine (Overline ~ LineThrough)
instance IsTextDecorationLine (Overline ~ Blink)
instance IsTextDecorationLine (LineThrough ~ Blink)
instance IsTextDecorationLine Underline
instance IsTextDecorationLine Overline
instance IsTextDecorationLine LineThrough
instance IsTextDecorationLine Blink

instance propertyTextDecorationLineIs
  :: ( IsTextDecorationLine a
     , ToVal a
     )
  => Property "textDecorationLine" a where
  pval = const val

-- https://www.w3.org/TR/css-text-decor-3/#propdef-text-decoration-style

class IsTextDecorationStyle (a :: Type)
instance IsTextDecorationStyle Solid
instance IsTextDecorationStyle Double
instance IsTextDecorationStyle Dotted
instance IsTextDecorationStyle Dashed
instance IsTextDecorationStyle Wavy

instance propertyTextDecorationStyleIs
  :: ( IsTextDecorationStyle a
     , ToVal a
     )
  => Property "textDecorationStyle" a where
  pval = const val

-- https://www.w3.org/TR/css-text-decor-3/#propdef-text-decoration

instance propertyTextDecorationColorIsColor
  :: ( IsColor a
     , ToVal a
     )
  => Property "textDecorationColor" a where
  pval = const val

-- https://www.w3.org/TR/css-text-decor-3/#propdef-text-shadow

class IsTextShadow (a :: Type)

instance isTextShadowColorOffsetsBlur
  :: ( IsColor color
     , LengthTag xo
     , LengthTag yo
     , LengthTag blur
     )
  => IsTextShadow (color ~ Measure xo ~ Measure yo ~ Measure blur)

instance isTextShadowOffsetsBlur
  :: ( LengthTag xo
     , LengthTag yo
     , LengthTag blur
     )
  => IsTextShadow (Measure xo ~ Measure yo ~ Measure blur)

else instance isTextShadowColorOffsets
  :: ( IsColor color
     , LengthTag xo
     , LengthTag yo
     )
  => IsTextShadow (color ~ Measure xo ~ Measure yo)

instance isTextShadowOffsets
  :: ( LengthTag xo
     , LengthTag yo
     )
  => IsTextShadow (Measure xo ~ Measure yo)

instance isTextShadowMultiple
  :: ( IsTextShadow x
     , IsTextShadow xs
     )
  => IsTextShadow (x /\ xs)

instance propertyTextShadowNone
  :: Property "textShadow" None where
  pval = const val

else instance propertyTextShadowIs
  :: ( IsTextShadow a
     , MultiVal a
     )
  => Property "textShadow" a where
  pval = const $ joinVals (Val \c -> "," <> c.separator) <<< multiVal

--------------------------------------------------------------------------------

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
matrix a b c d e f =
  TransformFunction $
    fn
      "matrix"
      [ val $ toNumber a
      , val $ toNumber b
      , val $ toNumber c
      , val $ toNumber d
      , val $ toNumber e
      , val $ toNumber f
      ]

-- https://www.w3.org/TR/css-transforms-1/#funcdef-transform-translate

translate
  :: forall x y
   . LengthPercentageTag x
  => LengthPercentageTag y
  => Measure x
  -> Measure y
  -> TransformFunction
translate tx ty = TransformFunction $ fn "translate" [val tx, val ty]

-- https://www.w3.org/TR/css-transforms-1/#funcdef-transform-translatex

translateX :: forall x. LengthPercentageTag x => Measure x -> TransformFunction
translateX tx = TransformFunction $ fn "translateX" [val tx]

-- https://www.w3.org/TR/css-transforms-1/#funcdef-transform-translatey

translateY :: forall y. LengthPercentageTag y => Measure y -> TransformFunction
translateY ty = TransformFunction $ fn "translateY" [val ty]

-- https://www.w3.org/TR/css-transforms-1/#funcdef-transform-scale

scale :: Number -> Number -> TransformFunction
scale sx sy = TransformFunction $ fn "scale" [val sx, val sy]

-- https://www.w3.org/TR/css-transforms-1/#funcdef-transform-scalex

scaleX :: Number -> TransformFunction
scaleX sx = TransformFunction $ fn "scaleX" [val sx]

-- https://www.w3.org/TR/css-transforms-1/#funcdef-transform-scaley

scaleY :: Number -> TransformFunction
scaleY sy = TransformFunction $ fn "scaleY" [val sy]

-- https://www.w3.org/TR/css-transforms-1/#funcdef-transform-rotate

rotate :: forall a. AngleTag a => Measure a -> TransformFunction
rotate angle = TransformFunction $ fn "rotate" [val angle]

-- https://www.w3.org/TR/css-transforms-1/#funcdef-transform-skew

-- `skew` has been omitted because, per spec, it "should not be used" anymore.

-- https://www.w3.org/TR/css-transforms-1/#funcdef-transform-skewx

skewX :: forall a. AngleTag a => Measure a -> TransformFunction
skewX angle = TransformFunction $ fn "skewX" [val angle]

-- https://www.w3.org/TR/css-transforms-1/#funcdef-transform-skewy

skewY :: forall a. AngleTag a => Measure a -> TransformFunction
skewY angle = TransformFunction $ fn "skewY" [val angle]

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
  TransformFunction $
    fn
      "matrix3d"
      [ val $ toNumber a1
      , val $ toNumber b1
      , val $ toNumber c1
      , val $ toNumber d1
      , val $ toNumber a2
      , val $ toNumber b2
      , val $ toNumber c2
      , val $ toNumber d2
      , val $ toNumber a3
      , val $ toNumber b3
      , val $ toNumber c3
      , val $ toNumber d3
      , val $ toNumber a4
      , val $ toNumber b4
      , val $ toNumber c4
      , val $ toNumber d4
      ]

-- https://www.w3.org/TR/css-transforms-2/#funcdef-translate3d

translate3d
  :: forall x y z
   . LengthPercentageTag x
  => LengthPercentageTag y
  => LengthTag z
  => Measure x
  -> Measure y
  -> Measure z
  -> TransformFunction
translate3d tx ty tz =
  TransformFunction $ fn "translate3d" [val tx, val ty, val tz]

-- https://www.w3.org/TR/css-transforms-2/#funcdef-translatez

translateZ :: forall a. LengthTag a => Measure a -> TransformFunction
translateZ tz = TransformFunction $ fn "translateZ" [val tz]

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
  TransformFunction $
    fn
      "scale3d"
      [ val $ toNumber sx
      , val $ toNumber sy
      , val $ toNumber sz
      ]

-- https://www.w3.org/TR/css-transforms-2/#funcdef-scalez

scaleZ :: forall a. ToNumber a => a -> TransformFunction
scaleZ sz = TransformFunction $ fn "scaleZ" [val $ toNumber sz]

-- https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function/rotate3d

rotate3d
  :: forall x y z a
   . ToNumber x
  => ToNumber y
  => ToNumber z
  => AngleTag a
  => x
  -> y
  -> z
  -> Measure a
  -> TransformFunction
rotate3d x y z a =
  TransformFunction $
    fn
      "rotate3d"
      [ val $ toNumber x
      , val $ toNumber y
      , val $ toNumber z
      , val a
      ]

-- https://www.w3.org/TR/css-transforms-2/#funcdef-rotatex

rotateX :: forall a. AngleTag a => Measure a -> TransformFunction
rotateX a = TransformFunction $ fn "rotateX" [val a]

-- https://www.w3.org/TR/css-transforms-2/#funcdef-rotatey

rotateY :: forall a. AngleTag a => Measure a -> TransformFunction
rotateY a = TransformFunction $ fn "rotateY" [val a]

-- https://www.w3.org/TR/css-transforms-2/#funcdef-rotatez

rotateZ :: forall a. AngleTag a => Measure a -> TransformFunction
rotateZ a = TransformFunction $ fn "rotateZ" [val a]

-- https://www.w3.org/TR/css-transforms-2/#funcdef-perspective

class IsPerspective (a :: Type)
instance IsPerspective None
instance LengthTag a => IsPerspective (Measure a)

perspective :: forall a. IsPerspective a => ToVal a => a -> TransformFunction
perspective a = TransformFunction $ fn "perspective" [val a]

-- https://www.w3.org/TR/css-transforms-1/#propdef-transform

class IsTransform (a :: Type)
instance IsTransform TransformFunction
instance isTransformMultiple
  :: IsTransform xs
  => IsTransform (TransformFunction /\ xs)

instance propertyTransformNone
  :: Property "transform" None where
  pval = const val

else instance propertyTransformIs
  :: ( IsTransform a
     , MultiVal a
     )
  => Property "transform" a where
  pval = const $ joinVals (val " ") <<< multiVal

-- https://www.w3.org/TR/css-transforms-1/#propdef-transform-origin

instance propertyTransformOriginPosition3d
  :: ( IsPositionX x
     , ToVal x
     , IsPositionY y
     , ToVal y
     , LengthTag z
     )
  => Property "transformOrigin" (x ~ y ~ Measure z) where
  pval = const val

else instance propertyTransformOriginIsPosition
  :: ( IsPosition xy
     , ToVal xy
     )
  => Property "transformOrigin" xy where
  pval = const val

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/CSS2/visudet.html

-- https://www.w3.org/TR/CSS2/visudet.html#propdef-vertical-align

newtype VerticalAlign = VerticalAlign String

derive newtype instance ToVal VerticalAlign

baseline :: VerticalAlign
baseline = VerticalAlign "baseline"

sub :: VerticalAlign
sub = VerticalAlign "sub"

super :: VerticalAlign
super = VerticalAlign "super"

textTop :: VerticalAlign
textTop = VerticalAlign "text-top"

textBottom :: VerticalAlign
textBottom = VerticalAlign "text-bottom"

class IsVerticalAlign (a :: Type)
instance IsVerticalAlign VerticalAlign
instance IsVerticalAlign Top
instance IsVerticalAlign Middle
instance IsVerticalAlign Bottom
instance LengthPercentageTag a => IsVerticalAlign (Measure a)

instance propertyVerticalAlignIs
  :: ( IsVerticalAlign a
     , ToVal a
     )
  => Property "verticalAlign" a where
  pval = const val

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/css-writing-modes-4/

-- https://www.w3.org/TR/css-writing-modes-4/#propdef-direction

newtype Direction = Direction String

derive newtype instance ToVal Direction

ltr :: Direction
ltr = Direction "ltr"

rtl :: Direction
rtl = Direction "rtl"

instance propertyDirectionIs
  :: Property "direction" Direction where
  pval = const val

--------------------------------------------------------------------------------

-- Global keywords

-- https://www.w3.org/TR/css3-values/#common-keywords

newtype CommonKeyword = CommonKeyword String

instance ToVal CommonKeyword where val (CommonKeyword x) = val x

inherit :: CommonKeyword
inherit = CommonKeyword "inherit"

initial :: CommonKeyword
initial = CommonKeyword "initial"

unset :: CommonKeyword
unset = CommonKeyword "unset"

--------------------------------------------------------------------------------

-- Various unit types representing keywords, attributes, etc.

-- WARNING: The following is generated code. Do not edit.

data Accept = Accept
instance ToVal Accept where val _ = val "accept"
accept = Accept :: Accept
instance IsAttribute Accept

data AcceptCharset = AcceptCharset
instance ToVal AcceptCharset where val _ = val "accept-charset"
acceptCharset = AcceptCharset :: AcceptCharset
instance IsAttribute AcceptCharset

data Accesskey = Accesskey
instance ToVal Accesskey where val _ = val "accesskey"
accesskey = Accesskey :: Accesskey
instance IsAttribute Accesskey

data Action = Action
instance ToVal Action where val _ = val "action"
action = Action :: Action
instance IsAttribute Action

data All = All
instance ToVal All where val _ = val "all"
all = All :: All

data Alt = Alt
instance ToVal Alt where val _ = val "alt"
alt = Alt :: Alt
instance IsAttribute Alt

data Async = Async
instance ToVal Async where val _ = val "async"
async = Async :: Async
instance IsAttribute Async

data Auto = Auto
instance ToVal Auto where val _ = val "auto"
auto = Auto :: Auto

data Autocomplete = Autocomplete
instance ToVal Autocomplete where val _ = val "autocomplete"
autocomplete = Autocomplete :: Autocomplete
instance IsAttribute Autocomplete

data Autofocus = Autofocus
instance ToVal Autofocus where val _ = val "autofocus"
autofocus = Autofocus :: Autofocus
instance IsAttribute Autofocus

data Autoplay = Autoplay
instance ToVal Autoplay where val _ = val "autoplay"
autoplay = Autoplay :: Autoplay
instance IsAttribute Autoplay

data Blink = Blink
instance ToVal Blink where val _ = val "blink"
blink = Blink :: Blink

data BorderBox = BorderBox
instance ToVal BorderBox where val _ = val "border-box"
borderBox = BorderBox :: BorderBox

data Both = Both
instance ToVal Both where val _ = val "both"
both = Both :: Both

data Bottom = Bottom
instance ToVal Bottom where val _ = val "bottom"
bottom = Bottom :: Bottom

data Center = Center
instance ToVal Center where val _ = val "center"
center = Center :: Center

data Charset = Charset
instance ToVal Charset where val _ = val "charset"
charset = Charset :: Charset
instance IsAttribute Charset

data Checked = Checked
instance ToVal Checked where val _ = val "checked"
checked = Checked :: Checked
instance IsAttribute Checked

data Circle = Circle
instance ToVal Circle where val _ = val "circle"
circle = Circle :: Circle

data Cite = Cite
instance ToVal Cite where val _ = val "cite"
cite = Cite :: Cite
instance IsAttribute Cite

data Class = Class
instance ToVal Class where val _ = val "class"
class' = Class :: Class
instance IsAttribute Class

data Clip = Clip
instance ToVal Clip where val _ = val "clip"
clip = Clip :: Clip

data Cols = Cols
instance ToVal Cols where val _ = val "cols"
cols = Cols :: Cols
instance IsAttribute Cols

data Colspan = Colspan
instance ToVal Colspan where val _ = val "colspan"
colspan = Colspan :: Colspan
instance IsAttribute Colspan

data Contain = Contain
instance ToVal Contain where val _ = val "contain"
contain = Contain :: Contain

data Content = Content
instance ToVal Content where val _ = val "content"
content = Content :: Content
instance IsAttribute Content

data ContentBox = ContentBox
instance ToVal ContentBox where val _ = val "content-box"
contentBox = ContentBox :: ContentBox

data Contenteditable = Contenteditable
instance ToVal Contenteditable where val _ = val "contenteditable"
contenteditable = Contenteditable :: Contenteditable
instance IsAttribute Contenteditable

data Controls = Controls
instance ToVal Controls where val _ = val "controls"
controls = Controls :: Controls
instance IsAttribute Controls

data Coords = Coords
instance ToVal Coords where val _ = val "coords"
coords = Coords :: Coords
instance IsAttribute Coords

data Cover = Cover
instance ToVal Cover where val _ = val "cover"
cover = Cover :: Cover

data Dashed = Dashed
instance ToVal Dashed where val _ = val "dashed"
dashed = Dashed :: Dashed

data Data = Data
instance ToVal Data where val _ = val "data"
data' = Data :: Data
instance IsAttribute Data

data Datetime = Datetime
instance ToVal Datetime where val _ = val "datetime"
datetime = Datetime :: Datetime
instance IsAttribute Datetime

data Default = Default
instance ToVal Default where val _ = val "default"
default = Default :: Default
instance IsAttribute Default

data Defer = Defer
instance ToVal Defer where val _ = val "defer"
defer = Defer :: Defer
instance IsAttribute Defer

data Dir = Dir
instance ToVal Dir where val _ = val "dir"
dir = Dir :: Dir
instance IsAttribute Dir

data Dirname = Dirname
instance ToVal Dirname where val _ = val "dirname"
dirname = Dirname :: Dirname
instance IsAttribute Dirname

data Disabled = Disabled
instance ToVal Disabled where val _ = val "disabled"
disabled = Disabled :: Disabled
instance IsAttribute Disabled

data Dotted = Dotted
instance ToVal Dotted where val _ = val "dotted"
dotted = Dotted :: Dotted

data Double = Double
instance ToVal Double where val _ = val "double"
double = Double :: Double

data Download = Download
instance ToVal Download where val _ = val "download"
download = Download :: Download
instance IsAttribute Download

data Draggable = Draggable
instance ToVal Draggable where val _ = val "draggable"
draggable = Draggable :: Draggable
instance IsAttribute Draggable

data Ellipse = Ellipse
instance ToVal Ellipse where val _ = val "ellipse"
ellipse = Ellipse :: Ellipse

data Ellipsis = Ellipsis
instance ToVal Ellipsis where val _ = val "ellipsis"
ellipsis = Ellipsis :: Ellipsis

data Enctype = Enctype
instance ToVal Enctype where val _ = val "enctype"
enctype = Enctype :: Enctype
instance IsAttribute Enctype

data End = End
instance ToVal End where val _ = val "end"
end = End :: End

data Fixed = Fixed
instance ToVal Fixed where val _ = val "fixed"
fixed = Fixed :: Fixed

data For = For
instance ToVal For where val _ = val "for"
for = For :: For
instance IsAttribute For

data Form = Form
instance ToVal Form where val _ = val "form"
form = Form :: Form
instance IsAttribute Form

data Formaction = Formaction
instance ToVal Formaction where val _ = val "formaction"
formaction = Formaction :: Formaction
instance IsAttribute Formaction

data FullSizeKana = FullSizeKana
instance ToVal FullSizeKana where val _ = val "full-size-kana"
fullSizeKana = FullSizeKana :: FullSizeKana

data FullWidth = FullWidth
instance ToVal FullWidth where val _ = val "full-width"
fullWidth = FullWidth :: FullWidth

data Headers = Headers
instance ToVal Headers where val _ = val "headers"
headers = Headers :: Headers
instance IsAttribute Headers

data Height = Height
instance ToVal Height where val _ = val "height"
height = Height :: Height
instance IsAttribute Height

data Hidden = Hidden
instance ToVal Hidden where val _ = val "hidden"
hidden = Hidden :: Hidden
instance IsAttribute Hidden

data High = High
instance ToVal High where val _ = val "high"
high = High :: High
instance IsAttribute High

data Href = Href
instance ToVal Href where val _ = val "href"
href = Href :: Href
instance IsAttribute Href

data Hreflang = Hreflang
instance ToVal Hreflang where val _ = val "hreflang"
hreflang = Hreflang :: Hreflang
instance IsAttribute Hreflang

data HttpEquiv = HttpEquiv
instance ToVal HttpEquiv where val _ = val "http-equiv"
httpEquiv = HttpEquiv :: HttpEquiv
instance IsAttribute HttpEquiv

data Id = Id
instance ToVal Id where val _ = val "id"
id = Id :: Id
instance IsAttribute Id

data Infinite = Infinite
instance ToVal Infinite where val _ = val "infinite"
infinite = Infinite :: Infinite

data Inset = Inset
instance ToVal Inset where val _ = val "inset"
inset = Inset :: Inset

data Ismap = Ismap
instance ToVal Ismap where val _ = val "ismap"
ismap = Ismap :: Ismap
instance IsAttribute Ismap

data Kind = Kind
instance ToVal Kind where val _ = val "kind"
kind = Kind :: Kind
instance IsAttribute Kind

data Label = Label
instance ToVal Label where val _ = val "label"
label = Label :: Label
instance IsAttribute Label

data Lang = Lang
instance ToVal Lang where val _ = val "lang"
lang = Lang :: Lang
instance IsAttribute Lang

data Left = Left
instance ToVal Left where val _ = val "left"
left = Left :: Left

data LineThrough = LineThrough
instance ToVal LineThrough where val _ = val "line-through"
lineThrough = LineThrough :: LineThrough

data List' = List'
instance ToVal List' where val _ = val "list"
list = List' :: List'
instance IsAttribute List'

data Local = Local
instance ToVal Local where val _ = val "local"
local = Local :: Local

data Loop = Loop
instance ToVal Loop where val _ = val "loop"
loop = Loop :: Loop
instance IsAttribute Loop

data Low = Low
instance ToVal Low where val _ = val "low"
low = Low :: Low
instance IsAttribute Low

data Max = Max
instance ToVal Max where val _ = val "max"
max = Max :: Max
instance IsAttribute Max

data Maxlength = Maxlength
instance ToVal Maxlength where val _ = val "maxlength"
maxlength = Maxlength :: Maxlength
instance IsAttribute Maxlength

data Media = Media
instance ToVal Media where val _ = val "media"
media' = Media :: Media
instance IsAttribute Media

data Medium = Medium
instance ToVal Medium where val _ = val "medium"
medium = Medium :: Medium

data Method = Method
instance ToVal Method where val _ = val "method"
method = Method :: Method
instance IsAttribute Method

data Middle = Middle
instance ToVal Middle where val _ = val "middle"
middle = Middle :: Middle

data Min = Min
instance ToVal Min where val _ = val "min"
min = Min :: Min
instance IsAttribute Min

data Multiple = Multiple
instance ToVal Multiple where val _ = val "multiple"
multiple = Multiple :: Multiple
instance IsAttribute Multiple

data Muted = Muted
instance ToVal Muted where val _ = val "muted"
muted = Muted :: Muted
instance IsAttribute Muted

data Name = Name
instance ToVal Name where val _ = val "name"
name = Name :: Name
instance IsAttribute Name

data None = None
instance ToVal None where val _ = val "none"
none = None :: None

data Normal = Normal
instance ToVal Normal where val _ = val "normal"
normal = Normal :: Normal

data Novalidate = Novalidate
instance ToVal Novalidate where val _ = val "novalidate"
novalidate = Novalidate :: Novalidate
instance IsAttribute Novalidate

data Nowrap = Nowrap
instance ToVal Nowrap where val _ = val "nowrap"
nowrap = Nowrap :: Nowrap

data Onabort = Onabort
instance ToVal Onabort where val _ = val "onabort"
onabort = Onabort :: Onabort
instance IsAttribute Onabort

data Onafterprint = Onafterprint
instance ToVal Onafterprint where val _ = val "onafterprint"
onafterprint = Onafterprint :: Onafterprint
instance IsAttribute Onafterprint

data Onbeforeprint = Onbeforeprint
instance ToVal Onbeforeprint where val _ = val "onbeforeprint"
onbeforeprint = Onbeforeprint :: Onbeforeprint
instance IsAttribute Onbeforeprint

data Onbeforeunload = Onbeforeunload
instance ToVal Onbeforeunload where val _ = val "onbeforeunload"
onbeforeunload = Onbeforeunload :: Onbeforeunload
instance IsAttribute Onbeforeunload

data Onblur = Onblur
instance ToVal Onblur where val _ = val "onblur"
onblur = Onblur :: Onblur
instance IsAttribute Onblur

data Oncanplay = Oncanplay
instance ToVal Oncanplay where val _ = val "oncanplay"
oncanplay = Oncanplay :: Oncanplay
instance IsAttribute Oncanplay

data Oncanplaythrough = Oncanplaythrough
instance ToVal Oncanplaythrough where val _ = val "oncanplaythrough"
oncanplaythrough = Oncanplaythrough :: Oncanplaythrough
instance IsAttribute Oncanplaythrough

data Onchange = Onchange
instance ToVal Onchange where val _ = val "onchange"
onchange = Onchange :: Onchange
instance IsAttribute Onchange

data Onclick = Onclick
instance ToVal Onclick where val _ = val "onclick"
onclick = Onclick :: Onclick
instance IsAttribute Onclick

data Oncontextmenu = Oncontextmenu
instance ToVal Oncontextmenu where val _ = val "oncontextmenu"
oncontextmenu = Oncontextmenu :: Oncontextmenu
instance IsAttribute Oncontextmenu

data Oncopy = Oncopy
instance ToVal Oncopy where val _ = val "oncopy"
oncopy = Oncopy :: Oncopy
instance IsAttribute Oncopy

data Oncuechange = Oncuechange
instance ToVal Oncuechange where val _ = val "oncuechange"
oncuechange = Oncuechange :: Oncuechange
instance IsAttribute Oncuechange

data Oncut = Oncut
instance ToVal Oncut where val _ = val "oncut"
oncut = Oncut :: Oncut
instance IsAttribute Oncut

data Ondblclick = Ondblclick
instance ToVal Ondblclick where val _ = val "ondblclick"
ondblclick = Ondblclick :: Ondblclick
instance IsAttribute Ondblclick

data Ondrag = Ondrag
instance ToVal Ondrag where val _ = val "ondrag"
ondrag = Ondrag :: Ondrag
instance IsAttribute Ondrag

data Ondragend = Ondragend
instance ToVal Ondragend where val _ = val "ondragend"
ondragend = Ondragend :: Ondragend
instance IsAttribute Ondragend

data Ondragenter = Ondragenter
instance ToVal Ondragenter where val _ = val "ondragenter"
ondragenter = Ondragenter :: Ondragenter
instance IsAttribute Ondragenter

data Ondragleave = Ondragleave
instance ToVal Ondragleave where val _ = val "ondragleave"
ondragleave = Ondragleave :: Ondragleave
instance IsAttribute Ondragleave

data Ondragover = Ondragover
instance ToVal Ondragover where val _ = val "ondragover"
ondragover = Ondragover :: Ondragover
instance IsAttribute Ondragover

data Ondragstart = Ondragstart
instance ToVal Ondragstart where val _ = val "ondragstart"
ondragstart = Ondragstart :: Ondragstart
instance IsAttribute Ondragstart

data Ondrop = Ondrop
instance ToVal Ondrop where val _ = val "ondrop"
ondrop = Ondrop :: Ondrop
instance IsAttribute Ondrop

data Ondurationchange = Ondurationchange
instance ToVal Ondurationchange where val _ = val "ondurationchange"
ondurationchange = Ondurationchange :: Ondurationchange
instance IsAttribute Ondurationchange

data Onemptied = Onemptied
instance ToVal Onemptied where val _ = val "onemptied"
onemptied = Onemptied :: Onemptied
instance IsAttribute Onemptied

data Onended = Onended
instance ToVal Onended where val _ = val "onended"
onended = Onended :: Onended
instance IsAttribute Onended

data Onerror = Onerror
instance ToVal Onerror where val _ = val "onerror"
onerror = Onerror :: Onerror
instance IsAttribute Onerror

data Onfocus = Onfocus
instance ToVal Onfocus where val _ = val "onfocus"
onfocus = Onfocus :: Onfocus
instance IsAttribute Onfocus

data Onhashchange = Onhashchange
instance ToVal Onhashchange where val _ = val "onhashchange"
onhashchange = Onhashchange :: Onhashchange
instance IsAttribute Onhashchange

data Oninput = Oninput
instance ToVal Oninput where val _ = val "oninput"
oninput = Oninput :: Oninput
instance IsAttribute Oninput

data Oninvalid = Oninvalid
instance ToVal Oninvalid where val _ = val "oninvalid"
oninvalid = Oninvalid :: Oninvalid
instance IsAttribute Oninvalid

data Onkeydown = Onkeydown
instance ToVal Onkeydown where val _ = val "onkeydown"
onkeydown = Onkeydown :: Onkeydown
instance IsAttribute Onkeydown

data Onkeypress = Onkeypress
instance ToVal Onkeypress where val _ = val "onkeypress"
onkeypress = Onkeypress :: Onkeypress
instance IsAttribute Onkeypress

data Onkeyup = Onkeyup
instance ToVal Onkeyup where val _ = val "onkeyup"
onkeyup = Onkeyup :: Onkeyup
instance IsAttribute Onkeyup

data Onload = Onload
instance ToVal Onload where val _ = val "onload"
onload = Onload :: Onload
instance IsAttribute Onload

data Onloadeddata = Onloadeddata
instance ToVal Onloadeddata where val _ = val "onloadeddata"
onloadeddata = Onloadeddata :: Onloadeddata
instance IsAttribute Onloadeddata

data Onloadedmetadata = Onloadedmetadata
instance ToVal Onloadedmetadata where val _ = val "onloadedmetadata"
onloadedmetadata = Onloadedmetadata :: Onloadedmetadata
instance IsAttribute Onloadedmetadata

data Onloadstart = Onloadstart
instance ToVal Onloadstart where val _ = val "onloadstart"
onloadstart = Onloadstart :: Onloadstart
instance IsAttribute Onloadstart

data Onmousedown = Onmousedown
instance ToVal Onmousedown where val _ = val "onmousedown"
onmousedown = Onmousedown :: Onmousedown
instance IsAttribute Onmousedown

data Onmousemove = Onmousemove
instance ToVal Onmousemove where val _ = val "onmousemove"
onmousemove = Onmousemove :: Onmousemove
instance IsAttribute Onmousemove

data Onmouseout = Onmouseout
instance ToVal Onmouseout where val _ = val "onmouseout"
onmouseout = Onmouseout :: Onmouseout
instance IsAttribute Onmouseout

data Onmouseover = Onmouseover
instance ToVal Onmouseover where val _ = val "onmouseover"
onmouseover = Onmouseover :: Onmouseover
instance IsAttribute Onmouseover

data Onmouseup = Onmouseup
instance ToVal Onmouseup where val _ = val "onmouseup"
onmouseup = Onmouseup :: Onmouseup
instance IsAttribute Onmouseup

data Onmousewheel = Onmousewheel
instance ToVal Onmousewheel where val _ = val "onmousewheel"
onmousewheel = Onmousewheel :: Onmousewheel
instance IsAttribute Onmousewheel

data Onoffline = Onoffline
instance ToVal Onoffline where val _ = val "onoffline"
onoffline = Onoffline :: Onoffline
instance IsAttribute Onoffline

data Ononline = Ononline
instance ToVal Ononline where val _ = val "ononline"
ononline = Ononline :: Ononline
instance IsAttribute Ononline

data Onpagehide = Onpagehide
instance ToVal Onpagehide where val _ = val "onpagehide"
onpagehide = Onpagehide :: Onpagehide
instance IsAttribute Onpagehide

data Onpageshow = Onpageshow
instance ToVal Onpageshow where val _ = val "onpageshow"
onpageshow = Onpageshow :: Onpageshow
instance IsAttribute Onpageshow

data Onpaste = Onpaste
instance ToVal Onpaste where val _ = val "onpaste"
onpaste = Onpaste :: Onpaste
instance IsAttribute Onpaste

data Onpause = Onpause
instance ToVal Onpause where val _ = val "onpause"
onpause = Onpause :: Onpause
instance IsAttribute Onpause

data Onplay = Onplay
instance ToVal Onplay where val _ = val "onplay"
onplay = Onplay :: Onplay
instance IsAttribute Onplay

data Onplaying = Onplaying
instance ToVal Onplaying where val _ = val "onplaying"
onplaying = Onplaying :: Onplaying
instance IsAttribute Onplaying

data Onpopstate = Onpopstate
instance ToVal Onpopstate where val _ = val "onpopstate"
onpopstate = Onpopstate :: Onpopstate
instance IsAttribute Onpopstate

data Onprogress = Onprogress
instance ToVal Onprogress where val _ = val "onprogress"
onprogress = Onprogress :: Onprogress
instance IsAttribute Onprogress

data Onratechange = Onratechange
instance ToVal Onratechange where val _ = val "onratechange"
onratechange = Onratechange :: Onratechange
instance IsAttribute Onratechange

data Onreset = Onreset
instance ToVal Onreset where val _ = val "onreset"
onreset = Onreset :: Onreset
instance IsAttribute Onreset

data Onresize = Onresize
instance ToVal Onresize where val _ = val "onresize"
onresize = Onresize :: Onresize
instance IsAttribute Onresize

data Onscroll = Onscroll
instance ToVal Onscroll where val _ = val "onscroll"
onscroll = Onscroll :: Onscroll
instance IsAttribute Onscroll

data Onsearch = Onsearch
instance ToVal Onsearch where val _ = val "onsearch"
onsearch = Onsearch :: Onsearch
instance IsAttribute Onsearch

data Onseeked = Onseeked
instance ToVal Onseeked where val _ = val "onseeked"
onseeked = Onseeked :: Onseeked
instance IsAttribute Onseeked

data Onseeking = Onseeking
instance ToVal Onseeking where val _ = val "onseeking"
onseeking = Onseeking :: Onseeking
instance IsAttribute Onseeking

data Onselect = Onselect
instance ToVal Onselect where val _ = val "onselect"
onselect = Onselect :: Onselect
instance IsAttribute Onselect

data Onstalled = Onstalled
instance ToVal Onstalled where val _ = val "onstalled"
onstalled = Onstalled :: Onstalled
instance IsAttribute Onstalled

data Onstorage = Onstorage
instance ToVal Onstorage where val _ = val "onstorage"
onstorage = Onstorage :: Onstorage
instance IsAttribute Onstorage

data Onsubmit = Onsubmit
instance ToVal Onsubmit where val _ = val "onsubmit"
onsubmit = Onsubmit :: Onsubmit
instance IsAttribute Onsubmit

data Onsuspend = Onsuspend
instance ToVal Onsuspend where val _ = val "onsuspend"
onsuspend = Onsuspend :: Onsuspend
instance IsAttribute Onsuspend

data Ontimeupdate = Ontimeupdate
instance ToVal Ontimeupdate where val _ = val "ontimeupdate"
ontimeupdate = Ontimeupdate :: Ontimeupdate
instance IsAttribute Ontimeupdate

data Ontoggle = Ontoggle
instance ToVal Ontoggle where val _ = val "ontoggle"
ontoggle = Ontoggle :: Ontoggle
instance IsAttribute Ontoggle

data Onunload = Onunload
instance ToVal Onunload where val _ = val "onunload"
onunload = Onunload :: Onunload
instance IsAttribute Onunload

data Onvolumechange = Onvolumechange
instance ToVal Onvolumechange where val _ = val "onvolumechange"
onvolumechange = Onvolumechange :: Onvolumechange
instance IsAttribute Onvolumechange

data Onwaiting = Onwaiting
instance ToVal Onwaiting where val _ = val "onwaiting"
onwaiting = Onwaiting :: Onwaiting
instance IsAttribute Onwaiting

data Onwheel = Onwheel
instance ToVal Onwheel where val _ = val "onwheel"
onwheel = Onwheel :: Onwheel
instance IsAttribute Onwheel

data Open = Open
instance ToVal Open where val _ = val "open"
open = Open :: Open
instance IsAttribute Open

data Optimum = Optimum
instance ToVal Optimum where val _ = val "optimum"
optimum = Optimum :: Optimum
instance IsAttribute Optimum

data Overline = Overline
instance ToVal Overline where val _ = val "overline"
overline = Overline :: Overline

data PaddingBox = PaddingBox
instance ToVal PaddingBox where val _ = val "padding-box"
paddingBox = PaddingBox :: PaddingBox

data Pattern = Pattern
instance ToVal Pattern where val _ = val "pattern"
pattern = Pattern :: Pattern
instance IsAttribute Pattern

data Placeholder = Placeholder
instance ToVal Placeholder where val _ = val "placeholder"
placeholder = Placeholder :: Placeholder
instance IsAttribute Placeholder

data Poster = Poster
instance ToVal Poster where val _ = val "poster"
poster = Poster :: Poster
instance IsAttribute Poster

data Preload = Preload
instance ToVal Preload where val _ = val "preload"
preload = Preload :: Preload
instance IsAttribute Preload

data Readonly = Readonly
instance ToVal Readonly where val _ = val "readonly"
readonly = Readonly :: Readonly
instance IsAttribute Readonly

data Rel = Rel
instance ToVal Rel where val _ = val "rel"
rel = Rel :: Rel
instance IsAttribute Rel

data Required = Required
instance ToVal Required where val _ = val "required"
required = Required :: Required
instance IsAttribute Required

data Reversed = Reversed
instance ToVal Reversed where val _ = val "reversed"
reversed = Reversed :: Reversed
instance IsAttribute Reversed

data Right = Right
instance ToVal Right where val _ = val "right"
right = Right :: Right

data Rows = Rows
instance ToVal Rows where val _ = val "rows"
rows = Rows :: Rows
instance IsAttribute Rows

data Rowspan = Rowspan
instance ToVal Rowspan where val _ = val "rowspan"
rowspan = Rowspan :: Rowspan
instance IsAttribute Rowspan

data Sandbox = Sandbox
instance ToVal Sandbox where val _ = val "sandbox"
sandbox = Sandbox :: Sandbox
instance IsAttribute Sandbox

data Scope = Scope
instance ToVal Scope where val _ = val "scope"
scope = Scope :: Scope
instance IsAttribute Scope

data Scroll = Scroll
instance ToVal Scroll where val _ = val "scroll"
scroll = Scroll :: Scroll

data Selected = Selected
instance ToVal Selected where val _ = val "selected"
selected = Selected :: Selected
instance IsAttribute Selected

data Shape = Shape
instance ToVal Shape where val _ = val "shape"
shape = Shape :: Shape
instance IsAttribute Shape

data Size = Size
instance ToVal Size where val _ = val "size"
size = Size :: Size
instance IsAttribute Size

data Sizes = Sizes
instance ToVal Sizes where val _ = val "sizes"
sizes = Sizes :: Sizes
instance IsAttribute Sizes

data Solid = Solid
instance ToVal Solid where val _ = val "solid"
solid = Solid :: Solid

data Span = Span
instance ToVal Span where val _ = val "span"
span = Span :: Span
instance IsAttribute Span

data Spellcheck = Spellcheck
instance ToVal Spellcheck where val _ = val "spellcheck"
spellcheck = Spellcheck :: Spellcheck
instance IsAttribute Spellcheck

data Src = Src
instance ToVal Src where val _ = val "src"
src = Src :: Src
instance IsAttribute Src

data Srcdoc = Srcdoc
instance ToVal Srcdoc where val _ = val "srcdoc"
srcdoc = Srcdoc :: Srcdoc
instance IsAttribute Srcdoc

data Srclang = Srclang
instance ToVal Srclang where val _ = val "srclang"
srclang = Srclang :: Srclang
instance IsAttribute Srclang

data Srcset = Srcset
instance ToVal Srcset where val _ = val "srcset"
srcset = Srcset :: Srcset
instance IsAttribute Srcset

data Start = Start
instance ToVal Start where val _ = val "start"
start = Start :: Start
instance IsAttribute Start

data Step = Step
instance ToVal Step where val _ = val "step"
step = Step :: Step
instance IsAttribute Step

data Style = Style
instance ToVal Style where val _ = val "style"
style = Style :: Style
instance IsAttribute Style

data Tabindex = Tabindex
instance ToVal Tabindex where val _ = val "tabindex"
tabindex = Tabindex :: Tabindex
instance IsAttribute Tabindex

data Target = Target
instance ToVal Target where val _ = val "target"
target = Target :: Target
instance IsAttribute Target

data Title = Title
instance ToVal Title where val _ = val "title"
title = Title :: Title
instance IsAttribute Title

data Top = Top
instance ToVal Top where val _ = val "top"
top = Top :: Top

data Translate = Translate
instance ToVal Translate where val _ = val "translate"
translate' = Translate :: Translate
instance IsAttribute Translate

data Type' = Type'
instance ToVal Type' where val _ = val "type"
type' = Type' :: Type'
instance IsAttribute Type'

data Underline = Underline
instance ToVal Underline where val _ = val "underline"
underline = Underline :: Underline

data Usemap = Usemap
instance ToVal Usemap where val _ = val "usemap"
usemap = Usemap :: Usemap
instance IsAttribute Usemap

data Value = Value
instance ToVal Value where val _ = val "value"
value = Value :: Value
instance IsAttribute Value

data Visible = Visible
instance ToVal Visible where val _ = val "visible"
visible = Visible :: Visible

data Wavy = Wavy
instance ToVal Wavy where val _ = val "wavy"
wavy = Wavy :: Wavy

data Width = Width
instance ToVal Width where val _ = val "width"
width = Width :: Width
instance IsAttribute Width

data Wrap = Wrap
instance ToVal Wrap where val _ = val "wrap"
wrap = Wrap :: Wrap
instance IsAttribute Wrap

data WrapReverse = WrapReverse
instance ToVal WrapReverse where val _ = val "wrap-reverse"
wrapReverse = WrapReverse :: WrapReverse
