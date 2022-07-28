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
import Data.List (List(Nil), (:))
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

data Declaration = Declaration

type SupportedDeclarations' (v :: Type) =
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
  , borderBottom :: v
  , borderBottomColor :: v
  , borderBottomLeftRadius :: v
  , borderBottomRightRadius :: v
  , borderBottomStyle :: v
  , borderBottomWidth :: v
  , borderColor :: v
  , borderLeft :: v
  , borderLeftColor :: v
  , borderLeftStyle :: v
  , borderLeftWidth :: v
  , borderRight :: v
  , borderRightColor :: v
  , borderRightStyle :: v
  , borderRightWidth :: v
  , borderStyle :: v
  , borderTop :: v
  , borderTopColor :: v
  , borderTopLeftRadius :: v
  , borderTopRightRadius :: v
  , borderTopStyle :: v
  , borderTopWidth :: v
  , borderWidth :: v
  , color :: v
  , height :: v
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
  , padding :: v
  , paddingBottom :: v
  , paddingLeft :: v
  , paddingRight :: v
  , paddingTop :: v
  , width :: v
  )

defaultDeclarations :: { | SupportedDeclarations }
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
  , borderBottom: v
  , borderBottomColor: v
  , borderBottomLeftRadius: v
  , borderBottomRightRadius: v
  , borderBottomStyle: v
  , borderBottomWidth: v
  , borderColor: v
  , borderLeft: v
  , borderLeftColor: v
  , borderLeftStyle: v
  , borderLeftWidth: v
  , borderRight: v
  , borderRightColor: v
  , borderRightStyle: v
  , borderRightWidth: v
  , borderStyle: v
  , borderTop: v
  , borderTopColor: v
  , borderTopLeftRadius: v
  , borderTopRightRadius: v
  , borderTopStyle: v
  , borderTopWidth: v
  , borderWidth: v
  , color: v
  , height: v
  , margin: v
  , marginBottom: v
  , marginLeft: v
  , marginRight: v
  , marginTop: v
  , maxHeight: v
  , maxWidth: v
  , minHeight: v
  , minWidth: v
  , padding: v
  , paddingBottom: v
  , paddingLeft: v
  , paddingRight: v
  , paddingTop: v
  , opacity: v
  , width: v
  }
  where
    v = Nothing

type SupportedDeclarations = SupportedDeclarations' (Maybe Val)

class Property (p :: Symbol) (v :: Type) where
  pval :: Proxy p -> v -> Val

instance Property p v => ConvertOption Declaration p v (Maybe Val) where
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
    Declaration
    { | SupportedDeclarations }
    { | providedDeclarations }
    { | SupportedDeclarations }
  => Statement' (NonEmpty Array (Measure Percentage)) { | providedDeclarations } (Writer (Array KeyframeBlock) Unit) where
  statement selectors provided =
    tell $ pure $ KeyframeBlock selectors $ declarationBlock provided
else instance
  ConvertOptionsWithDefaults
    Declaration
    { | SupportedDeclarations }
    { | providedDeclarations }
    { | SupportedDeclarations }
  => Statement' (NonEmpty Array (Selector tag)) { | providedDeclarations } (Writer (Array Statement) Unit) where
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
  :: forall providedDeclarations rl
   . RowToList SupportedDeclarations rl
  => CollectDeclarations rl SupportedDeclarations
  => ConvertOptionsWithDefaults Declaration
       { | SupportedDeclarations }
       { | providedDeclarations }
       { | SupportedDeclarations }
  => { | providedDeclarations }
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
  all' = convertOptionsWithDefaults Declaration defaultDeclarations provided

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
    Declaration
    { | SupportedDeclarations }
    { | providedDeclarations }
    { | SupportedDeclarations }
  => Render (Record providedDeclarations) where
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

infixl 7 add as @+@

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

infixl 7 subtract as @-@

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

infixl 8 multiply as @*

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

infixl 8 multiplyFlipped as *@

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

infixl 8 divide as @/

-- https://www.w3.org/TR/css-values-3/#url-value

newtype URL = URL String

instance toValURL :: ToVal URL where
  val (URL x) = val $ fn "url" [val $ quote x]

url :: String -> URL
url = URL

-- https://www.w3.org/TR/css-values-4/#typedef-position

newtype Position = Position Val

derive newtype instance ToVal Position

class ToVal a <= At1 (a :: Type)
instance At1 Left
instance At1 Center
instance At1 Right
instance At1 Top
instance At1 Bottom
instance LengthPercentageTag a => At1 (Measure a)

at :: forall a. At1 a => a -> Position
at = Position <<< val

class (ToVal x, ToVal y) <= At2 x y
instance At2 Left Top
instance At2 Left Center
instance At2 Left Bottom
instance At2 Center Top
instance At2 Center Center
instance At2 Center Bottom
instance At2 Right Top
instance At2 Right Center
instance At2 Right Bottom
instance LengthPercentageTag x => At2 (Measure x) Top
instance LengthPercentageTag x => At2 (Measure x) Center
instance LengthPercentageTag x => At2 (Measure x) Bottom
instance LengthPercentageTag y => At2 Left (Measure y)
instance LengthPercentageTag y => At2 Center (Measure y)
instance LengthPercentageTag y => At2 Right (Measure y)
instance
  ( LengthPercentageTag x
  , LengthPercentageTag y
  ) => At2 (Measure x) (Measure y)

at2 :: forall x y. At2 x y => x -> y -> Position
at2 x y = Position $ val x <> val " " <> val y

class (ToVal a, ToVal b, ToVal c) <= At3 (a :: Type) (b :: Type) (c :: Type)
instance LengthPercentageTag a => At3 Left (Measure a) Top
instance LengthPercentageTag a => At3 Left (Measure a) Center
instance LengthPercentageTag a => At3 Left (Measure a) Bottom
instance LengthPercentageTag a => At3 Left Top (Measure a)
instance LengthPercentageTag a => At3 Left Bottom (Measure a)
instance LengthPercentageTag a => At3 Right (Measure a) Top
instance LengthPercentageTag a => At3 Right (Measure a) Center
instance LengthPercentageTag a => At3 Right (Measure a) Bottom
instance LengthPercentageTag a => At3 Right Top (Measure a)
instance LengthPercentageTag a => At3 Right Bottom (Measure a)
instance LengthPercentageTag a => At3 Center Top (Measure a)
instance LengthPercentageTag a => At3 Center Bottom (Measure a)

at3 :: forall a b c. At3 a b c => a -> b -> c -> Position
at3 a b c = Position $ joinVals (val " ") [val a, val b, val c]

class
  ( ToVal x
  , ToVal xo
  , ToVal y
  , ToVal yo
  ) <= At4 (x :: Type) (xo :: Type) (y :: Type)  (yo :: Type)
instance
  ( LengthPercentageTag xo
  , LengthPercentageTag yo
  ) => At4 Left (Measure xo) Top (Measure yo)
instance
  ( LengthPercentageTag xo
  , LengthPercentageTag yo
  ) => At4 Left (Measure xo) Bottom (Measure yo)
instance
  ( LengthPercentageTag xo
  , LengthPercentageTag yo
  ) => At4 Right (Measure xo) Top (Measure yo)
instance
  ( LengthPercentageTag xo
  , LengthPercentageTag yo
  ) => At4 Right (Measure xo) Bottom (Measure yo)

at4 :: forall x xo y yo. At4 x xo y yo => x -> xo -> y -> yo -> Position
at4 x xo y yo = Position $ joinVals (val " ") [val x, val xo, val y, val yo]

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
instance MediaFeature f v => ConvertOption MediaFeature' f v (Maybe Val) where
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

class ValAnimationName (a :: Type) where
  valAnimationName :: a -> Val

instance valAnimationNameNone :: ValAnimationName None where
  valAnimationName = val

instance valAnimationNameKeyframesName :: ValAnimationName KeyframesName where
  valAnimationName = val

instance
  valAnimationNameMultiple
  :: (ValAnimationName a, ValAnimationName b)
  => ValAnimationName (a /\ b) where
  valAnimationName (a /\ b) =
    joinVals
      (Val \c -> "," <> c.separator)
      [ valAnimationName a
      , valAnimationName b
      ]

instance propertyAnimationNameCommonKeyword
  :: Property "animationName" CommonKeyword where
  pval = const val

else instance propertyAnimationNameNone
  :: ValAnimationName a
  => Property "animationName" a where
  pval = const valAnimationName

-- https://www.w3.org/TR/css-animations-1/#propdef-animation-duration

class ValAnimationDuration (a :: Type) where
  valAnimationDuration :: a -> Val

instance valAnimationDurationNone :: ValAnimationDuration None where
  valAnimationDuration = val

instance valAnimationDurationTime
  :: TimeTag a
  => ValAnimationDuration (Measure a) where
  valAnimationDuration = val

instance valAnimationDurationMultiple
  :: ( ValAnimationDuration a
     , ValAnimationDuration b
     )
  => ValAnimationDuration (a /\ b) where
  valAnimationDuration (a /\ b) =
    joinVals
      (Val \c -> "," <> c.separator)
      [ valAnimationDuration a
      , valAnimationDuration b
      ]

instance propertyAnimationDurationCommonKeyword
  :: Property "animationDuration" CommonKeyword where
  pval = const val

else instance propertyAnimationDurationNone
  :: ValAnimationDuration a
  => Property "animationDuration" a where
  pval = const valAnimationDuration

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/css-animations-1/#propdef-animation-timing-function

class ValAnimationTimingFunction (a :: Type) where
  valAnimationTimingFunction :: a -> Val

instance valAnimationTimingFunctionSingle
  :: ValAnimationTimingFunction EasingFunction where
  valAnimationTimingFunction = val

instance valAnimationTimingFunctionMultiple
  :: ( ValAnimationTimingFunction a
     , ValAnimationTimingFunction b
     )
  => ValAnimationTimingFunction (a /\ b) where
  valAnimationTimingFunction (a /\ b) =
    joinVals
      (Val \{ separator } -> "," <> separator)
      [ valAnimationTimingFunction a, valAnimationTimingFunction b]

instance propertyAnimationTimingFunctionCommonKeyword
  :: Property "animationTimingFunction" CommonKeyword where
  pval = const val

else instance propertyAnimationTimingFunctionVal
  :: ValAnimationTimingFunction a
  => Property "animationTimingFunction" a where
  pval = const valAnimationTimingFunction

-- https://www.w3.org/TR/css-animations-1/#propdef-animation-iteration-count

data Infinite = Infinite
infinite = Infinite :: Infinite
instance ToVal Infinite where val _ = val "infinite"

class ToVal a <= SingleAnimationIterationCountCompatible (a :: Type)
instance SingleAnimationIterationCountCompatible Infinite
instance SingleAnimationIterationCountCompatible Int

class ValAnimationIterationCount (a :: Type) where
  valAnimationIterationCount :: a -> Val

instance valAnimationIterationCountMultiple
  :: ( ValAnimationIterationCount a
     , ValAnimationIterationCount b
     )
  => ValAnimationIterationCount (a /\ b) where
  valAnimationIterationCount (a /\ b) =
    joinVals
      (Val \{ separator } -> "," <> separator)
      [ valAnimationIterationCount a
      , valAnimationIterationCount b
      ]

else instance valAnimationIterationCountSingle
  :: SingleAnimationIterationCountCompatible a
  => ValAnimationIterationCount a where
  valAnimationIterationCount = val

instance propertyAnimationIterationCountCommonKeyword
  :: Property "animationIterationCount" CommonKeyword where
  pval = const val

else instance propertyAnimationIterationCountVal
  :: ValAnimationIterationCount a
  => Property "animationIterationCount" a where
  pval = const valAnimationIterationCount

-- https://www.w3.org/TR/css-animations-1/#propdef-animation-direction

newtype SingleAnimationDirection = SingleAnimationDirection String

derive newtype instance ToVal SingleAnimationDirection

reverse :: SingleAnimationDirection
reverse = SingleAnimationDirection "reverse"

alternate :: SingleAnimationDirection
alternate = SingleAnimationDirection "alternate"

alternateReverse :: SingleAnimationDirection
alternateReverse = SingleAnimationDirection "alternate-reverse"

class ToVal a <= SingleAnimationDirectionCompatible (a :: Type)
instance SingleAnimationDirectionCompatible Normal
instance SingleAnimationDirectionCompatible SingleAnimationDirection

class ValAnimationDirection (a :: Type) where
  valAnimationDirection :: a -> Val

instance valAnimationDirectionMultiple
  :: ( ValAnimationDirection a
     , ValAnimationDirection b
     )
  => ValAnimationDirection (a /\ b) where
  valAnimationDirection (a /\ b) =
    joinVals
      (Val \{ separator } -> "," <> separator)
      [ valAnimationDirection a
      , valAnimationDirection b
      ]

else instance valAnimationDirectionSingle
  :: SingleAnimationDirectionCompatible a
  => ValAnimationDirection a where
  valAnimationDirection = val

instance propertyAnimationDirectionCommonKeyword
  :: Property "animationDirection" CommonKeyword where
  pval = const val

else instance propertyAnimationDirection
  :: ValAnimationDirection a
  => Property "animationDirection" a where
  pval = const valAnimationDirection

-- https://www.w3.org/TR/css-animations-1/#propdef-animation-play-state

newtype SingleAnimationPlayState = SingleAnimationPlayState String

derive newtype instance ToVal SingleAnimationPlayState

running :: SingleAnimationPlayState
running = SingleAnimationPlayState "running"

paused :: SingleAnimationPlayState
paused = SingleAnimationPlayState "paused"

class ValAnimationPlayState (a :: Type) where
  valAnimationPlayState :: a -> Val

instance valAnimationPlayStateMultiple
  :: ( ValAnimationPlayState a
     , ValAnimationPlayState b
     )
  => ValAnimationPlayState (a /\ b) where
  valAnimationPlayState (a /\ b) =
    joinVals
      (Val \{ separator } -> "," <> separator)
      [ valAnimationPlayState a
      , valAnimationPlayState b
      ]

instance valAnimationPlayStateSingle
  :: ValAnimationPlayState SingleAnimationPlayState where
  valAnimationPlayState = val

instance propertyAnimationPlayStateCommonKeyword
  :: Property "animationPlayState" CommonKeyword where
  pval = const val

else instance propertyAnimationPlayStateValAnimationPlayState
  :: ValAnimationPlayState a
  => Property "animationPlayState" a where
  pval = const valAnimationPlayState

-- https://www.w3.org/TR/css-animations-1/#propdef-animation-delay

instance propertyAnimationDelay
  :: Property "animationDuration" a
  => Property "animationDelay" a where
  pval _ = pval (Proxy :: _ "animationDuration")

-- https://www.w3.org/TR/css-animations-1/#propdef-animation-fill-mode

newtype SingleAnimationFillMode = SingleAnimationFillMode String

derive newtype instance ToVal SingleAnimationFillMode

forwards :: SingleAnimationFillMode
forwards = SingleAnimationFillMode "forwards"

backwards :: SingleAnimationFillMode
backwards = SingleAnimationFillMode "backwards"

class ToVal a <= SingleAnimationFillModeCompatible (a :: Type)
instance SingleAnimationFillModeCompatible SingleAnimationFillMode
instance SingleAnimationFillModeCompatible None
instance SingleAnimationFillModeCompatible Both

class ValAnimationFillMode (a :: Type) where
  valAnimationFillMode :: a -> Val

instance valAnimationFillModeMultiple
  :: ( ValAnimationFillMode a
     , ValAnimationFillMode b
     )
  => ValAnimationFillMode (a /\ b) where
  valAnimationFillMode (a /\ b) =
    joinVals
      (Val \{ separator } -> "," <> separator)
      [ valAnimationFillMode a
      , valAnimationFillMode b
      ]

else instance valAnimationFillModeSingle
  :: SingleAnimationFillModeCompatible a
  => ValAnimationFillMode a where
  valAnimationFillMode = val

instance propertyAnimationFillModeCommonKeyword
  :: Property "animationFillMode" CommonKeyword where
  pval = const val

else instance propertyAnimationFillModeVal
  :: ValAnimationFillMode a => Property "animationFillMode" a where
  pval = const valAnimationFillMode

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/css-backgrounds-3/

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-background-color

instance propertyBackgroundColorCommonKeyword
  :: Property "backgroundColor" CommonKeyword where
  pval = const val

else instance propertyBackgroundColorColor
  :: IsColor a => Property "backgroundColor" a where
  pval = const val

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-background-image

class ValBackgroundImage (a :: Type) where
  valBackgroundImage :: a -> Val

instance valBackgroundImageMultiple
  :: ( ValBackgroundImage a
     , ValBackgroundImage b
     )
  => ValBackgroundImage (a /\ b) where
  valBackgroundImage (a /\ b) =
    valBackgroundImage a
    <> (Val \c -> "," <> c.separator)
    <> valBackgroundImage b

else instance ValBackgroundImage None where
  valBackgroundImage = val

else instance IsImage a => ValBackgroundImage a where
  valBackgroundImage = val

instance propertyBackgroundImageCommonKeyword
  :: Property "backgroundImage" CommonKeyword where
  pval = const val

else instance propertyBackgroundImageVal
  :: ValBackgroundImage a
  => Property "backgroundImage" a where
  pval = const valBackgroundImage

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

repeat2
  :: RepeatStyle Unit
  -> RepeatStyle Unit
  -> RepeatStyle (RepeatStyle Unit)
repeat2 (RepeatStyle a) (RepeatStyle b) = RepeatStyle $ a <> " " <> b

class ValBackgroundRepeat (a :: Type) where
  valBackgroundRepeat :: a -> Val

instance valBackgroundRepeatMultiple
  :: ( ValBackgroundRepeat a
     , ValBackgroundRepeat b
     )
  => ValBackgroundRepeat (a /\ b) where
  valBackgroundRepeat (a /\ b) =
    valBackgroundRepeat a
      <> Val (\{ separator } -> "," <> separator)
      <> valBackgroundRepeat b

instance ValBackgroundRepeat (RepeatStyle a) where
  valBackgroundRepeat = val

instance propertyBackgroundRepeatCommonKeyword
  :: Property "backgroundRepeat" CommonKeyword where
  pval = const val

else instance propertyBackgroundRepeatVal
  :: ValBackgroundRepeat a
  => Property "backgroundRepeat" a where
  pval = const valBackgroundRepeat

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-background-attachment

data Local = Local
instance ToVal Local where val _ = val "local"
local = Local :: Local

class ValBackgroundAttachment (a :: Type) where
  valBackgroundAttachment :: a -> Val

instance valBackgroundAttachmentMultiple
  :: ( ValBackgroundAttachment a
     , ValBackgroundAttachment b
     )
  => ValBackgroundAttachment (a /\ b) where
    valBackgroundAttachment (a /\ b) =
      valBackgroundAttachment a
      <> Val (\{ separator } -> "," <> separator)
      <> valBackgroundAttachment b

instance ValBackgroundAttachment Fixed where
  valBackgroundAttachment = val

instance ValBackgroundAttachment Local where
  valBackgroundAttachment = val

instance ValBackgroundAttachment Scroll where
  valBackgroundAttachment = val

instance propertyBackgrondAttachmentCommonKeyword
  :: Property "backgroundAttachment" CommonKeyword where
  pval = const val

else instance propertyBackgrondAttachmentVal
  :: ValBackgroundAttachment a
  => Property "backgroundAttachment" a where
  pval = const valBackgroundAttachment

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-background-position

class ValBackgroundPosition (a :: Type) where
  valBackgroundPosition :: a -> Val

instance valBackgroundPositionMultiple
  :: ( ValBackgroundPosition a
     , ValBackgroundPosition b
     )
  => ValBackgroundPosition (a /\ b) where
  valBackgroundPosition (a /\ b) =
    valBackgroundPosition a
    <> Val (\{ separator } -> "," <> separator)
    <> valBackgroundPosition b

else instance valBackgroundPositionPosition
  :: ValBackgroundPosition Position where
  valBackgroundPosition = val

else instance valBackgroundPositionAt1 :: At1 a => ValBackgroundPosition a where
  valBackgroundPosition = val

instance propertyBackgroundPositionCommonKeyword
  :: Property "backgroundPosition" CommonKeyword where
  pval = const val

else instance propertyBackgroundPositionVal
  :: ValBackgroundPosition a
  => Property "backgroundPosition" a where
  pval = const valBackgroundPosition

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-background-clip

data PaddingBox = PaddingBox
instance ToVal PaddingBox where val _ = val "padding-box"
paddingBox = PaddingBox :: PaddingBox

class ValBackgroundClip (a :: Type) where
  valBackgroundClip :: a -> Val

instance valBackgroundClipMultiple
  :: ( ValBackgroundClip a
     , ValBackgroundClip b
     )
  => ValBackgroundClip (a /\ b) where
  valBackgroundClip (a /\ b) =
    valBackgroundClip a
    <> Val (\{ separator } -> "," <> separator)
    <> valBackgroundClip b

instance ValBackgroundClip BorderBox where
  valBackgroundClip = val

instance ValBackgroundClip PaddingBox where
  valBackgroundClip = val

instance ValBackgroundClip ContentBox where
  valBackgroundClip = val

instance propertyBackgroundClipCommonKeyword
  :: Property "backgroundClip" CommonKeyword where
  pval = const val

else instance propertyBackgroundClipVal
  :: ValBackgroundClip a
  => Property "backgroundClip" a where
  pval = const valBackgroundClip

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-background-origin

instance propertyBackgroundOriginBackgroundClip
  :: Property "backgroundClip" a
  => Property "backgroundOrigin" a where
  pval = const $ pval (Proxy :: _ "backgroundClip")

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-background-size

newtype BgSize = BgSize Val

derive newtype instance ToVal BgSize

class (ToVal x, ToVal y) <= BgSize2 (x :: Type) (y :: Type)
instance LengthPercentageTag x => BgSize2 Auto Auto
instance LengthPercentageTag x => BgSize2 (Measure x) Auto
instance LengthPercentageTag y => BgSize2 Auto (Measure y)
instance
  ( LengthPercentageTag x
  , LengthPercentageTag y
  )
  => BgSize2 (Measure x) (Measure y)

bgSize2 :: forall x y. BgSize2 x y => x -> y -> BgSize
bgSize2 x y = BgSize $ val x <> val " " <> val y

class ValBackgroundSize (a :: Type) where
  valBackgroundSize :: a -> Val

instance valBackgroundSizeMultiple
  :: ( ValBackgroundSize a
     , ValBackgroundSize b
     )
  => ValBackgroundSize (a /\ b) where
  valBackgroundSize (a /\ b) =
    valBackgroundSize a
    <> Val (\{ separator } -> "," <> separator)
    <> valBackgroundSize b

instance LengthPercentageTag a => ValBackgroundSize (Measure a) where
  valBackgroundSize = val

instance ValBackgroundSize Auto where
  valBackgroundSize = val

instance ValBackgroundSize Cover where
  valBackgroundSize = val

instance ValBackgroundSize Contain where
  valBackgroundSize = val

instance ValBackgroundSize BgSize where
  valBackgroundSize = val

instance propertyBackgroundSizeCommonKeyword
  :: Property "backgroundSize" CommonKeyword where
  pval = const val

else instance propertyBackgroundSizeVal
  :: ValBackgroundSize a
  => Property "backgroundSize" a where
  pval = const valBackgroundSize

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-top-color

instance propertyBorderTopColorCommonKeyword
  :: Property "borderTopColor" CommonKeyword where
  pval = const val

else instance propertyBorderTopColorColor
  :: IsColor a
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

class ValBorderColor (a :: Type) where
  valBorderColor :: a -> Val

instance valBorderColor4
  :: ( IsColor a
     , IsColor b
     , IsColor c
     , IsColor d
     )
  => ValBorderColor (a /\ b /\ c /\ d) where
  valBorderColor (a /\ b /\ c /\ d) =
    joinVals (val " ") [val a, val b, val c, val d]

else instance valBorderColor3
  :: ( IsColor a
     , IsColor b
     , IsColor c
     )
  => ValBorderColor (a /\ b /\ c) where
  valBorderColor (a /\ b /\ c) = joinVals (val " ") [val a, val b, val c]

else instance valBorderColor2
  :: ( IsColor a
     , IsColor b
     )
  => ValBorderColor (a /\ b) where
  valBorderColor (a /\ b) = joinVals (val " ") [val a, val b]
   
else instance valBorderColor1
  :: IsColor a
  => ValBorderColor a where
  valBorderColor = val

instance propertyBorderColorCommonKeyword
  :: Property "borderColor" CommonKeyword where
  pval = const val

else instance propertyBorderColorVal
  :: ValBorderColor a
  => Property "borderColor" a where
  pval = const valBorderColor

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-top-style

newtype LineStyle = LineStyle String

derive newtype instance ToVal LineStyle

dotted :: LineStyle
dotted = LineStyle "dotted"

dashed :: LineStyle
dashed = LineStyle "dashed"

solid :: LineStyle
solid = LineStyle "solid"

double :: LineStyle
double = LineStyle "double"

groove :: LineStyle
groove = LineStyle "groove"

ridge :: LineStyle
ridge = LineStyle "ridge"

inset :: LineStyle
inset = LineStyle "inset"

outset :: LineStyle
outset = LineStyle "outset"

class ToVal a <= ValBorderTopStyle (a :: Type)

instance ValBorderTopStyle None
instance ValBorderTopStyle Hidden
instance ValBorderTopStyle LineStyle

instance propertyBorderTopStyleCommonKeyword
  :: Property "borderTopStyle" CommonKeyword where
  pval = const val

else instance propertyBorderTopStyleVal
  :: ValBorderTopStyle a
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

class ValBorderStyle (a :: Type) where
  valBorderStyle :: a -> Val

instance valBorderStyle4
  :: ( ValBorderTopStyle a
     , ValBorderTopStyle b
     , ValBorderTopStyle c
     , ValBorderTopStyle d
     )
  => ValBorderStyle (a /\ b /\ c /\ d) where
  valBorderStyle (a /\ b /\ c /\ d) =
    joinVals (val " ") [val a, val b, val c, val d]

else instance valBorderStyle3
  :: ( ValBorderTopStyle a
     , ValBorderTopStyle b
     , ValBorderTopStyle c
     )
  => ValBorderStyle (a /\ b /\ c) where
  valBorderStyle (a /\ b /\ c) = joinVals (val " ") [val a, val b, val c]

else instance valBorderStyle2
  :: ( ValBorderTopStyle a
     , ValBorderTopStyle b
     )
  => ValBorderStyle (a /\ b) where
  valBorderStyle (a /\ b) = joinVals (val " ") [val a, val b]

else instance valBorderStyle1
  :: ValBorderTopStyle a
  => ValBorderStyle a where
  valBorderStyle = val

instance propertyBorderStyleCommonKeyword
  :: Property "borderStyle" CommonKeyword where
  pval = const val

else instance propertyBorderStyleVal
  :: ValBorderStyle a
  => Property "borderStyle" a where
  pval = const valBorderStyle

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-top-width

newtype LineWidth = LineWidth String

derive newtype instance ToVal LineWidth

thin :: LineWidth
thin = LineWidth "thin"

thick :: LineWidth
thick = LineWidth "thick"

class ToVal a <= ValBorderTopWidth (a :: Type)
instance LengthTag a => ValBorderTopWidth (Measure a)
instance ValBorderTopWidth LineWidth
instance ValBorderTopWidth Medium

instance propertyBorderTopWidthCommonKeyword
  :: Property "borderTopWidth" CommonKeyword where
  pval = const val

else instance propertyBorderTopWidthVal
  :: ValBorderTopWidth a
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

class ValBorderWidth (a :: Type) where
  valBorderWidth :: a -> Val

instance valBorderWidth4
  :: ( ValBorderTopWidth a
     , ValBorderTopWidth b
     , ValBorderTopWidth c
     , ValBorderTopWidth d
     )
  => ValBorderWidth (a /\ b /\ c /\ d) where
  valBorderWidth (a /\ b /\ c /\ d) =
    joinVals (val " ") [val a, val b, val c, val d]

else instance valBorderWidth3
  :: ( ValBorderTopWidth a
     , ValBorderTopWidth b
     , ValBorderTopWidth c
     )
  => ValBorderWidth (a /\ b /\ c) where
  valBorderWidth (a /\ b /\ c) = joinVals (val " ") [val a, val b, val c]

else instance valBorderWidth2
  :: ( ValBorderTopWidth a
     , ValBorderTopWidth b
     )
  => ValBorderWidth (a /\ b) where
  valBorderWidth (a /\ b) = val a <> val " " <> val b

else instance valBorderWidth1 :: ValBorderTopWidth a => ValBorderWidth a where
  valBorderWidth = val

instance propertyBorderWidthCommonKeyword
  :: Property "borderWidth" CommonKeyword where
  pval = const val

else instance propertyBorderWidthVal
  :: ValBorderWidth a
  => Property "borderWidth" a where
  pval = const valBorderWidth

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-top

class ValBorderTop (a :: Type) where
  valBorderTop :: a -> Val

instance valBorderTopLengthNone
  :: ( LengthTag a
     , IsColor c
     )
  => ValBorderTop (Measure a /\ None /\ c) where
  valBorderTop (a /\ b /\ c) = joinVals (val " ") [val a, val b, val c]

instance valBorderTopLineWidthNone
  :: IsColor c
  => ValBorderTop (LineWidth /\ None /\ c) where
  valBorderTop (a /\ b /\ c) = joinVals (val " ") [val a, val b, val c]

instance valBorderTopMediumNone
  :: IsColor c
  => ValBorderTop (Medium /\ None /\ c) where
  valBorderTop (a /\ b /\ c) = joinVals (val " ") [val a, val b, val c]

instance valBorderTopLengthHidden
  :: ( LengthTag a
     , IsColor c
     )
  => ValBorderTop (Measure a /\ Hidden /\ c) where
  valBorderTop (a /\ b /\ c) = joinVals (val " ") [val a, val b, val c]

instance valBorderTopLineWidthHidden
  :: IsColor c
  => ValBorderTop (LineWidth /\ Hidden /\ c) where
  valBorderTop (a /\ b /\ c) = joinVals (val " ") [val a, val b, val c]

instance valBorderTopMediumHidden
  :: IsColor c
  => ValBorderTop (Medium /\ Hidden /\ c) where
  valBorderTop (a /\ b /\ c) = joinVals (val " ") [val a, val b, val c]

instance valBorderTopLengthLineStyle
  :: ( LengthTag a
     , IsColor c
     )
  => ValBorderTop (Measure a /\ LineStyle /\ c) where
  valBorderTop (a /\ b /\ c) = joinVals (val " ") [val a, val b, val c]

instance valBorderTopLineWidthLineStyle
  :: IsColor c
  => ValBorderTop (LineWidth /\ LineStyle /\ c) where
  valBorderTop (a /\ b /\ c) = joinVals (val " ") [val a, val b, val c]

instance valBorderTopMediumLineStyle
  :: IsColor c
  => ValBorderTop (Medium /\ LineStyle /\ c) where
  valBorderTop (a /\ b /\ c) = joinVals (val " ") [val a, val b, val c]

instance propertyBorderTopCommonKeyword
  :: Property "borderTop" CommonKeyword where
  pval = const val

else instance propertyBorderTopVal
  :: ValBorderTop a
  => Property "borderTop" a where
  pval = const valBorderTop

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-right

instance propertyBorderRightBorderTop
  :: Property "borderTop" a
  => Property "borderRight" a where
  pval = const $ pval (Proxy :: _ "borderTop")

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-bottom

instance propertyBorderBottomBorderTop
  :: Property "borderTop" a
  => Property "borderBottom" a where
  pval = const $ pval (Proxy :: _ "borderTop")

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-left

instance propertyBorderLeftBorderTop
  :: Property "borderTop" a
  => Property "borderLeft" a where
  pval = const $ pval (Proxy :: _ "borderTop")

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border

class ValBorder (a :: Type) where
  valBorder :: a -> Val

instance valBorderBorderTop4
  :: ( ValBorderTop (a /\ b /\ c)
     , ValBorderTop (d /\ e /\ f)
     , ValBorderTop (g /\ h /\ i)
     , ValBorderTop (j /\ k /\ l)
     )
  => ValBorder (a /\ b /\ c /\ d /\ e /\ f /\ g /\ h /\ i /\ j /\ k /\ l) where
  valBorder (a /\ b /\ c /\ d /\ e /\ f /\ g /\ h /\ i /\ j /\ k /\ l) =
    joinVals
    (Val \{ separator } -> "," <> separator)
    [ valBorderTop $ a /\ b /\ c
    , valBorderTop $ d /\ e /\ f
    , valBorderTop $ g /\ h /\ i
    , valBorderTop $ j /\ k /\ l
    ]

else instance valBorderBorderTop3
  :: ( ValBorderTop (a /\ b /\ c)
     , ValBorderTop (d /\ e /\ f)
     , ValBorderTop (g /\ h /\ i)
     )
  => ValBorder (a /\ b /\ c /\ d /\ e /\ f /\ g /\ h /\ i) where
  valBorder (a /\ b /\ c /\ d /\ e /\ f /\ g /\ h /\ i) =
    joinVals
    (Val \{ separator } -> "," <> separator)
    [ valBorderTop $ a /\ b /\ c
    , valBorderTop $ d /\ e /\ f
    , valBorderTop $ g /\ h /\ i
    ]

else instance valBorderBorderTop2
  :: ( ValBorderTop (a /\ b /\ c)
     , ValBorderTop (d /\ e /\ f)
     )
  => ValBorder (a /\ b /\ c /\ d /\ e /\ f) where
  valBorder (a /\ b /\ c /\ d /\ e /\ f) =
    joinVals
    (Val \{ separator } -> "," <> separator)
    [ valBorderTop $ a /\ b /\ c
    , valBorderTop $ d /\ e /\ f
    ]

else instance valBorderBorderTop1
  :: ValBorderTop (a /\ b /\ c)
  => ValBorder (a /\ b /\ c) where
  valBorder = valBorderTop

instance propertyBorderCommonKeyword
  :: Property "border" CommonKeyword where
  pval = const val

else instance propertyBorderVal
  :: ValBorder a
  => Property "border" a where
  pval = const valBorder

-- https://www.w3.org/TR/css-backgrounds-3/#propdef-border-top-left-radius

class ValBorderTopLeftRadius (a :: Type) where
  valBorderTopLeftRadius :: a -> Val

instance valBorderTopLeftRadius2
  :: ( LengthPercentageTag a
     , LengthPercentageTag b
     )
  => ValBorderTopLeftRadius (Measure a /\ Measure b) where
  valBorderTopLeftRadius (a /\ b) = val a <> val " " <> val b

instance valBorderTopLeftRadius1
  :: LengthPercentageTag a
  => ValBorderTopLeftRadius (Measure a) where
  valBorderTopLeftRadius = val

instance propertyBorderTopLeftRadiusCommonKeyword
  :: Property "borderTopLeftRadius" CommonKeyword where
  pval = const val

else instance propertyBorderTopLeftRadiusVal
  :: ValBorderTopLeftRadius a
  => Property "borderTopLeftRadius" a where
  pval = const valBorderTopLeftRadius

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

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/css-box-3/

-- https://www.w3.org/TR/css-box-3/#propdef-margin-top

class ValMarginTop (a :: Type) where
  valMarginTop :: a -> Val

instance valMarginTopLengthPercentage
  :: LengthPercentageTag a
  => ValMarginTop (Measure a) where
  valMarginTop = val

instance valMarginTopAuto :: ValMarginTop Auto where
  valMarginTop = val

instance propertyMarginTopCommonKeyword
  :: Property "marginTop" CommonKeyword where
  pval = const val

else instance propertyMarginTopVal
  :: ValMarginTop a
  => Property "marginTop" a where
  pval = const valMarginTop

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

instance propertyMarginValMarginTop4
  :: ( ValMarginTop a
     , ValMarginTop b
     , ValMarginTop c
     , ValMarginTop d
     )
  => Property "margin" (a /\ b /\ c /\ d) where
  pval _ (a /\ b /\ c /\ d) =
    joinVals
      (val " ")
      [ valMarginTop a
      , valMarginTop b
      , valMarginTop c
      , valMarginTop d
      ]

else instance propertyMarginValMarginTop3
  :: ( ValMarginTop a
     , ValMarginTop b
     , ValMarginTop c
     )
  => Property "margin" (a /\ b /\ c) where
  pval _ (a /\ b /\ c) =
    joinVals (val " ") [valMarginTop a, valMarginTop b, valMarginTop c]

else instance propertyMarginValMarginTop2
  :: ( ValMarginTop a
     , ValMarginTop b
     )
  => Property "margin" (a /\ b) where
  pval _ (a /\ b) = joinVals (val " ") [valMarginTop a, valMarginTop b]

else instance propertyMarginMarginTop
  :: Property "marginTop" a
  => Property "margin" a where
  pval = const $ pval (Proxy :: _ "marginTop")

-- https://www.w3.org/TR/css-box-3/#propdef-padding-top

instance propertyPaddingTopCommonKeyword
  :: Property "paddingTop" CommonKeyword where
  pval = const val

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
  :: ( LengthPercentageTag a
     , LengthPercentageTag b
     , LengthPercentageTag c
     , LengthPercentageTag d
     )
  => Property "padding" (Measure a /\ Measure b /\ Measure c /\ Measure d) where
  pval _ (a /\ b /\ c /\ d) = joinVals (val " ") [val a, val b, val c, val d]

else instance paddingLengthPercentage3
  :: ( LengthPercentageTag a
     , LengthPercentageTag b
     , LengthPercentageTag c
     )
  => Property "padding" (Measure a /\ Measure b /\ Measure c) where
  pval _ (a /\ b /\ c) = joinVals (val " ") [val a, val b, val c]

else instance paddingLengthPercentage2
  :: ( LengthPercentageTag a
     , LengthPercentageTag b
     )
  => Property "padding" (Measure a /\ Measure b) where
  pval _ (a /\ b) = joinVals (val " ") [val a, val b]

else instance paddingPaddingTop
  :: Property "paddingTop" a
  => Property "padding" a where
  pval = const $ pval (Proxy :: _ "paddingTop")

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/css-color-4/

-- https://www.w3.org/TR/css-color-4/#propdef-color

instance propertyColorCommonKeyword :: Property "color" CommonKeyword where
  pval = const val

else instance propertyColorIsColor :: IsColor a => Property "color" a where
  pval = const val

-- https://www.w3.org/TR/css-color-4/#propdef-opacity

instance propertyOpacityCommonKeyword :: Property "opacity" CommonKeyword where
  pval = const val

else instance propertyOpacityNumber :: Property "opacity" Number where
  pval = const val

-- https://www.w3.org/TR/css-color-4/#typedef-color

newtype CSSColor = CSSColor String

derive newtype instance ToVal CSSColor

currentColor :: CSSColor
currentColor = CSSColor "currentColor"

transparent :: CSSColor
transparent = CSSColor "transparent"

instance ToVal Color where val c = Val \cfg -> cfg.color c

class ToVal a <= IsColor (a :: Type)
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

-- https://www.w3.org/TR/css-images-3/

-- https://www.w3.org/TR/css-images-3/#typedef-image

class ToVal a <= IsImage (a :: Type)
instance IsImage URL

data Repeating

data ColorStop

data TransitionHint

class IsColorStopListItem (a :: Type)
instance IsColorStopListItem ColorStop
instance IsColorStopListItem TransitionHint

newtype Gradient (repeating :: Type) (previous :: Type) (last :: Type) =
  Gradient (String /\ List Val)

instance
  IsColorStopListItem previous
  => ToVal (Gradient repeating previous ColorStop) where
  val (Gradient (f /\ args)) = fn f $ Array.reverse $ Array.fromFoldable args

instance IsColorStopListItem previous
  => IsImage (Gradient repeating previous ColorStop)

mkGradient :: String -> Gradient Unit Unit Unit
mkGradient ty = Gradient $ (ty <> "-gradient") /\ Nil

repeating :: Gradient Unit Unit Unit -> Gradient Repeating Unit Unit
repeating (Gradient (f /\ args)) = Gradient $ ("repeating-" <> f) /\ args

addGradientDetail
  :: forall a repeating previous last next
   . ToVal a
  => a
  -> Gradient repeating previous last
  -> Gradient repeating last next
addGradientDetail arg (Gradient (f /\ args)) = Gradient $ f /\ val arg : args
 
stop
  :: forall color repeating previous last
   . IsColor color
  => color
  -> Gradient repeating previous last
  -> Gradient repeating last ColorStop
stop = addGradientDetail

stop2
  :: forall color lengthPercentage repeating previous last
   . IsColor color
  => LengthPercentageTag lengthPercentage
  => color
  -> Measure lengthPercentage
  -> Gradient repeating previous last
  -> Gradient repeating last ColorStop
stop2 color pos = addGradientDetail $ val color <> val " " <> val pos

hint
  :: forall lengthPercentage repeating previous
   . LengthPercentageTag lengthPercentage
  => Measure lengthPercentage
  -> Gradient repeating previous ColorStop
  -> Gradient repeating ColorStop TransitionHint
hint = addGradientDetail

newtype GradientAngle = GradientAngle Val

derive newtype instance ToVal GradientAngle

class ToVal a <= To1 (a :: Type)
instance To1 Top
instance To1 Right
instance To1 Bottom
instance To1 Left

to :: forall a. To1 a => a -> GradientAngle
to = GradientAngle <<< (val "to " <> _) <<< val

class (ToVal y, ToVal x) <= To2 (y :: Type) (x :: Type)
instance To2 Top Left
instance To2 Top Right
instance To2 Bottom Left
instance To2 Bottom Right

to2 :: forall y x. To2 y x => y -> x -> GradientAngle
to2 y x = GradientAngle $ val "to " <> val y <> val " " <> val x

class ToVal a <= IsGradientAngle (a :: Type)
instance IsGradientAngle GradientAngle
instance AngleTag a => IsGradientAngle (Measure a)

-- https://www.w3.org/TR/css-images-3/#linear-gradient-syntax

linearGradient :: Gradient Unit Unit Unit
linearGradient = mkGradient "linear"

linearGradient1
  :: forall angle
   . IsGradientAngle angle
  => angle
  -> Gradient Unit Unit Unit
linearGradient1 = flip addGradientDetail linearGradient

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

data Ellipse = Ellipse
instance ToVal Ellipse where val _ = val "ellipse"
ellipse = Ellipse :: Ellipse

radialGradient :: Gradient Unit Unit Unit
radialGradient = mkGradient "radial"
  
class RadialGradient1 (a :: Type) where
  radialGradient1 :: a -> Gradient Unit Unit Unit

instance RadialGradient1 Circle where
  radialGradient1 = flip addGradientDetail radialGradient

instance RadialGradient1 Ellipse where
  radialGradient1 = flip addGradientDetail radialGradient

instance RadialGradient1 Extent where
  radialGradient1 = flip addGradientDetail radialGradient

instance RadialGradient1 Position where
  radialGradient1 =
    flip addGradientDetail radialGradient <<< (val "at " <> _) <<< val

instance LengthTag length => RadialGradient1 (Measure length) where
  radialGradient1 = flip addGradientDetail radialGradient

class RadialGradient2 (a :: Type) (b :: Type) where
  radialGradient2 :: a -> b -> Gradient Unit Unit Unit

instance RadialGradient2 Circle Extent where
  radialGradient2 endingShape extent =
    radialGradient
      # addGradientDetail (val endingShape <> val " " <> val extent)

instance RadialGradient2 Ellipse Extent where
  radialGradient2 endingShape extent =
    radialGradient
      # addGradientDetail (val endingShape <> val " " <> val extent)

instance RadialGradient2 Circle Position where
  radialGradient2 endingShape position =
    radialGradient
      # addGradientDetail (val endingShape <> val " at " <> val position)

instance RadialGradient2 Ellipse Position where
  radialGradient2 endingShape position =
    radialGradient
      # addGradientDetail (val endingShape <> val " at " <> val position)

instance LengthTag length => RadialGradient2 Circle (Measure length) where
  radialGradient2 endingShape radius =
    radialGradient
      # addGradientDetail (val endingShape <> val " " <> val radius)

instance
  ( LengthPercentageTag lengthPercentage1
  , LengthPercentageTag lengthPercentage2
  ) => RadialGradient2 (Measure lengthPercentage1) (Measure lengthPercentage2) where
  radialGradient2 x y =
    radialGradient # addGradientDetail (val x <> val " " <> val y)

class RadialGradient3 (a :: Type) (b :: Type) (c :: Type) where
  radialGradient3 :: a -> b -> c -> Gradient Unit Unit Unit

instance RadialGradient3 Circle Extent Position where
  radialGradient3 endingShape extent position =
    radialGradient
      # addGradientDetail
        ( joinVals
            (val " ")
            [ val endingShape
            , val extent
            , val "at"
            , val position
            ]
        )

instance RadialGradient3 Ellipse Extent Position where
  radialGradient3 endingShape extent position =
    radialGradient
      # addGradientDetail
        ( joinVals
            (val " ")
            [ val endingShape
            , val extent
            , val "at"
            , val position
            ]
        )

instance LengthTag length => RadialGradient3 Circle (Measure length) Position where
  radialGradient3 endingShape radius position =
    radialGradient
      # addGradientDetail
        ( joinVals
            (val " ")
            [ val endingShape
            , val radius
            , val "at"
            , val position
            ]
        )

instance
  ( LengthPercentageTag lengthPercentage1
  , LengthPercentageTag lengthPercentage2
  )
  => RadialGradient3 (Measure lengthPercentage1) (Measure lengthPercentage2) Position where
  radialGradient3 x y position =
    radialGradient
      # addGradientDetail
        ( joinVals
            (val " ")
            [ val x
            , val y
            , val "at"
            , val position
            ]
        )

instance
  ( LengthPercentageTag lengthPercentage1
  , LengthPercentageTag lengthPercentage2
  ) => RadialGradient3 Ellipse (Measure lengthPercentage1) (Measure lengthPercentage2) where
  radialGradient3 endingShape x y =
    radialGradient
      # addGradientDetail (joinVals (val " ") [val endingShape, val x, val y])

radialGradient4
  :: forall x y
   . LengthPercentageTag x
  => LengthPercentageTag y
  => Ellipse
  -> (Measure x)
  -> (Measure y)
  -> Position
  -> Gradient Unit Unit Unit
radialGradient4 endingShape x y position =
  radialGradient
    # addGradientDetail
      ( joinVals
          (val " ")
          [ val endingShape
          , val x
          , val y
          , val "at"
          , val position
          ]
      )

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/css-sizing-3/

-- https://www.w3.org/TR/css-sizing-3/#propdef-width

instance propertyWidthCommonKeyword :: Property "width" CommonKeyword where
  pval = const val

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

instance propertyMaxWidthCommonKeyword
  :: Property "maxWidth" CommonKeyword where
  pval = const val

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

-- Misc/common words

-- WARNING: The following is generated code. Edit with care!

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

data Download = Download
instance ToVal Download where val _ = val "download"
download = Download :: Download
instance IsAttribute Download

data Draggable = Draggable
instance ToVal Draggable where val _ = val "draggable"
draggable = Draggable :: Draggable
instance IsAttribute Draggable

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

data List' = List'
instance ToVal List' where val _ = val "list"
list = List' :: List'
instance IsAttribute List'

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

data Usemap = Usemap
instance ToVal Usemap where val _ = val "usemap"
usemap = Usemap :: Usemap
instance IsAttribute Usemap

data Value = Value
instance ToVal Value where val _ = val "value"
value = Value :: Value
instance IsAttribute Value

data Width = Width
instance ToVal Width where val _ = val "width"
width = Width :: Width
instance IsAttribute Width

data Wrap = Wrap
instance ToVal Wrap where val _ = val "wrap"
wrap = Wrap :: Wrap
instance IsAttribute Wrap
