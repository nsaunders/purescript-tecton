module PSCSS where

import Prelude hiding (add)

import Color (Color, cssStringHSLA, toHexString)
import Control.Monad.Writer (Writer, execWriter, tell)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOption, convertOptionsWithDefaults)
import Data.Array (replicate)
import Data.Array as Array
import Data.Either (Either(..))
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex)
import Data.Int as Int
import Data.List (List, (:))
import Data.Maybe (Maybe(..))
import Data.Number.Format as Number
import Data.String as String
import Data.String.Regex (regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (global)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Prelude (class ListToRow)
import Type.Proxy (Proxy(..))

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

newtype Value = Value (Configuration -> String)

instance Semigroup Value where
  append (Value f) (Value g) = Value \x -> f x <> g x

instance Monoid Value where
  mempty = value ""

class ToValue (a :: Type) where
  value :: a -> Value

instance ToValue Value where
  value = identity

instance ToValue String where
  value = Value <<< const

instance ToValue Number where
  value = value <<< Number.toString

instance ToValue Int where
  value = value <<< Int.toNumber

instance (ToValue a, ToValue b) => ToValue (a /\ b) where
  value (a /\ b) = value a <> value " " <> value b

joinValues 
  :: forall s f
   . ToValue s
  => FoldableWithIndex Int f
  => s
  -> f Value
  -> Value
joinValues sep =
  foldlWithIndex
    (\i a b -> a <> (if i > 0 then value sep else mempty) <> b)
    mempty

runValue :: Configuration -> Value -> String
runValue x (Value f) = f x

--------------------------------------------------------------------------------

-- Supported style properties

data Declaration = Declaration

type SupportedDeclarations' (v :: Type) =
  ( color :: v
  , height :: v
  , maxHeight :: v
  , maxWidth :: v
  , minHeight :: v
  , minWidth :: v
  , opacity :: v
  , width :: v
  )

defaultDeclarations :: { | SupportedDeclarations }
defaultDeclarations =
  { color: Nothing
  , height: Nothing
  , maxHeight: Nothing
  , maxWidth: Nothing
  , minHeight: Nothing
  , minWidth: Nothing
  , opacity: Nothing
  , width: Nothing
  }

type SupportedDeclarations = SupportedDeclarations' (Maybe Value)

class ToValue v <= Property (p :: Symbol) (v :: Type)

instance Property p v => ConvertOption Declaration p v (Maybe Value) where
  convertOption _ _ = pure <<< value

--------------------------------------------------------------------------------

-- Style sheet structures

newtype NestedRule = NestedRule Value

newtype Selector = Selector Value

data Statement
  = NestedAtRule NestedRule (Array Statement)
  | Ruleset Selector Value

class Statement' (a :: Type) (b :: Type) | a -> b where
  statement :: a -> b -> Writer (Array Statement) Unit

instance Statement' NestedRule (Writer (Array Statement) Unit) where
  statement rule = tell <<< pure <<< NestedAtRule rule <<< execWriter

instance
  ConvertOptionsWithDefaults
    Declaration
    { | SupportedDeclarations }
    { | providedDeclarations }
    { | SupportedDeclarations }
  => Statement' Selector { | providedDeclarations } where
  statement selector provided =
    tell $ pure $ Ruleset selector $ declarationBlock provided

infixr 5 statement as ?

class CollectDeclarations (xs :: RowList Type) (row :: Row Type) where
  collectDeclarations :: Proxy xs -> Record row -> List Value

instance CollectDeclarations RL.Nil row where
  collectDeclarations _ _ = mempty

instance
  ( IsSymbol property
  , Row.Cons property (Maybe Value) tailRow row
  , Row.Lacks property tailRow
  , ListToRow tailRowList tailRow
  , CollectDeclarations tailRowList tailRow
  ) => CollectDeclarations (RL.Cons property (Maybe Value) tailRowList) row where
  collectDeclarations _ rec =
    let
      camelToKebab s =
        case regex "[A-Z]" global of
          Left _ ->
            s
          Right caps ->
            Regex.replace' caps (const <<< ("-" <> _) <<< String.toLower) s
      field = Proxy :: _ property
      rest =
        collectDeclarations (Proxy :: _ tailRowList) (Record.delete field rec)
    in
      case Record.get field rec of
        Just v ->
          let
            decl =
              Value \c@{ separator } ->
                runValue c (value $ camelToKebab $ reflectSymbol field)
                <> ":"
                <> separator
                <> runValue c v
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
  -> Value
declarationBlock provided =
  Value \config@{ newline, finalSemicolon } ->
    ( runValue config $
        joinValues
          (";" <> newline)
          ( map
            (\line ->
              Value \config'@{ indentLevel, indentation } ->
                (String.joinWith "" $ replicate indentLevel indentation)
                <>
                runValue config' line
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
    runValue config
      <<< joinValues config.newline
      <<< map renderStatement
      <<< execWriter

    where

    renderStatement =
      case _ of
        Ruleset (Selector selector) declarations ->
          nested selector declarations
        NestedAtRule (NestedRule nestedRule) statements ->
          nested (value "@" <> nestedRule) $
            joinValues config.newline (renderStatement <$> statements)

    nested outer inner =
      Value \c@{ indentLevel, indentation, newline, separator } ->
        let
          indent = String.joinWith mempty $ replicate indentLevel indentation
        in
          indent
          <> runValue c outer
          <> separator
          <> "{"
          <> newline
          <> runValue (c { indentLevel = indentLevel + 1 }) inner
          <> newline
          <> indent
          <> "}"

instance
  ConvertOptionsWithDefaults
    Declaration
    { | SupportedDeclarations }
    { | providedDeclarations }
    { | SupportedDeclarations }
  => Render (Record providedDeclarations) where
  render c =
    runValue c { newline = if c.newline /= mempty then " " else mempty }
      <<< declarationBlock

--------------------------------------------------------------------------------

-- Common values

fn :: forall f. FoldableWithIndex Int f => Value -> f Value -> Value
fn name args = Value \c ->
  let
    name' = runValue c name
    args' = runValue c $ joinValues ("," <> c.separator) args
  in
    name' <> "(" <> args' <> ")"

newtype Measure (tag :: Type) = Measure Value

instance ToValue (Measure tag) where
  value (Measure v) = v

measure :: forall a. String -> Number -> Measure a
measure u n = Measure $ value $ Number.toString n <> u

data Nil

nil :: Measure Nil
nil = Measure $ value 0

data Length

class ToLength a where
  ch :: a -> Measure Length
  em :: a -> Measure Length
  ex :: a -> Measure Length
  rem :: a -> Measure Length
  vh :: a -> Measure Length
  vw :: a -> Measure Length
  vmin :: a -> Measure Length
  vmax :: a -> Measure Length
  px :: a -> Measure Length
  cm :: a -> Measure Length
  mm :: a -> Measure Length
  pc :: a -> Measure Length
  pt :: a -> Measure Length
  inch :: a -> Measure Length

instance ToLength Number where
  ch = measure "ch"
  em = measure "em"
  ex = measure "ex"
  rem = measure "rem"
  vh = measure "vh"
  vw = measure "vw"
  vmin = measure "vmin"
  vmax = measure "vmax"
  px = measure "px"
  cm = measure "cm"
  mm = measure "mm"
  pc = measure "pc"
  pt = measure "pt"
  inch = measure "in"

instance ToLength Int where
  ch = ch <<< Int.toNumber
  em = em <<< Int.toNumber
  ex = ex <<< Int.toNumber
  rem = rem <<< Int.toNumber
  vh = vh <<< Int.toNumber
  vw = vw <<< Int.toNumber
  vmin = vmin <<< Int.toNumber
  vmax = vmax <<< Int.toNumber
  px = px <<< Int.toNumber
  cm = cm <<< Int.toNumber
  mm = mm <<< Int.toNumber
  pc = pc <<< Int.toNumber
  pt = pt <<< Int.toNumber
  inch = inch <<< Int.toNumber

class LengthTag (a :: Type)
instance LengthTag Length
instance LengthTag Nil

data Percentage

class ToPercentage a where
  pct :: a -> Measure Percentage

instance ToPercentage Number where
  pct = measure "%"

instance ToPercentage Int where
  pct = pct <<< Int.toNumber

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

class ToAngle a where
  deg :: a -> Measure Angle
  rad :: a -> Measure Angle
  turn :: a -> Measure Angle

instance ToAngle Number where
  deg = measure "deg"
  rad = measure "rad"
  turn = measure "turn"

instance ToAngle Int where
  deg = deg <<< Int.toNumber
  rad = rad <<< Int.toNumber
  turn = turn <<< Int.toNumber

class AngleTag (a :: Type)
instance AngleTag Angle
instance AngleTag Nil

data Time

class ToTime a where
  ms :: a -> Measure Time
  sec :: a -> Measure Time

instance ToTime Number where
  ms = measure "ms"
  sec = measure "s"

instance ToTime Int where
  ms = ms <<< Int.toNumber
  sec = sec <<< Int.toNumber

class TimeTag (a :: Type)
instance TimeTag Time
instance TimeTag Nil

data Add = Add
data Subtract = Subtract
data Multiply = Multiply
data Divide = Divide

instance ToValue Add where
  value = const $ value "+"

instance ToValue Subtract where
  value = const $ value "-"

instance ToValue Multiply where
  value = const $ value "*"

instance ToValue Divide where
  value = const $ value "/"

class Calc :: Type -> Type -> Type -> Type -> Constraint
class Calc op a b c | op -> c

instance Calc Add (Measure Length) (Measure Percentage) (Measure LengthPercentage)
else instance Calc Add (Measure Length) (Measure LengthPercentage) (Measure LengthPercentage)
else instance Calc Add (Measure Percentage) (Measure LengthPercentage) (Measure LengthPercentage)
else instance Calc Add a a a
else instance Calc Add a b c => Calc Add b a c
else instance Calc Add a b c => Calc Subtract a b c
else instance Calc Multiply a Number a
else instance Calc Multiply a Int a
else instance Calc Multiply a Number c => Calc Divide a Number c
else instance Calc Multiply a Int c => Calc Divide a Int c

calc
  :: forall op a b
   . ToValue op
  => ToValue a
  => ToValue b
  => op
  -> a
  -> b
  -> Value
calc op a b = fn (value "calc") [value $ a /\ op /\ b]

add
  :: forall a b c
   . Calc Add (Measure a) (Measure b) (Measure c)
  => Measure a
  -> Measure b
  -> Measure c
add a b = Measure $ calc Add a b

infixl 7 add as @+@

subtract
  :: forall a b c
   . Calc Subtract (Measure a) (Measure b) (Measure c)
  => Measure a
  -> Measure b
  -> Measure c
subtract a b = Measure $ calc Subtract a b

infixl 7 subtract as @-@

multiply
  :: forall a b
   . ToValue b
  => Calc Multiply (Measure a) b (Measure a)
  => Measure a
  -> b
  -> Measure a
multiply a b = Measure $ calc Multiply a b

infixl 8 multiply as @*

multiplyFlipped
  :: forall a b
   . ToValue b
  => Calc Multiply (Measure a) b (Measure a)
  => b
  -> Measure a
  -> Measure a
multiplyFlipped b a = Measure $ calc Multiply b a

infixl 8 multiplyFlipped as *@

divide
  :: forall a b
   . ToValue b
  => Calc Divide (Measure a) b (Measure a)
  => Measure a
  -> b
  -> Measure a
divide a b = Measure $ calc Divide a b

infixl 8 divide as @/

--------------------------------------------------------------------------------

-- Common keywords

newtype CommonKeyword = CommonKeyword String
inherit = CommonKeyword "inherit" :: CommonKeyword
initial = CommonKeyword "initial" :: CommonKeyword
unset = CommonKeyword "unset" :: CommonKeyword
instance ToValue CommonKeyword where value (CommonKeyword x) = value x

data All = All
all = All :: All
instance ToValue All where value _ = value "all"

data Auto = Auto
auto = Auto :: Auto
instance ToValue Auto where value _ = value "auto"

data None = None
none = None :: None
instance ToValue None where value _ = value "none"

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/mediaqueries-3/

newtype MediaType = MediaType String
print = MediaType "print" :: MediaType
screen = MediaType "screen" :: MediaType
derive newtype instance ToValue MediaType

class ToValue a <= IsMediaType (a :: Type)
instance IsMediaType All
instance IsMediaType MediaType

type SupportedMediaFeatures' (v :: Type) =
  ( minWidth :: v
  , maxWidth :: v
  , width :: v
  )

defaultMediaFeatures :: { | SupportedMediaFeatures }
defaultMediaFeatures =
  { maxWidth: Nothing
  , minWidth: Nothing
  , width: Nothing
  }

type SupportedMediaFeatures = SupportedMediaFeatures' (Maybe Value)

data MediaFeature' = MediaFeature'

class ToValue v <= MediaFeature (f :: Symbol) (v :: Type)
instance MediaFeature f v => ConvertOption MediaFeature' f v (Maybe Value) where
  convertOption _ _ = pure <<< value

instance mediaFeatureWidthLength :: LengthTag a => MediaFeature "width" (Measure a)
instance mediaFeatureMinWidthLength :: LengthTag a => MediaFeature "minWidth" (Measure a)
instance mediaFeatureMaxWidthLength :: LengthTag a => MediaFeature "maxWidth" (Measure a)

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
      joinValues ""
      $ map (\v -> value " and (" <> v <> value ")")
        $ Array.reverse
        $ Array.fromFoldable
        $ collectDeclarations (Proxy :: _ rl) $
            convertOptionsWithDefaults
              MediaFeature'
              defaultMediaFeatures
              providedMediaFeatures
  in
    NestedRule $ value "media " <> value mediaType <> features

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/selectors-4/

universal = Selector (value "*") :: Selector

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/css-color-4/

-- https://www.w3.org/TR/css-color-4/#propdef-color

instance propertyColorCommonKeyword :: Property "color" CommonKeyword
else instance propertyColorIsColor :: IsColor a => Property "color" a

-- https://www.w3.org/TR/css-color-4/#propdef-opacity

instance propertyOpacityCommonKeyword :: Property "opacity" CommonKeyword
else instance propertyOpacityNumber :: Property "opacity" Number

-- https://www.w3.org/TR/css-color-4/#typedef-color

newtype CSSColor = CSSColor String
currentColor = CSSColor "currentColor" :: CSSColor
transparent = CSSColor "transparent" :: CSSColor
derive newtype instance ToValue CSSColor

class ToValue a <= IsColor (a :: Type)
instance ToValue Color where value c = Value \cfg -> cfg.color c
instance IsColor Color
instance IsColor CSSColor

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/css-sizing-3/

-- https://www.w3.org/TR/css-sizing-3/#propdef-width

instance propertyWidthCommonKeyword :: Property "width" CommonKeyword

instance propertyWidthAuto :: Property "width" Auto

instance propertyWidthLengthPercentage :: LengthPercentageTag a => Property "width" (Measure a)

instance propertyWidthContentSizingValue :: Property "width" ContentSizingValue

-- https://www.w3.org/TR/css-sizing-3/#propdef-height

instance propertyHeightWidth :: Property "width" a => Property "height" a

-- https://www.w3.org/TR/css-sizing-3/#propdef-min-height

instance propertyMinHeightWidth :: Property "width" a => Property "minHeight" a

-- https://www.w3.org/TR/css-sizing-3/#propdef-min-width

instance propertyMinWidthWidth :: Property "width" a => Property "minWidth" a

-- https://www.w3.org/TR/css-sizing-3/#propdef-max-width

instance propertyMaxWidthCommonKeyword :: Property "maxWidth" CommonKeyword

instance propertyMaxWidthNone :: Property "maxWidth" None

instance propertyMaxWidthLengthPercentage :: LengthPercentageTag a => Property "maxWidth" (Measure a)

instance propertyMaxWidthContentSizingValue :: Property "maxWidth" ContentSizingValue

-- https://www.w3.org/TR/css-sizing-3/#propdef-max-height

instance propertyMaxHeightMaxWidth :: Property "maxWidth" a => Property "maxHeight" a

-- https://www.w3.org/TR/css-sizing-3/#sizing-values

newtype ContentSizingValue = ContentSizingValue Value

minContent = ContentSizingValue (value "min-content") :: ContentSizingValue
maxContent = ContentSizingValue (value "max-content") :: ContentSizingValue

fitContent :: forall a. LengthPercentageTag a => Measure a -> ContentSizingValue
fitContent a = ContentSizingValue (fn (value "fit-content") [value a])

derive newtype instance ToValue ContentSizingValue
