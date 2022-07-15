module PSCSS where

import Prelude hiding (add)

import Color (Color, cssStringHSLA, toHexString)
import Control.Monad.Writer (Writer, execWriter, tell)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Array (replicate)
import Data.Array as Array
import Data.Either (Either(..))
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex)
import Data.Int as Int
import Data.List (List, (:))
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

instance (ToVal a, ToVal b) => ToVal (a /\ b) where
  val (a /\ b) = val a <> val " " <> val b

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

type SupportedDeclarations = SupportedDeclarations' (Maybe Val)

class ToVal v <= Property (p :: Symbol) (v :: Type)

instance Property p v => ConvertOption Declaration p v (Maybe Val) where
  convertOption _ _ = pure <<< val

--------------------------------------------------------------------------------

-- Style sheet structures

newtype NestedRule = NestedRule Val

data Closed

newtype Selector (tag :: Type) = Selector Val

data Statement
  = NestedAtRule NestedRule (Array Statement)
  | Ruleset (NonEmpty Array (Selector Closed)) Val

class Statement' (a :: Type) (b :: Type) where
  statement :: a -> b -> Writer (Array Statement) Unit

instance Statement' NestedRule (Writer (Array Statement) Unit) where
  statement rule = tell <<< pure <<< NestedAtRule rule <<< execWriter
else instance
  ConvertOptionsWithDefaults
    Declaration
    { | SupportedDeclarations }
    { | providedDeclarations }
    { | SupportedDeclarations }
  => Statement' (NonEmpty Array (Selector tag)) { | providedDeclarations } where
  statement selectors provided =
    tell $ pure $ Ruleset (closeSelector <$> selectors) $ declarationBlock provided
else instance Statement' (NonEmpty Array a) b => Statement' a b where
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

instance
  ConvertOptionsWithDefaults
    Declaration
    { | SupportedDeclarations }
    { | providedDeclarations }
    { | SupportedDeclarations }
  => Render (Record providedDeclarations) where
  render c =
    runVal c { newline = if c.newline /= mempty then " " else mempty }
      <<< declarationBlock

--------------------------------------------------------------------------------

-- Common values

fn :: forall f. FoldableWithIndex Int f => Val -> f Val -> Val
fn name' args = Val \c ->
  let
    args' = runVal c $ joinVals ("," <> c.separator) args
  in
    runVal c name' <> "(" <> args' <> ")"

newtype Measure (tag :: Type) = Measure Val

instance ToVal (Measure tag) where
  val (Measure v) = v

measure :: forall a. String -> Number -> Measure a
measure u n = Measure $ val $ Number.toString n <> u

data Nil

nil :: Measure Nil
nil = Measure $ val 0

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
else instance Calc Multiply a Number a
else instance Calc Multiply a Int a
else instance Calc Multiply a Number c => Calc Divide a Number c
else instance Calc Multiply a Int c => Calc Divide a Int c

calc
  :: forall op a b
   . ToVal op
  => ToVal a
  => ToVal b
  => op
  -> a
  -> b
  -> Val
calc op a b = fn (val "calc") [val $ a /\ op /\ b]

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
   . ToVal b
  => Calc Multiply (Measure a) b (Measure a)
  => Measure a
  -> b
  -> Measure a
multiply a b = Measure $ calc Multiply a b

infixl 8 multiply as @*

multiplyFlipped
  :: forall a b
   . ToVal b
  => Calc Multiply (Measure a) b (Measure a)
  => b
  -> Measure a
  -> Measure a
multiplyFlipped b a = Measure $ calc Multiply b a

infixl 8 multiplyFlipped as *@

divide
  :: forall a b
   . ToVal b
  => Calc Divide (Measure a) b (Measure a)
  => Measure a
  -> b
  -> Measure a
divide a b = Measure $ calc Divide a b

infixl 8 divide as @/

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/mediaqueries-3/

newtype MediaType = MediaType String
print = MediaType "print" :: MediaType
screen = MediaType "screen" :: MediaType
derive newtype instance ToVal MediaType

class ToVal a <= IsMediaType (a :: Type)
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

type SupportedMediaFeatures = SupportedMediaFeatures' (Maybe Val)

data MediaFeature' = MediaFeature'

class ToVal v <= MediaFeature (f :: Symbol) (v :: Type)
instance MediaFeature f v => ConvertOption MediaFeature' f v (Maybe Val) where
  convertOption _ _ = pure <<< val

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

--------------------------------------------------------------------------------

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
      x =
        Val \c@{ separator } ->
          String.joinWith ("," <> separator)
            $ Array.fromFoldable
            $ (\(Selector s) -> runVal c s) <$> selectors
    in
      appendSelectorDetail $ val ":not(" <> x <> val ")"

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
derive newtype instance ToVal CSSColor

class ToVal a <= IsColor (a :: Type)
instance ToVal Color where val c = Val \cfg -> cfg.color c
instance IsColor Color
instance IsColor CSSColor

--------------------------------------------------------------------------------

-- https://www.w3.org/TR/css-sizing-3/

-- https://www.w3.org/TR/css-sizing-3/#propdef-width

instance propertyWidthCommonKeyword :: Property "width" CommonKeyword

instance propertyWidthAuto :: Property "width" Auto

instance propertyWidthLengthPercentage :: LengthPercentageTag a => Property "width" (Measure a)

instance propertyWidthContentSizingVal :: Property "width" ContentSizingVal

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

instance propertyMaxWidthContentSizingVal :: Property "maxWidth" ContentSizingVal

-- https://www.w3.org/TR/css-sizing-3/#propdef-max-height

instance propertyMaxHeightMaxWidth :: Property "maxWidth" a => Property "maxHeight" a

-- https://www.w3.org/TR/css-sizing-3/#sizing-values

newtype ContentSizingVal = ContentSizingVal Val

minContent = ContentSizingVal (val "min-content") :: ContentSizingVal
maxContent = ContentSizingVal (val "max-content") :: ContentSizingVal

fitContent :: forall a. LengthPercentageTag a => Measure a -> ContentSizingVal
fitContent a = ContentSizingVal (fn (val "fit-content") [val a])

derive newtype instance ToVal ContentSizingVal

--------------------------------------------------------------------------------

-- Common keywords

newtype CommonKeyword = CommonKeyword String
inherit = CommonKeyword "inherit" :: CommonKeyword
initial = CommonKeyword "initial" :: CommonKeyword
unset = CommonKeyword "unset" :: CommonKeyword
instance ToVal CommonKeyword where val (CommonKeyword x) = val x

data Accept = Accept
accept = Accept :: Accept
instance ToVal Accept where val _ = val "accept"
instance IsAttribute Accept

data AcceptCharset = AcceptCharset
acceptCharset = AcceptCharset :: AcceptCharset
instance ToVal AcceptCharset where val _ = val "accept-charset"
instance IsAttribute AcceptCharset

data Accesskey = Accesskey
accesskey = Accesskey :: Accesskey
instance ToVal Accesskey where val _ = val "accesskey"
instance IsAttribute Accesskey

data Action = Action
action = Action :: Action
instance ToVal Action where val _ = val "action"
instance IsAttribute Action

data All = All
all = All :: All
instance ToVal All where val _ = val "all"

data Alt = Alt
alt = Alt :: Alt
instance ToVal Alt where val _ = val "alt"
instance IsAttribute Alt

data Async = Async
async = Async :: Async
instance ToVal Async where val _ = val "async"
instance IsAttribute Async

data Auto = Auto
auto = Auto :: Auto
instance ToVal Auto where val _ = val "auto"

data Autocomplete = Autocomplete
autocomplete = Autocomplete :: Autocomplete
instance ToVal Autocomplete where val _ = val "autocomplete"
instance IsAttribute Autocomplete

data Autofocus = Autofocus
autofocus = Autofocus :: Autofocus
instance ToVal Autofocus where val _ = val "autofocus"
instance IsAttribute Autofocus

data Autoplay = Autoplay
autoplay = Autoplay :: Autoplay
instance ToVal Autoplay where val _ = val "autoplay"
instance IsAttribute Autoplay

data Charset = Charset
charset = Charset :: Charset
instance ToVal Charset where val _ = val "charset"
instance IsAttribute Charset

data Checked = Checked
checked = Checked :: Checked
instance ToVal Checked where val _ = val "checked"
instance IsAttribute Checked

data Cite = Cite
cite = Cite :: Cite
instance ToVal Cite where val _ = val "cite"
instance IsAttribute Cite

data Class = Class
class' = Class :: Class
instance ToVal Class where val _ = val "class"
instance IsAttribute Class

data Cols = Cols
cols = Cols :: Cols
instance ToVal Cols where val _ = val "cols"
instance IsAttribute Cols

data Colspan = Colspan
colspan = Colspan :: Colspan
instance ToVal Colspan where val _ = val "colspan"
instance IsAttribute Colspan

data Content = Content
content = Content :: Content
instance ToVal Content where val _ = val "content"
instance IsAttribute Content

data Contenteditable = Contenteditable
contenteditable = Contenteditable :: Contenteditable
instance ToVal Contenteditable where val _ = val "contenteditable"
instance IsAttribute Contenteditable

data Controls = Controls
controls = Controls :: Controls
instance ToVal Controls where val _ = val "controls"
instance IsAttribute Controls

data Coords = Coords
coords = Coords :: Coords
instance ToVal Coords where val _ = val "coords"
instance IsAttribute Coords

data Data = Data
data' = Data :: Data
instance ToVal Data where val _ = val "data"
instance IsAttribute Data

data Datetime = Datetime
datetime = Datetime :: Datetime
instance ToVal Datetime where val _ = val "datetime"
instance IsAttribute Datetime

data Default = Default
default = Default :: Default
instance ToVal Default where val _ = val "default"
instance IsAttribute Default

data Defer = Defer
defer = Defer :: Defer
instance ToVal Defer where val _ = val "defer"
instance IsAttribute Defer

data Dir = Dir
dir = Dir :: Dir
instance ToVal Dir where val _ = val "dir"
instance IsAttribute Dir

data Dirname = Dirname
dirname = Dirname :: Dirname
instance ToVal Dirname where val _ = val "dirname"
instance IsAttribute Dirname

data Disabled = Disabled
disabled = Disabled :: Disabled
instance ToVal Disabled where val _ = val "disabled"
instance IsAttribute Disabled

data Download = Download
download = Download :: Download
instance ToVal Download where val _ = val "download"
instance IsAttribute Download

data Draggable = Draggable
draggable = Draggable :: Draggable
instance ToVal Draggable where val _ = val "draggable"
instance IsAttribute Draggable

data Enctype = Enctype
enctype = Enctype :: Enctype
instance ToVal Enctype where val _ = val "enctype"
instance IsAttribute Enctype

data For = For
for = For :: For
instance ToVal For where val _ = val "for"
instance IsAttribute For

data Form = Form
form = Form :: Form
instance ToVal Form where val _ = val "form"
instance IsAttribute Form

data Formaction = Formaction
formaction = Formaction :: Formaction
instance ToVal Formaction where val _ = val "formaction"
instance IsAttribute Formaction

data Headers = Headers
headers = Headers :: Headers
instance ToVal Headers where val _ = val "headers"
instance IsAttribute Headers

data Height = Height
height = Height :: Height
instance ToVal Height where val _ = val "height"
instance IsAttribute Height

data Hidden = Hidden
hidden = Hidden :: Hidden
instance ToVal Hidden where val _ = val "hidden"
instance IsAttribute Hidden

data High = High
high = High :: High
instance ToVal High where val _ = val "high"
instance IsAttribute High

data Href = Href
href = Href :: Href
instance ToVal Href where val _ = val "href"
instance IsAttribute Href

data Hreflang = Hreflang
hreflang = Hreflang :: Hreflang
instance ToVal Hreflang where val _ = val "hreflang"
instance IsAttribute Hreflang

data HttpEquiv = HttpEquiv
httpEquiv = HttpEquiv :: HttpEquiv
instance ToVal HttpEquiv where val _ = val "http-equiv"
instance IsAttribute HttpEquiv

data Id = Id
id = Id :: Id
instance ToVal Id where val _ = val "id"
instance IsAttribute Id

data Ismap = Ismap
ismap = Ismap :: Ismap
instance ToVal Ismap where val _ = val "ismap"
instance IsAttribute Ismap

data Kind = Kind
kind = Kind :: Kind
instance ToVal Kind where val _ = val "kind"
instance IsAttribute Kind

data Label = Label
label = Label :: Label
instance ToVal Label where val _ = val "label"
instance IsAttribute Label

data Lang = Lang
lang = Lang :: Lang
instance ToVal Lang where val _ = val "lang"
instance IsAttribute Lang

data List' = List'
list = List' :: List'
instance ToVal List' where val _ = val "list"
instance IsAttribute List'

data Loop = Loop
loop = Loop :: Loop
instance ToVal Loop where val _ = val "loop"
instance IsAttribute Loop

data Low = Low
low = Low :: Low
instance ToVal Low where val _ = val "low"
instance IsAttribute Low

data Max = Max
max = Max :: Max
instance ToVal Max where val _ = val "max"
instance IsAttribute Max

data Maxlength = Maxlength
maxlength = Maxlength :: Maxlength
instance ToVal Maxlength where val _ = val "maxlength"
instance IsAttribute Maxlength

data Media = Media
media' = Media :: Media
instance ToVal Media where val _ = val "media"
instance IsAttribute Media

data Method = Method
method = Method :: Method
instance ToVal Method where val _ = val "method"
instance IsAttribute Method

data Min = Min
min = Min :: Min
instance ToVal Min where val _ = val "min"
instance IsAttribute Min

data Multiple = Multiple
multiple = Multiple :: Multiple
instance ToVal Multiple where val _ = val "multiple"
instance IsAttribute Multiple

data Muted = Muted
muted = Muted :: Muted
instance ToVal Muted where val _ = val "muted"
instance IsAttribute Muted

data Name = Name
name = Name :: Name
instance ToVal Name where val _ = val "name"
instance IsAttribute Name

data None = None
none = None :: None
instance ToVal None where val _ = val "none"

data Novalidate = Novalidate
novalidate = Novalidate :: Novalidate
instance ToVal Novalidate where val _ = val "novalidate"
instance IsAttribute Novalidate

data Onabort = Onabort
onabort = Onabort :: Onabort
instance ToVal Onabort where val _ = val "onabort"
instance IsAttribute Onabort

data Onafterprint = Onafterprint
onafterprint = Onafterprint :: Onafterprint
instance ToVal Onafterprint where val _ = val "onafterprint"
instance IsAttribute Onafterprint

data Onbeforeprint = Onbeforeprint
onbeforeprint = Onbeforeprint :: Onbeforeprint
instance ToVal Onbeforeprint where val _ = val "onbeforeprint"
instance IsAttribute Onbeforeprint

data Onbeforeunload = Onbeforeunload
onbeforeunload = Onbeforeunload :: Onbeforeunload
instance ToVal Onbeforeunload where val _ = val "onbeforeunload"
instance IsAttribute Onbeforeunload

data Onblur = Onblur
onblur = Onblur :: Onblur
instance ToVal Onblur where val _ = val "onblur"
instance IsAttribute Onblur

data Oncanplay = Oncanplay
oncanplay = Oncanplay :: Oncanplay
instance ToVal Oncanplay where val _ = val "oncanplay"
instance IsAttribute Oncanplay

data Oncanplaythrough = Oncanplaythrough
oncanplaythrough = Oncanplaythrough :: Oncanplaythrough
instance ToVal Oncanplaythrough where val _ = val "oncanplaythrough"
instance IsAttribute Oncanplaythrough

data Onchange = Onchange
onchange = Onchange :: Onchange
instance ToVal Onchange where val _ = val "onchange"
instance IsAttribute Onchange

data Onclick = Onclick
onclick = Onclick :: Onclick
instance ToVal Onclick where val _ = val "onclick"
instance IsAttribute Onclick

data Oncontextmenu = Oncontextmenu
oncontextmenu = Oncontextmenu :: Oncontextmenu
instance ToVal Oncontextmenu where val _ = val "oncontextmenu"
instance IsAttribute Oncontextmenu

data Oncopy = Oncopy
oncopy = Oncopy :: Oncopy
instance ToVal Oncopy where val _ = val "oncopy"
instance IsAttribute Oncopy

data Oncuechange = Oncuechange
oncuechange = Oncuechange :: Oncuechange
instance ToVal Oncuechange where val _ = val "oncuechange"
instance IsAttribute Oncuechange

data Oncut = Oncut
oncut = Oncut :: Oncut
instance ToVal Oncut where val _ = val "oncut"
instance IsAttribute Oncut

data Ondblclick = Ondblclick
ondblclick = Ondblclick :: Ondblclick
instance ToVal Ondblclick where val _ = val "ondblclick"
instance IsAttribute Ondblclick

data Ondrag = Ondrag
ondrag = Ondrag :: Ondrag
instance ToVal Ondrag where val _ = val "ondrag"
instance IsAttribute Ondrag

data Ondragend = Ondragend
ondragend = Ondragend :: Ondragend
instance ToVal Ondragend where val _ = val "ondragend"
instance IsAttribute Ondragend

data Ondragenter = Ondragenter
ondragenter = Ondragenter :: Ondragenter
instance ToVal Ondragenter where val _ = val "ondragenter"
instance IsAttribute Ondragenter

data Ondragleave = Ondragleave
ondragleave = Ondragleave :: Ondragleave
instance ToVal Ondragleave where val _ = val "ondragleave"
instance IsAttribute Ondragleave

data Ondragover = Ondragover
ondragover = Ondragover :: Ondragover
instance ToVal Ondragover where val _ = val "ondragover"
instance IsAttribute Ondragover

data Ondragstart = Ondragstart
ondragstart = Ondragstart :: Ondragstart
instance ToVal Ondragstart where val _ = val "ondragstart"
instance IsAttribute Ondragstart

data Ondrop = Ondrop
ondrop = Ondrop :: Ondrop
instance ToVal Ondrop where val _ = val "ondrop"
instance IsAttribute Ondrop

data Ondurationchange = Ondurationchange
ondurationchange = Ondurationchange :: Ondurationchange
instance ToVal Ondurationchange where val _ = val "ondurationchange"
instance IsAttribute Ondurationchange

data Onemptied = Onemptied
onemptied = Onemptied :: Onemptied
instance ToVal Onemptied where val _ = val "onemptied"
instance IsAttribute Onemptied

data Onended = Onended
onended = Onended :: Onended
instance ToVal Onended where val _ = val "onended"
instance IsAttribute Onended

data Onerror = Onerror
onerror = Onerror :: Onerror
instance ToVal Onerror where val _ = val "onerror"
instance IsAttribute Onerror

data Onfocus = Onfocus
onfocus = Onfocus :: Onfocus
instance ToVal Onfocus where val _ = val "onfocus"
instance IsAttribute Onfocus

data Onhashchange = Onhashchange
onhashchange = Onhashchange :: Onhashchange
instance ToVal Onhashchange where val _ = val "onhashchange"
instance IsAttribute Onhashchange

data Oninput = Oninput
oninput = Oninput :: Oninput
instance ToVal Oninput where val _ = val "oninput"
instance IsAttribute Oninput

data Oninvalid = Oninvalid
oninvalid = Oninvalid :: Oninvalid
instance ToVal Oninvalid where val _ = val "oninvalid"
instance IsAttribute Oninvalid

data Onkeydown = Onkeydown
onkeydown = Onkeydown :: Onkeydown
instance ToVal Onkeydown where val _ = val "onkeydown"
instance IsAttribute Onkeydown

data Onkeypress = Onkeypress
onkeypress = Onkeypress :: Onkeypress
instance ToVal Onkeypress where val _ = val "onkeypress"
instance IsAttribute Onkeypress

data Onkeyup = Onkeyup
onkeyup = Onkeyup :: Onkeyup
instance ToVal Onkeyup where val _ = val "onkeyup"
instance IsAttribute Onkeyup

data Onload = Onload
onload = Onload :: Onload
instance ToVal Onload where val _ = val "onload"
instance IsAttribute Onload

data Onloadeddata = Onloadeddata
onloadeddata = Onloadeddata :: Onloadeddata
instance ToVal Onloadeddata where val _ = val "onloadeddata"
instance IsAttribute Onloadeddata

data Onloadedmetadata = Onloadedmetadata
onloadedmetadata = Onloadedmetadata :: Onloadedmetadata
instance ToVal Onloadedmetadata where val _ = val "onloadedmetadata"
instance IsAttribute Onloadedmetadata

data Onloadstart = Onloadstart
onloadstart = Onloadstart :: Onloadstart
instance ToVal Onloadstart where val _ = val "onloadstart"
instance IsAttribute Onloadstart

data Onmousedown = Onmousedown
onmousedown = Onmousedown :: Onmousedown
instance ToVal Onmousedown where val _ = val "onmousedown"
instance IsAttribute Onmousedown

data Onmousemove = Onmousemove
onmousemove = Onmousemove :: Onmousemove
instance ToVal Onmousemove where val _ = val "onmousemove"
instance IsAttribute Onmousemove

data Onmouseout = Onmouseout
onmouseout = Onmouseout :: Onmouseout
instance ToVal Onmouseout where val _ = val "onmouseout"
instance IsAttribute Onmouseout

data Onmouseover = Onmouseover
onmouseover = Onmouseover :: Onmouseover
instance ToVal Onmouseover where val _ = val "onmouseover"
instance IsAttribute Onmouseover

data Onmouseup = Onmouseup
onmouseup = Onmouseup :: Onmouseup
instance ToVal Onmouseup where val _ = val "onmouseup"
instance IsAttribute Onmouseup

data Onmousewheel = Onmousewheel
onmousewheel = Onmousewheel :: Onmousewheel
instance ToVal Onmousewheel where val _ = val "onmousewheel"
instance IsAttribute Onmousewheel

data Onoffline = Onoffline
onoffline = Onoffline :: Onoffline
instance ToVal Onoffline where val _ = val "onoffline"
instance IsAttribute Onoffline

data Ononline = Ononline
ononline = Ononline :: Ononline
instance ToVal Ononline where val _ = val "ononline"
instance IsAttribute Ononline

data Onpagehide = Onpagehide
onpagehide = Onpagehide :: Onpagehide
instance ToVal Onpagehide where val _ = val "onpagehide"
instance IsAttribute Onpagehide

data Onpageshow = Onpageshow
onpageshow = Onpageshow :: Onpageshow
instance ToVal Onpageshow where val _ = val "onpageshow"
instance IsAttribute Onpageshow

data Onpaste = Onpaste
onpaste = Onpaste :: Onpaste
instance ToVal Onpaste where val _ = val "onpaste"
instance IsAttribute Onpaste

data Onpause = Onpause
onpause = Onpause :: Onpause
instance ToVal Onpause where val _ = val "onpause"
instance IsAttribute Onpause

data Onplay = Onplay
onplay = Onplay :: Onplay
instance ToVal Onplay where val _ = val "onplay"
instance IsAttribute Onplay

data Onplaying = Onplaying
onplaying = Onplaying :: Onplaying
instance ToVal Onplaying where val _ = val "onplaying"
instance IsAttribute Onplaying

data Onpopstate = Onpopstate
onpopstate = Onpopstate :: Onpopstate
instance ToVal Onpopstate where val _ = val "onpopstate"
instance IsAttribute Onpopstate

data Onprogress = Onprogress
onprogress = Onprogress :: Onprogress
instance ToVal Onprogress where val _ = val "onprogress"
instance IsAttribute Onprogress

data Onratechange = Onratechange
onratechange = Onratechange :: Onratechange
instance ToVal Onratechange where val _ = val "onratechange"
instance IsAttribute Onratechange

data Onreset = Onreset
onreset = Onreset :: Onreset
instance ToVal Onreset where val _ = val "onreset"
instance IsAttribute Onreset

data Onresize = Onresize
onresize = Onresize :: Onresize
instance ToVal Onresize where val _ = val "onresize"
instance IsAttribute Onresize

data Onscroll = Onscroll
onscroll = Onscroll :: Onscroll
instance ToVal Onscroll where val _ = val "onscroll"
instance IsAttribute Onscroll

data Onsearch = Onsearch
onsearch = Onsearch :: Onsearch
instance ToVal Onsearch where val _ = val "onsearch"
instance IsAttribute Onsearch

data Onseeked = Onseeked
onseeked = Onseeked :: Onseeked
instance ToVal Onseeked where val _ = val "onseeked"
instance IsAttribute Onseeked

data Onseeking = Onseeking
onseeking = Onseeking :: Onseeking
instance ToVal Onseeking where val _ = val "onseeking"
instance IsAttribute Onseeking

data Onselect = Onselect
onselect = Onselect :: Onselect
instance ToVal Onselect where val _ = val "onselect"
instance IsAttribute Onselect

data Onstalled = Onstalled
onstalled = Onstalled :: Onstalled
instance ToVal Onstalled where val _ = val "onstalled"
instance IsAttribute Onstalled

data Onstorage = Onstorage
onstorage = Onstorage :: Onstorage
instance ToVal Onstorage where val _ = val "onstorage"
instance IsAttribute Onstorage

data Onsubmit = Onsubmit
onsubmit = Onsubmit :: Onsubmit
instance ToVal Onsubmit where val _ = val "onsubmit"
instance IsAttribute Onsubmit

data Onsuspend = Onsuspend
onsuspend = Onsuspend :: Onsuspend
instance ToVal Onsuspend where val _ = val "onsuspend"
instance IsAttribute Onsuspend

data Ontimeupdate = Ontimeupdate
ontimeupdate = Ontimeupdate :: Ontimeupdate
instance ToVal Ontimeupdate where val _ = val "ontimeupdate"
instance IsAttribute Ontimeupdate

data Ontoggle = Ontoggle
ontoggle = Ontoggle :: Ontoggle
instance ToVal Ontoggle where val _ = val "ontoggle"
instance IsAttribute Ontoggle

data Onunload = Onunload
onunload = Onunload :: Onunload
instance ToVal Onunload where val _ = val "onunload"
instance IsAttribute Onunload

data Onvolumechange = Onvolumechange
onvolumechange = Onvolumechange :: Onvolumechange
instance ToVal Onvolumechange where val _ = val "onvolumechange"
instance IsAttribute Onvolumechange

data Onwaiting = Onwaiting
onwaiting = Onwaiting :: Onwaiting
instance ToVal Onwaiting where val _ = val "onwaiting"
instance IsAttribute Onwaiting

data Onwheel = Onwheel
onwheel = Onwheel :: Onwheel
instance ToVal Onwheel where val _ = val "onwheel"
instance IsAttribute Onwheel

data Open = Open
open = Open :: Open
instance ToVal Open where val _ = val "open"
instance IsAttribute Open

data Optimum = Optimum
optimum = Optimum :: Optimum
instance ToVal Optimum where val _ = val "optimum"
instance IsAttribute Optimum

data Pattern = Pattern
pattern = Pattern :: Pattern
instance ToVal Pattern where val _ = val "pattern"
instance IsAttribute Pattern

data Placeholder = Placeholder
placeholder = Placeholder :: Placeholder
instance ToVal Placeholder where val _ = val "placeholder"
instance IsAttribute Placeholder

data Poster = Poster
poster = Poster :: Poster
instance ToVal Poster where val _ = val "poster"
instance IsAttribute Poster

data Preload = Preload
preload = Preload :: Preload
instance ToVal Preload where val _ = val "preload"
instance IsAttribute Preload

data Readonly = Readonly
readonly = Readonly :: Readonly
instance ToVal Readonly where val _ = val "readonly"
instance IsAttribute Readonly

data Rel = Rel
rel = Rel :: Rel
instance ToVal Rel where val _ = val "rel"
instance IsAttribute Rel

data Required = Required
required = Required :: Required
instance ToVal Required where val _ = val "required"
instance IsAttribute Required

data Reversed = Reversed
reversed = Reversed :: Reversed
instance ToVal Reversed where val _ = val "reversed"
instance IsAttribute Reversed

data Rows = Rows
rows = Rows :: Rows
instance ToVal Rows where val _ = val "rows"
instance IsAttribute Rows

data Rowspan = Rowspan
rowspan = Rowspan :: Rowspan
instance ToVal Rowspan where val _ = val "rowspan"
instance IsAttribute Rowspan

data Sandbox = Sandbox
sandbox = Sandbox :: Sandbox
instance ToVal Sandbox where val _ = val "sandbox"
instance IsAttribute Sandbox

data Scope = Scope
scope = Scope :: Scope
instance ToVal Scope where val _ = val "scope"
instance IsAttribute Scope

data Selected = Selected
selected = Selected :: Selected
instance ToVal Selected where val _ = val "selected"
instance IsAttribute Selected

data Shape = Shape
shape = Shape :: Shape
instance ToVal Shape where val _ = val "shape"
instance IsAttribute Shape

data Size = Size
size = Size :: Size
instance ToVal Size where val _ = val "size"
instance IsAttribute Size

data Sizes = Sizes
sizes = Sizes :: Sizes
instance ToVal Sizes where val _ = val "sizes"
instance IsAttribute Sizes

data Span = Span
span = Span :: Span
instance ToVal Span where val _ = val "span"
instance IsAttribute Span

data Spellcheck = Spellcheck
spellcheck = Spellcheck :: Spellcheck
instance ToVal Spellcheck where val _ = val "spellcheck"
instance IsAttribute Spellcheck

data Src = Src
src = Src :: Src
instance ToVal Src where val _ = val "src"
instance IsAttribute Src

data Srcdoc = Srcdoc
srcdoc = Srcdoc :: Srcdoc
instance ToVal Srcdoc where val _ = val "srcdoc"
instance IsAttribute Srcdoc

data Srclang = Srclang
srclang = Srclang :: Srclang
instance ToVal Srclang where val _ = val "srclang"
instance IsAttribute Srclang

data Srcset = Srcset
srcset = Srcset :: Srcset
instance ToVal Srcset where val _ = val "srcset"
instance IsAttribute Srcset

data Start = Start
start = Start :: Start
instance ToVal Start where val _ = val "start"
instance IsAttribute Start

data Step = Step
step = Step :: Step
instance ToVal Step where val _ = val "step"
instance IsAttribute Step

data Style = Style
style = Style :: Style
instance ToVal Style where val _ = val "style"
instance IsAttribute Style

data Tabindex = Tabindex
tabindex = Tabindex :: Tabindex
instance ToVal Tabindex where val _ = val "tabindex"
instance IsAttribute Tabindex

data Target = Target
target = Target :: Target
instance ToVal Target where val _ = val "target"
instance IsAttribute Target

data Title = Title
title = Title :: Title
instance ToVal Title where val _ = val "title"
instance IsAttribute Title

data Translate = Translate
translate' = Translate :: Translate
instance ToVal Translate where val _ = val "translate"
instance IsAttribute Translate

data Type' = Type'
type' = Type' :: Type'
instance ToVal Type' where val _ = val "type"
instance IsAttribute Type'

data Usemap = Usemap
usemap = Usemap :: Usemap
instance ToVal Usemap where val _ = val "usemap"
instance IsAttribute Usemap

data Value = Value
value = Value :: Value
instance ToVal Value where val _ = val "value"
instance IsAttribute Value

data Width = Width
width = Width :: Width
instance ToVal Width where val _ = val "width"
instance IsAttribute Width

data Wrap = Wrap
wrap = Wrap :: Wrap
instance ToVal Wrap where val _ = val "wrap"
instance IsAttribute Wrap
