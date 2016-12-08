-- | Partially taken from Hugs AnsiScreen.hs library:
module Language.Haskell.HsColour.ANSI
  ( highlightOnG,highlightOn
  , highlightOff
  , highlightG,highlight
  , cleareol, clearbol, clearline, clearDown, clearUp, cls
  , goto
  , cursorUp, cursorDown, cursorLeft, cursorRight
  , savePosition, restorePosition
  , Highlight(..)
  , Colour(..)
  , colourCycle
  , enableScrollRegion, scrollUp, scrollDown
  , lineWrap
  , TerminalType(..)
  ) where

import Data.List (intersperse,isPrefixOf)
import Data.Char (isDigit)
import Data.Word

data TerminalType =
      Ansi16Colour -- ^ @\\033[Xm@-style escape sequences (with /X/ =~ [34][0-7])
    | XTerm256Compatible -- ^ 'Ansi16Colour', and also @\\033[Y8;5;Zm]@-style escape sequences (with /Y/ =~ [3,4] and /Z/ an integer in [0,255] with the XTerm colour cube semantics).
    deriving (Show,Eq,Ord)

-- | The supported output formats.
data Output = TTY   -- ^ ANSI terminal codes. Equivalent to 'TTYg' 'Ansi16Colour' but left in for backwards compatibility.
            | TTYg TerminalType -- ^ Terminal codes appropriate for the 'TerminalType'.
            | LaTeX -- ^ TeX macros
            | HTML  -- ^ HTML with font tags
            | CSS   -- ^ HTML with CSS.
            | ACSS  -- ^ HTML with CSS and mouseover types.
            | ICSS  -- ^ HTML with inline CSS.
            | MIRC  -- ^ mIRC chat clients
  deriving (Eq,Show)


-- | Colours supported by ANSI codes.
data Colour = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Rgb Word8 Word8 Word8
  deriving (Eq,Show,Read)

-- | Convert an integer in the range [0,2^24-1] to its base 256-triplet, passing the result to the given continuation (avoid unnecessary tupleism).
base256 :: Integral int => (Word8 -> Word8 -> Word8 -> r) -> int -> r
base256 kont x =
    let
        (r,gb) = divMod x 256
        (g,b)  = divMod gb 256
        fi = fromIntegral
    in
        kont (fi r) (fi g) (fi b)

-- | Convert a three-digit numeral in the given (as arg 1) base to its integer value.
unbase :: Integral int => int -> Word8 -> Word8 -> Word8 -> int
unbase base r g b = (fi r*base+fi g)*base+fi b
    where fi = fromIntegral

-- | Approximate a 24-bit Rgb colour with a colour in the xterm256 6x6x6 colour cube, returning its index.
rgb24bit_to_xterm256 :: (Integral t) => Word8 -> Word8 -> Word8 -> t
rgb24bit_to_xterm256 r g b = let f = (`div` 43)
                          in 16 + unbase 6 (f r) (f g) (f b)


-- | Ap\"proxi\"mate a 24-bit Rgb colour with an ANSI8 colour. Will leave other colours unchanged and will never return an 'Rgb' constructor value.
projectToBasicColour8 ::  Colour -> Colour
projectToBasicColour8 (Rgb r g b) = let f = (`div` 128)
                          in  toEnum ( unbase 2 (f r) (f g) (f b) )
projectToBasicColour8 x = x


-- | Lift 'projectToBasicColour8' to 'Highlight's
hlProjectToBasicColour8 ::  Highlight -> Highlight
hlProjectToBasicColour8 (Foreground c) = Foreground (projectToBasicColour8 c)
hlProjectToBasicColour8 (Background c) = Background (projectToBasicColour8 c)
hlProjectToBasicColour8 h = h

instance Enum Colour where
    toEnum 0 = Black
    toEnum 1 = Red
    toEnum 2 = Green
    toEnum 3 = Yellow
    toEnum 4 = Blue
    toEnum 5 = Magenta
    toEnum 6 = Cyan
    toEnum 7 = White
    -- Arbitrary extension; maybe just 'error' out instead
    toEnum x = base256 Rgb (x-8)

    fromEnum Black   = 0
    fromEnum Red     = 1
    fromEnum Green   = 2
    fromEnum Yellow  = 3
    fromEnum Blue    = 4
    fromEnum Magenta = 5
    fromEnum Cyan    = 6
    fromEnum White   = 7
    -- Arbitrary extension; maybe just 'error' out instead
    fromEnum (Rgb r g b) = 8 + unbase 256 r g b


-- | Types of highlighting supported by ANSI codes (and some extra styles).
data Highlight =
    Normal
  | Bold
  | Dim
  | Underscore
  | Blink
  | ReverseVideo
  | Concealed
  | Foreground Colour
  | Background Colour
  -- The above styles are ANSI-supported, with the exception of the 'Rgb' constructor for 'Colour's.  Below are extra styles (e.g. for Html rendering).
  | Italic
  deriving (Eq,Show,Read)


-- Basic screen control codes:

type Pos           = (Int,Int)

at        :: Pos -> String -> String
-- | Move the screen cursor to the given position.
goto      :: Int -> Int -> String
home      :: String
-- | Clear the screen.
cls       :: String

at (x,y) s  = goto x y ++ s
goto x y    = '\ESC':'[':(show y ++(';':show x ++ "H"))
home        = goto 1 1

cursorUp    = "\ESC[A"
cursorDown  = "\ESC[B"
cursorRight = "\ESC[C"
cursorLeft  = "\ESC[D"

cleareol    = "\ESC[K"
clearbol    = "\ESC[1K"
clearline   = "\ESC[2K"
clearDown   = "\ESC[J"
clearUp     = "\ESC[1J"
-- Choose whichever of the following lines is suitable for your system:
cls         = "\ESC[2J"     -- for PC with ANSI.SYS
--cls         = "\^L"         -- for Sun window

savePosition    = "\ESC7"
restorePosition = "\ESC8"


-- data Colour    -- imported from ColourHighlight
-- data Highlight -- imported from ColourHighlight

instance Enum Highlight where
  fromEnum Normal       = 0
  fromEnum Bold         = 1
  fromEnum Dim          = 2
  fromEnum Underscore   = 4
  fromEnum Blink        = 5
  fromEnum ReverseVideo = 7
  fromEnum Concealed    = 8
  -- The translation of these depends on the terminal type, and they don't translate to single numbers anyway. Should we really use the Enum class for this purpose rather than simply moving this table to 'renderAttrG'?
  fromEnum (Foreground (Rgb _ _ _)) = error "Internal error: fromEnum (Foreground (Rgb _ _ _))"
  fromEnum (Background (Rgb _ _ _)) = error "Internal error: fromEnum (Background (Rgb _ _ _))"
  fromEnum (Foreground c) = 30 + fromEnum c
  fromEnum (Background c) = 40 + fromEnum c
  fromEnum Italic       = 2


-- | = 'highlightG' 'Ansi16Colour'
highlight ::  [Highlight] -> String -> String
highlight = highlightG Ansi16Colour

-- | = 'highlightOn' 'Ansi16Colour'
highlightOn ::  [Highlight] -> String
highlightOn = highlightOnG Ansi16Colour


-- | Make the given string appear with all of the listed highlights
highlightG :: TerminalType -> [Highlight] -> String -> String
highlightG tt attrs s = highlightOnG tt attrs ++ s ++ highlightOff

highlightOnG :: TerminalType -> [Highlight] -> String
highlightOnG tt []     = highlightOnG tt [Normal]
highlightOnG tt attrs  = "\ESC["
                       ++ concat (intersperse ";" (concatMap (renderAttrG tt) attrs))
                       ++"m"
highlightOff ::  [Char]
highlightOff = "\ESC[0m"

renderAttrG ::  TerminalType -> Highlight -> [String]
renderAttrG XTerm256Compatible (Foreground (Rgb r g b)) =
    [ "38", "5", show ( rgb24bit_to_xterm256 r g b ) ]
renderAttrG XTerm256Compatible (Background (Rgb r g b)) =
    [ "48", "5", show ( rgb24bit_to_xterm256 r g b ) ]
renderAttrG _ a                                         =
    [ show (fromEnum (hlProjectToBasicColour8 a)) ]

-- | An infinite supply of colours.
colourCycle :: [Colour]
colourCycle = cycle [Red,Blue,Magenta,Green,Cyan]


-- | Scrolling
enableScrollRegion :: Int -> Int -> String
enableScrollRegion start end = "\ESC["++show start++';':show end++"r"

scrollDown ::  String
scrollDown  = "\ESCD"
scrollUp ::  String
scrollUp    = "\ESCM"

-- Line-wrapping mode
lineWrap ::  Bool -> [Char]
lineWrap True  = "\ESC[7h"
lineWrap False = "\ESC[7l"

