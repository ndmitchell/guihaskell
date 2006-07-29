
module Text.EscapeCodes where

import Data.Char
import Data.Maybe


data Attribute = Normal | Bold | Underline | Blink | Reverse | Invisible
                 deriving (Show, Eq)

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
             deriving (Show, Eq, Enum, Bounded)


data EscapeCode = FormatAttribute Attribute
                | FormatForeground Color
                | FormatBackground Color
                | FormatUnknown Int
                deriving (Show, Eq)


escapeChar :: Char
escapeChar = chr 27


parseEscapeCodes :: String -> [Either Char EscapeCode]
parseEscapeCodes x = f 0 x
    where
        f 0 (c1:c2:cs) | c1 == escapeChar && c2 == '[' = f 1 cs
        
        f 1 (c:cs) | isDigit c = Right (g $ read a) : f 2 b
            where (a,b) = span isDigit (c:cs)

        f 2 (';':cs) = f 1 cs
        f 2 ('m':cs) = f 0 cs

        -- defensive coding, never pattern match
        -- always continue producing
        f _ (c:cs) = Left c : f 0 cs
        f _ [] = []


        g :: Int -> EscapeCode
        g x | x >= 30 && x <= 37 = FormatForeground $ toEnum (x - 30)
            | x >= 40 && x <= 47 = FormatBackground $ toEnum (x - 40)
            | otherwise = case lookup x attribs of
                              Nothing -> FormatUnknown x
                              Just y -> FormatAttribute y

        attribs = [(0, Normal),
                   (1, Bold),
                   (4, Underline),
                   (5, Blink),
                   (7, Reverse),
                   (8, Invisible)]


getColor :: Color -> (Int,Int,Int)
getColor x = case lookup x colors of
                 Nothing -> (0,0,0)
                 Just x -> x
    where
        colors = [(Black, (0,0,0)),
                  (Red, (h,0,0)),
                  (Green, (0,h,0)),
                  (White, (f,f,f))]
        
        h = 0x80
        f = 0xff
