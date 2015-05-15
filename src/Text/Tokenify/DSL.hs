module Text.Tokenify.DSL where

import Prelude hiding (concat, any)

import qualified Text.Tokenify.Response as Response
import qualified Text.Tokenify.Regex as Regex
import Text.Tokenify.Regex (Regex)
import Text.Tokenify.Types


{-| Response Constructors -}


fails :: Regex s -> Token s a
fails r = (r, Response.Error)

ignore :: Regex s -> Token s a
ignore r = (r, Response.Ignore)

insert :: Regex s -> (Pos -> a) -> Token s a
insert r f = (r, Response.Display f)

evaluate :: Regex s -> (s -> Pos -> a) -> Token s a
evaluate r f = (r, Response.Process f)


{-| Regex Constructors -}


string :: s -> Regex s
string = Regex.String

range :: Char -> Char -> Regex s
range = Regex.Range

char :: Char -> Regex s
char = Regex.Char

alt :: Regex s -> Regex s -> Regex s
alt = Regex.Alt

any :: [Regex s] -> Regex s
any []     = Regex.NoPass
any (x:[]) = x
any (x:xs) = Regex.Alt x (any xs)

append :: Regex s -> Regex s -> Regex s
append = Regex.Append

concat :: [Regex s] -> Regex s
concat []     = Regex.NoPass
concat (x:[]) = x
concat (x:xs) = Regex.Append x (concat xs)

option :: Regex s -> Regex s
option = Regex.Option

repeat :: Regex s -> Regex s
repeat = Regex.Repeat

repeat1 :: Regex s -> Regex s
repeat1 = Regex.Repeat1




