-- | The DSL for creating a grammar/tokenizer definition for 'Text.Tokenify.tokenizer'

module Text.Tokenify.DSL where

import Prelude hiding (concat, any)

import qualified Text.Tokenify.Response as Response
import qualified Text.Tokenify.Regex as Regex
import Text.Tokenify.Regex (Regex)
import Text.Tokenify.Types


-- * Response Constructors

-- | Creates a response which will fail on a regex
fails :: Regex s -> Token s a
fails r = (r, Response.Error)

-- | Creates a response which will ignore a regex
ignore :: Regex s -> Token s a
ignore r = (r, Response.Ignore)

-- | Creates a response which consumes the text position
insert :: Regex s -> (Pos -> a) -> Token s a
insert r f = (r, Response.Display f)

-- | Creates a response which consumes the captures 'CharSeq' and the text position
evaluate :: Regex s -> (s -> Pos -> a) -> Token s a
evaluate r f = (r, Response.Process f)


-- * Regex Constructors


-- | Creates a regex that matches a string
string :: s -> Regex s
string = Regex.String

-- | Creates a regex that matches a char
char :: Char -> Regex s
char = Regex.Char

-- | Creates a create that will match a range of characters
range :: Char -> Char -> Regex s
range = Regex.Range

-- | Creates a regex that will attmpt to make the regex on the left, if
-- that fails it will attmpt to match the regex on the right
alt :: Regex s -> Regex s -> Regex s
alt = Regex.Alt

-- | Creates a regex that will attmpt to match a Sequence of regex\'s
-- in a sequencial order
any :: [Regex s] -> Regex s
any []     = Regex.NoPass
any (x:[]) = x
any (x:xs) = Regex.Alt x (any xs)

-- | Create a regex that appends the result of two regex\'s
append :: Regex s -> Regex s -> Regex s
append = Regex.Append

-- | Create a regex that appends the result of a sequence of regex\'s
concat :: [Regex s] -> Regex s
concat []     = Regex.NoPass
concat (x:[]) = x
concat (x:xs) = Regex.Append x (concat xs)

-- | Create a regex that may or may not match a regex
option :: Regex s -> Regex s
option = Regex.Option

-- | Create a regex that matches zero or more of a regex
repeat :: Regex s -> Regex s
repeat = Regex.Repeat

-- | Create a regex that matches one or more of a regex
repeat1 :: Regex s -> Regex s
repeat1 = Regex.Repeat1




