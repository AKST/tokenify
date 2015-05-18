-- | The DSL for creating a grammar/tokenizer definition for 'Text.Tokenify.tokenizer'

-- NOTE: at the moment this module exists to provide
-- an abstraction over both Regex & Response, so that
-- neither of these modules have know about each other
-- otherwise we'd get a circular dependency.

module Text.Tokenify.DSL (

  -- * Token Constructors
  fails, ignore, insert, evaluate,

  -- * Regex Constructors
  Regex.string, Regex.char, Regex.range, Regex.alt, Regex.any,
  Regex.repeat, Regex.repeat1, Regex.append, Regex.option,
  Regex.concat

) where

import Prelude hiding (concat, any)

import qualified Text.Tokenify.Response as Response
import qualified Text.Tokenify.Regex as Regex
import Text.Tokenify.Regex (Regex)
import Text.Tokenify.Types


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


