
-- | Common types used by Text.Tokenify
module Text.Tokenify.Types where

import Text.Tokenify.Response (Response)
import Text.Tokenify.Regex (Regex)

-- | A series of Tokens which will match in sequencial order
type Tokenizer s a = [Token s a]

-- | Defines what is matches, and how to respond to said match
type Token s a = (Regex s, Response s a)

-- | The type for a token position in a file
type Pos = (Int, Int)

