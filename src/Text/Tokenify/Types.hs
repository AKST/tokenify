module Text.Tokenify.Types where

import Text.Tokenify.Response (Response)
import Text.Tokenify.Regex (Regex)

type Tokenizer s a = [Token s a]

type Token s a = (Regex s, Response s a)

type Pos = (Int, Int)

