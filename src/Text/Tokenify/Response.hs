module Text.Tokenify.Response (Response(..)) where

type Pos = (Int, Int)


data Response s a
  = Ignore
  | Display (Pos -> a)
  | Process (s -> Pos -> a)
  | Error


