-- | An abtract datatype for dealing with regex results
module Text.Tokenify.Response (Response(..)) where



-- | The purpose of a response is to provide behaviour to
-- interprete the regular expression
data Response s a
  = Ignore
  | Display ((Int, Int) -> a)
  | Process (s -> (Int, Int) -> a)
  | Error


