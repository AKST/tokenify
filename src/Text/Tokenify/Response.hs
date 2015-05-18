-- | An abtract datatype for dealing with regex results
module Text.Tokenify.Response (Response(..)) where



-- | A response, as the name suggests is a response which is triggered when
-- a regular expression is matched.
data Response s a
  = Ignore
  | Display ((Int, Int) -> a)
  | Process (s -> (Int, Int) -> a)
  | Error


