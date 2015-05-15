
-- | A DSL for representing qualifies as a matched token

module Text.Tokenify.Regex (Regex(..)) where


-- | An Abstract data type for representing a regex


data Regex s
  = Char Char
  | String s
  | Alt (Regex s) (Regex s)
  | Append (Regex s) (Regex s)
  | Range Char Char
  | Option (Regex s)
  | Repeat (Regex s)
  | Repeat1 (Regex s)
  | NoPass


instance Show s => Show (Regex s) where
  show regex = "Regex \\" ++ showRegex regex ++ "\\" where

    showRegex :: Show s => Regex s -> String
    showRegex r = case r of
      Char c   -> [c]
      String s -> show s
      --
      -- TODO
      --  - make '(a|(b|c))' appear as '(a|b|c)'
      --  - make '(a|nopass)' appear as 'a'
      --
      Alt l NoPass -> showRegex l
      Alt l r -> "("++showRegex l++"|"++showRegex r++")"

      Append l NoPass -> showRegex l
      Append l r -> showRegex l ++ showRegex r

      Range s e -> "["++[s]++"-"++[e]++"]"
      Option r -> "("++showRegex r++")?"
      Repeat r -> "("++showRegex r++")*"
      Repeat1 r -> "("++showRegex r++")+"


unquote string = case string of
  '\"':xs -> impl xs ""
  other   -> impl string ""
  where impl ('\"':[]) acc = acc
        impl (x:xs) acc = impl xs (acc++[x])

