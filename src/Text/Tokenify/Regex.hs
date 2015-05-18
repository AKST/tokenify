{-# LANGUAGE ViewPatterns #-}

-- | A DSL for representing qualifies as a matched token

module Text.Tokenify.Regex (

  -- * Types
  Regex(),

  -- * Constructors
  string, char, range, alt, any,
  repeat, repeat1, append, option,
  concat,

  -- * Module Utitlity
  matchHead

) where

import Prelude hiding (concat, any, repeat)

import qualified Text.Tokenify.CharSeq as CSeq
import Text.Tokenify.CharSeq (CharSeq)

import qualified Data.Monoid as Monoid
import Data.Monoid ((<>))

import Control.Applicative ((<|>))

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


-- * Regex Constructors


-- | Creates a regex that matches a string
string :: s -> Regex s
string = String

-- | Creates a regex that matches a char
char :: Char -> Regex s
char = Char

-- | Creates a create that will match a range of characters
range :: Char -> Char -> Regex s
range = Range

-- | Creates a regex that will attmpt to make the regex on the left, if
-- that fails it will attmpt to match the regex on the right
alt :: Regex s -> Regex s -> Regex s
alt = Alt

-- | Creates a regex that will attmpt to match a Sequence of regex\'s
-- in a sequencial order
any :: [Regex s] -> Regex s
any []     = NoPass
any (x:[]) = x
any (x:xs) = Alt x (any xs)

-- | Create a regex that appends the result of two regex\'s
append :: Regex s -> Regex s -> Regex s
append = Append

-- | Create a regex that appends the result of a sequence of regex\'s
concat :: [Regex s] -> Regex s
concat []     = NoPass
concat (x:[]) = x
concat (x:xs) = Append x (concat xs)

-- | Create a regex that may or may not match a regex
option :: Regex s -> Regex s
option = Option

-- | Create a regex that matches zero or more of a regex
repeat :: Regex s -> Regex s
repeat = Repeat

-- | Create a regex that matches one or more of a regex
repeat1 :: Regex s -> Regex s
repeat1 = Repeat1


-- * Regex utitlity


-- | Attmpts to match the front of a 'CharSeq' with a 'Regex',
-- if succeful, it returns a tuple containing
--
--  * The matched 'CharSeq'
--  * The remaining 'CharSeq'
--  * The amount of characters consumed
matchHead :: (CharSeq s) => Regex s -> s -> Maybe (s, s, Int)
matchHead regex input = case regex of
  -- match nothing
  NoPass -> Nothing

  -- match the char 'c'
  Char c -> CSeq.head input >>=
    \head -> if head == c
      then return (CSeq.singleton head, CSeq.tail input, 1)
      else Nothing

  -- match the string 's'
  String s -> prefixTail s input >>=
    \(diff, dSize) -> return (s, diff, dSize)

  -- either 'l' or 'r'
  Alt l r ->
    matchHead l input <|> matchHead r input

  -- matches char between 's' & 'e'
  Range s e -> do
    head <- CSeq.head input
    if head >= s && e >= head
      then return (CSeq.singleton head, CSeq.tail input, 1)
      else Nothing

  -- chain 'l' & 'r'
  Append l r -> do
    (a, cont, ai) <- matchHead l input
    (b, cont, bi) <- matchHead r cont
    return (a <> b, cont, ai + bi)

  -- optionally matches
  Option o -> case matchHead o input of
    Nothing -> return (Monoid.mempty, input, 0)
    anythingElse -> anythingElse

  -- repeat 0 or more times
  Repeat r -> impl Monoid.mempty input 0 where
    impl acc cont@(matchHead r -> Nothing) i = Just (acc, cont, i)
    impl a (matchHead r -> Just (b, cont, ib)) ia =
      impl (a <> b) cont (ia + ib)

  -- repeat 1 or more times
  Repeat1 r -> do
    (a, cont, ai) <- matchHead r input
    (b, cont, bi) <- matchHead (Repeat r) input
    return (a <> b, cont, ai + bi)


--
-- dumb helpers
--


prefixTail :: (CharSeq s) => s -> s -> Maybe (s, Int)
prefixTail prefix input = trySplit prefix input 0 where
  trySplit pre dec index
    | CSeq.null pre = Just (dec, index)

    -- if (dec.length != 0 && dec.head == pre.head)
    --   trySplit(pre.tail, dec.tail, ++index);
    | not (CSeq.null dec) && (CSeq.head dec == CSeq.head pre)
       = trySplit (CSeq.tail pre) (CSeq.tail dec) (index + 1)

    | otherwise = Nothing


