{-# LANGUAGE ViewPatterns #-}

module Text.Tokenify (

  tokenize,

  matchHead,

) where

import Prelude hiding (head)

import qualified Text.Tokenify.Response as Response
import qualified Text.Tokenify.CharSeq as CSeq
import qualified Text.Tokenify.Regex as Regex

import Text.Tokenify.Response (Response)
import Text.Tokenify.CharSeq (CharSeq)
import Text.Tokenify.Regex (Regex)
import Text.Tokenify.Types

import qualified Data.Monoid as Monoid
import qualified Data.Sequence as Seq
import Data.Sequence ((|>), Seq)
import Data.Monoid ((<>))

import Control.Applicative ((<|>))


{-- main functionality --}


tokenize :: (CharSeq s) => Tokenizer s a -> s -> Either String (Seq a)
tokenize tokenizers input = impl tokenizers Seq.empty 0 input where

  inputInfo :: [Int]
  inputInfo = CSeq.lineInfo input

  getPos :: Int -> (Int, Int)
  getPos offset = getPosImpl 0 1 inputInfo where
    getPosImpl position row (lineLength:lines)
      -- in this line
      | offset >= position && offset <= position+lineLength
        = (row, offset - position)
      -- beyond this line
      | offset > position+lineLength
        = getPosImpl (position + lineLength) (row + 1) lines


  -- exit edge condition
  impl _ acc _ input | CSeq.null input = Right acc

  -- fail edge condition
  impl [] acc position _
    = Left ("failed to match at" ++ show (getPos position))

  -- normal loop
  impl ((rx, rs):ts) acc position input
    = case matchHead rx input of
      Nothing -> impl ts acc position input
      Just (matched, rest, moved) ->

        let position' = position+moved
            coordants = getPos position in case rs of

          Response.Error     -> Left ("matched error at " ++ show position)
          Response.Ignore    ->
            impl tokenizers acc position' rest
          Response.Display p ->
            impl tokenizers (acc |> p coordants) position' rest
          Response.Process p ->
            impl tokenizers (acc |> p matched coordants) position' rest



matchHead :: (CharSeq s) => Regex s -> s -> Maybe (s, s, Int)
matchHead regex input = case regex of
  -- match nothing
  Regex.NoPass -> Nothing

  -- match the char 'c'
  Regex.Char c -> CSeq.head input >>=
    \head -> if head == c
      then return (CSeq.singleton head, CSeq.tail input, 1)
      else Nothing

  -- match the string 's'
  Regex.String s -> prefixTail s input >>=
    \(diff, dSize) -> return (s, diff, dSize)

  -- either 'l' or 'r'
  Regex.Alt l r ->
    matchHead l input <|> matchHead r input

  -- matches char between 's' & 'e'
  Regex.Range s e -> do
    head <- CSeq.head input
    if head >= s && e >= head
      then return (CSeq.singleton head, CSeq.tail input, 1)
      else Nothing

  -- chain 'l' & 'r'
  Regex.Append l r -> do
    (a, cont, ai) <- matchHead l input
    (b, cont, bi) <- matchHead r cont
    return (a <> b, cont, ai + bi)

  -- optionally matches
  Regex.Option o -> case matchHead o input of
    Nothing -> return (Monoid.mempty, input, 0)
    anythingElse -> anythingElse

  -- repeat 0 or more times
  Regex.Repeat r -> impl Monoid.mempty input 0 where
    impl acc cont@(matchHead r -> Nothing) i = Just (acc, cont, i)
    impl a (matchHead r -> Just (b, cont, ib)) ia =
      impl (a <> b) cont (ia + ib)

  -- repeat 1 or more times
  Regex.Repeat1 r -> do
    (a, cont, ai) <- matchHead r input
    (b, cont, bi) <- matchHead (Regex.Repeat r) input
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


