-- | 'Text.Tokenify' is a module used for generating a
-- tokenizer from a regex based grammar

module Text.Tokenify (

  tokenize,

  Regex.matchHead,

  module Text.Tokenify.DSL,
  module Text.Tokenify.Types,
  module Text.Tokenify.CharSeq

) where

import Prelude hiding (head)

import qualified Text.Tokenify.Response as Response
import qualified Text.Tokenify.CharSeq as CSeq
import qualified Text.Tokenify.Regex as Regex
import qualified Text.Tokenify.Types as Types

import Text.Tokenify.DSL ()
import Text.Tokenify.Response (Response)
import Text.Tokenify.CharSeq (CharSeq)
import Text.Tokenify.Regex (Regex)
import Text.Tokenify.Types

import qualified Data.Sequence as Seq
import Data.Sequence ((|>), Seq)


{-- main functionality --}


-- | 'tokenize' will transform a 'CharSeq' into a sequence of tokens
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
    = case Regex.matchHead rx input of
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


