{-# LANGUAGE FlexibleInstances #-}

module Text.Tokenify.CharSeq where

import qualified Prelude as Prelude
import Prelude hiding (head, tail, null)

import qualified GHC.Exts as Exts
import qualified Data.Text as Text
import Data.Monoid (Monoid)
import Data.Text (Text)


--
-- this typeclass is designed just for
-- the tokenizer so, it requires Eq for
-- convience
--
class (Monoid a, Eq a) => CharSeq a where
  head :: a -> Maybe Char
  tail :: a -> a
  cons :: Char -> a -> a
  snoc :: a -> Char -> a
  null :: a -> Bool
  singleton :: Char -> a
  lineInfo :: a -> [Int]


instance CharSeq Text where
  head input
    | not (Text.null input) = Just (Text.head input)
    | otherwise             = Nothing
  tail = Text.tail
  cons = Text.cons
  snoc = Text.snoc
  null = Text.null
  singleton = Text.singleton

  lineInfo = map Text.length . Text.lines

instance CharSeq [Char] where
  head input 
    | not (Prelude.null input) = Just (Prelude.head input)
    | otherwise        = Nothing
  tail = Prelude.tail
  cons = (:)
  snoc s c = s ++ [c]
  null = Prelude.null
  singleton c = [c]

  lineInfo = map length . lines

