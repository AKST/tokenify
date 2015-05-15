{-# LANGUAGE FlexibleInstances #-}

-- | An abstaction over string like data types to prevent
-- restricting 'Data.Tokenify.tokenizer' to a particular
-- data type, as there are many string types in haskell.

module Text.Tokenify.CharSeq where

import qualified Prelude as Prelude
import Prelude hiding (head, tail, null)

import qualified GHC.Exts as Exts
import qualified Data.Text as Text
import Data.Monoid (Monoid)
import Data.Text (Text)


-- | This typeclass is designed to make the tokenizer more polymorphic,
-- which is why it's a super set of both 'Monoid' and 'Eq'.
class (Monoid a, Eq a) => CharSeq a where
  -- | The first char in a 'CharSeq'
  head :: a -> Maybe Char
  -- | Every char but the first in a 'CharSeq'
  tail :: a -> a
  -- | Pushes the to the head of a 'CharSeq'
  cons :: Char -> a -> a
  -- | Pushes the to the end of a 'CharSeq'
  snoc :: a -> Char -> a
  -- | A 'CharSeq' containing a single character
  null :: a -> Bool
  -- | Makes a 'CharSeq' containing a single character
  singleton :: Char -> a
  -- | Provides line information of a 'CharSeq'
  lineInfo :: a -> [Int]


-- | Enables 'Data.Text' to be used for 'Text.Tokenify.tokenizer'
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

-- | the main purpose of this implemenation is make
-- testing in the repl non-painful
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

