# Tokenify, _a Lexer_

This is a small lexer library, here's a small snippet of usage

```haskell
module MyModule where

import qualified Text.Tokenify.DSl as T
import qualified Text.Tokenify as T
import Text.Tokenify.Types (Pos)


data IR = Num Int Pos | Op Op Pos deriving (Show)

data Op = Pl | Mi | Mu | Di deriving (Show)


grammar = [
    T.evalute (T.range '0' '9') (\n pos -> Num (read n) pos),
    T.insert (T.char '+') (\pos -> Op Pl pos),
    T.insert (T.char '-') (\pos -> Op Mi pos),
    T.insert (T.char '*') (\pos -> Op Mu pos),
    T.insert (T.char '\') (\pos -> Op Di pos),
    T.ignore (T.char '\n')
  ]

```

In ghci, you should get something along these lines

```
$ cabal repl
λ: import MyModule
λ: import Text.Tokenify (tokenize)
λ: tokenize grammar "1+3-4\n+3"

Right (fromList [
  Num 1 (1, 0),
  Op Pl (1, 1),
  Num 3 (1, 2),
  Op Mi (1, 3),
  Num 4 (1, 4),
  Op Pl (2, 0),
  Num 3 (2, 1)
])
```


