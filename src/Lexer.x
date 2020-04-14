{
module Lexer (scan, Token (..)) where
}

%wrapper "basic"

$digit = 0-9       -- digits
$alpha = [a-zA-Z\ \(\)0-9\-\_\']  -- alphabetic characters
@pathr = (\/.+)+

tokens :-
  $white+               ;
  \#title\ $alpha+  { \s -> Title $ tail $ dropWhile (/= ' ') s}
  \#def\ $alpha+    { \s -> Def   $ tail $ dropWhile (/= ' ') s}
  \#eqn\ $alpha+    { \s -> Eqn   $ tail $ dropWhile (/= ' ') s}
  \#is\ $alpha+     { \s -> Is    $ tail $ dropWhile (/= ' ') s}
  \#file\ $alpha+   { \s -> File  $ tail $ dropWhile (/= ' ') s}
  [^\#]+            { \s -> Body s }

{

data Token = Title String
           | Def   String
           | Eqn   String
           | Is    String
           | File  String
           | Body  String
           deriving (Eq, Show)

scan = alexScanTokens

}
