# OpenBook
OpenBook is a command line application to assist with open book exams.

## Install
You will need to have cabal and GHC installed. You simply navigate to the clones directory and simply type
```
cabal v2-install
```
make sure the cabal bin dir is in your path! I have no idea if it works on Windows.

## Usage
When it is completed, you will simply run `OpenBook [filename]` where the file is
a store of all the data you might want to look up in the exam.

When the app is running, 'ESC' closes the app, type to search and arrow keys to select a hit.
searching can be more focused with the following rules:
- Typing anything will simply search through names of the entries.
- `def text` will search for definitions
- `eqn text` will search for equations relating to text
- `opt text` will search for options, for example, in a computer security exam `opt attacks` would give me the definitions of differnt type of cyber attack
- `txt text` will search the body of data entries
- `reg text` will do the same but text is a regular expression
All searches are case insensitive

## Inputing data
Data is input in `.ob` files. Here is an example file:
```md
#title Monad
A Monad is a type constructor, M, together with an instance of return and bind

#def Monad Laws
The monad laws are the following:
Associativity:
  (xm $> f) $> g = xm $> (\x -> f x $> g)
Identity:
  (result x) $> f = f x
  xm $> result = xm

#eqn monad-eqn
result :: a -> M a
($>) :: M a -> (a -> M b) -> M b

#title Continuation Monad
The continuation monad encapsulates continuations

#def ContMonadDef
type M a = (a -> Ans) -> Ans

#is Monad
-

#eqn contMonadEq
result x k = k x 

(x $> f) k = x (\x -> f x k)
```

## TODO List
As you can see, not much has been implemented yet
- [X] Build UI
- [X] Design file format and parser for text
- [ ] Implement search utility
