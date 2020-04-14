# OpenBook
OpenBook is a command line application to assist with open book exams.

## Install
You will need to have cabal and GHC installed. You simply navigate to the clones directory and simply type
```
cabal v2-build
./copyExec.sh [Install dir]
```
The script will not work on windows

## Usage
When it is completed, you will simply run `OpenBook [filename]` where the file is
a store of all the data you might want to look up in the exam.

- When the app is running, 'ESC' closes the app, type to search and arrow keys to select a hit.
- Clicking on "See also" or "related" section will jump there
- On MacOS Only, clicking on a "File : " section will open the file in QuickLook

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

#file LaTeXed Formula
/Users/James/formula.png
```

## Getting file support working on linux
In `src/Main.hs` (as of writing line 72) there is a line that looks like this:
```haskell
handleEvent entries s (MouseDown (Extern path) button mods coords) = (liftIO $ spawnCommand ("qlmanage -p " ++ path ++ " >& /dev/null")) >> continue s
```
swap out `qlmanage -p` with whatever file opening command you want!

## TODO List
- [X] Build UI
- [X] Design file format and parser for text
- [X] Implement search utility
- [ ] Implement more focuses search
