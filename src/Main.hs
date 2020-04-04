{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import qualified Graphics.Vty as V

import Brick.Util (on, fg)
import Brick.Markup (markup, (@?))
import Brick.AttrMap (attrMap, AttrMap)
import Data.Text.Markup ((@@))
import Brick.Widgets.Border
import qualified Brick.Widgets.Center as C
import qualified Data.Text as T 

import Lexer
import Parser

buildType = parse . scan

type Info = (String, String)

data State = Init | Results String [Info] Int

keywords = ["def", "eqn", "opt", "txt", "reg"]

getIName, getIData :: Info -> String
getIName = fst
getIData = snd

drawUI :: State -> [Widget ()]
drawUI s = [drawTopBar s <=> (vBox [drawSelect s] <+> vBorder <+> vBox [drawResult s])]

drawTopBar :: State -> Widget ()
drawTopBar (Init) = str "OpenBook" <=> hBorder <=> str "█" <=> hBorder
drawTopBar (Results s _ _) = str "OpenBook" <=> hBorder <=> (renderSearch s) <=> hBorder

renderSearch :: String -> Widget ()
renderSearch [] = str "█"
renderSearch ys@(x:y:z:xs) = (if (x:y:z:[] `elem` keywords) then markup (T.pack (x:y:z:[]) @? "keyword1") <+> str xs else str ys) <+> str "█"
renderSearch n = str n <+> str "█"

mapSelect :: Int -> [n] -> (n -> String) -> [Widget n2]
mapSelect n []     f = []
mapSelect 0 (x:xs) f = (markup $ (T.pack (f x) @? "Selected")) : map (str . f) xs
mapSelect n (x:xs) f = (str . f) x : mapSelect (n-1) xs f

drawSelect :: State -> Widget ()
drawSelect (Init) = str ""
drawSelect (Results _ res selected) = vBox $ mapSelect selected res getIName

drawResult :: State -> Widget ()
drawResult (Init) = str ""
drawResult (Results _ res selected) = (str . getIData) $ res !! selected

chooseCursor :: State -> [CursorLocation ()] -> Maybe (CursorLocation ())
chooseCursor = neverShowCursor

handleEvent  :: State -> BrickEvent () e -> EventM () (Next State)
handleEvent s@(Init) (VtyEvent e) = case e of
                              V.EvKey V.KEsc      [] -> halt s
                              V.EvKey (V.KChar c) [] -> continue (Results [c] [("One", "One"), ("Two", "Hello World"), ("Three", "BLA BLA BLA BLA BLA BLA BLA BLA \n BLA")] 0)
                              _                      -> continue s
handleEvent s@(Results search r sel) (VtyEvent e) = case e of
                              V.EvKey V.KEsc      [] -> halt s
                              V.EvKey V.KDown     [] -> continue (Results search r (sel + 1))
                              V.EvKey V.KUp       [] -> continue (Results search r (sel - 1))
                              V.EvKey V.KBS       [] -> if (length search > 1) then  continue (Results (init search) r 0) else continue Init
                              V.EvKey (V.KChar c) [] -> continue (Results (search ++ [c]) r 0)
                              _                      -> continue s

startEvent :: State -> EventM () State
startEvent = return

attMap :: s -> AttrMap
attMap = const theMap

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ ("keyword1",      fg V.brightBlue)
    , ("Selected",      fg V.red)
    ]

main :: IO ()
main = do
  let app = App { appDraw         = drawUI
                , appChooseCursor = chooseCursor
                , appHandleEvent  = handleEvent
                , appStartEvent   = startEvent
                , appAttrMap      = attMap
                }
  let initialState = Init
  finalState <- defaultMain app initialState
  return ()
