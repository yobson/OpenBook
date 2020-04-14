{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import qualified Graphics.Vty as V

import Brick.Util (on, fg)
import Brick.Markup (markup, (@?))
import Brick.AttrMap (attrMap, AttrMap)
import Data.Text.Markup ((@@))
import Brick.Widgets.Border
import Brick.Widgets.Core
import qualified Brick.Widgets.Center as C
import qualified Data.Text as T 
import System.Environment
import System.Process
import Control.Monad
import Control.Monad.IO.Class

import Lexer
import Parser
import Search


data State = Init | Results String [Info] Int

keywords = ["def", "eqn", "opt", "txt", "reg"]


drawUI :: State -> [Widget ButtonType]
drawUI s = [drawTopBar s <=> (vBox [drawSelect s] <+> vBorder <+> vBox [drawResult s])]

drawTopBar :: State -> Widget ButtonType
drawTopBar (Init) = str "OpenBook" <=> hBorder <=> str "█" <=> hBorder
drawTopBar (Results s _ _) = str "OpenBook" <=> hBorder <=> (renderSearch s) <=> hBorder

renderSearch :: String -> Widget ButtonType
renderSearch [] = str "█"
renderSearch ys@(x:y:z:xs) = (if (x:y:z:[] `elem` keywords) then markup (T.pack (x:y:z:[]) @? "keyword1") <+> str xs else str ys) <+> str "█"
renderSearch n = str n <+> str "█"

mapSelect :: Int -> [n] -> (n -> String) -> [Widget n2]
mapSelect n []     f = []
mapSelect 0 (x:xs) f = (markup $ (T.pack (f x) @? "Selected")) : map (str . f) xs
mapSelect n (x:xs) f = (str . f) x : mapSelect (n-1) xs f

drawSelect :: State -> Widget ButtonType
drawSelect (Init) = str ""
drawSelect (Results _ res selected) = vBox $ mapSelect selected res getIName

drawResult :: State -> Widget ButtonType
drawResult (Init) = str ""
drawResult (Results _ res selected) | selected >= 0 && selected < length res = getIData $ res !! selected
                                    | otherwise                             = str ""

chooseCursor :: State -> [CursorLocation ButtonType] -> Maybe (CursorLocation ButtonType)
chooseCursor = neverShowCursor

handleEvent  :: [Info] -> State -> BrickEvent ButtonType e -> EventM ButtonType (Next State)
handleEvent entries s@(Init) (VtyEvent e) = case e of
                              V.EvKey V.KEsc      [] -> halt s
                              V.EvKey (V.KChar c) [] -> continue (Results [c] (getResults entries [c]) 0)
                              _                      -> continue s
handleEvent entries s@(Results search r sel) (VtyEvent e) = case e of
                              V.EvKey V.KEsc      [] -> halt s
                              V.EvKey V.KDown     [] -> let nSel = sel + 1 in continue (Results search r (if nSel >= (length r) then 0 else nSel))
                              V.EvKey V.KUp       [] -> let nSel = sel - 1 in continue (Results search r (if nSel < 0 then length r - 1 else nSel))
                              V.EvKey V.KBS       [] -> if (length search > 1) then  continue (Results (init search) (getResults entries (init search)) 0) else continue Init
                              V.EvKey (V.KChar c) [] -> let nSearch = search ++ [c] in continue (Results nSearch (getResults entries nSearch) 0)
                              _                      -> continue s
handleEvent entries s (MouseDown (Extern path) button mods coords) = (liftIO $ spawnCommand ("qlmanage -p " ++ path ++ " >& /dev/null")) >> continue s
handleEvent entries s (MouseDown (Intern path) button mods coords) = continue (Results path (getResults entries path) 0)
handleEvent entries s _ = continue s

startEvent :: State -> EventM ButtonType State
startEvent s = do
  vty <- Brick.getVtyHandle
  let output = V.outputIface vty
  when (V.supportsMode output V.Mouse) $
    liftIO $ V.setMode output V.Mouse True
  return s

attMap :: s -> AttrMap
attMap = const theMap

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ ("keyword1",      fg V.brightBlue)
    , ("Selected",      fg V.red)
    ]

buildType = parse . scan

main :: IO ()
main = do
  args <- getArgs
  files <- mapM readFile args
  let entries = concatMap buildType files
  let app = App { appDraw         = drawUI
                , appChooseCursor = chooseCursor
                , appHandleEvent  = handleEvent entries
                , appStartEvent   = startEvent
                , appAttrMap      = attMap
                }
  let initialState = Init
  finalState <- defaultMain app initialState
  return ()
