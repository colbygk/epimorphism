module KeyHandlers where

import Prelude
import Config (UIST, UIConf)
import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST, STRef, readSTRef, modifySTRef)
import DOM (DOM)
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (insert, member, lookup)
import Graphics.Canvas (CANVAS)
import Util (inj)

-- converts key codes into command sequences
type KeyHandler = forall eff h. STRef h UIConf -> STRef h UIST -> String -> Eff (canvas :: CANVAS, dom :: DOM, st :: ST h | eff) String

keyHandler :: (Partial) => KeyHandler
keyHandler ucRef usRef char = do
  uiConf <- readSTRef ucRef

  --let x = lg char
  case uiConf.keySet of
    "dev"  -> devKeyHandler ucRef usRef char
    "prod" -> prodKeyHandler ucRef usRef char
    _      -> commonKeyHandler ucRef usRef char


devKeyHandler :: (Partial) => KeyHandler
devKeyHandler ucRef usRef char = do
  uiConf <- readSTRef ucRef
  uiST   <- readSTRef usRef

  case char of
    "1" -> do
      incMod uiConf uiST "main.application.t" "t_inner" "all" 1
    "q" -> do
      incMod uiConf uiST "main.application.t" "t_inner" "all" (-1)
    "!" -> do
      incMod uiConf uiST "main.application" "t" "all''" 1
    "Q" -> do
      incMod uiConf uiST "main.application" "t" "all''" (-1)
    "1" -> do
      incMod uiConf uiST "main.application.t" "t_inner" "all" 1
    "q" -> do
      incMod uiConf uiST "main.application.t" "t_inner" "all" (-1)
    "2" -> do
      incMod uiConf uiST "main.application" "seed" "t_test" 1
    "w" -> do
      incMod uiConf uiST "main.application" "seed" "t_test" (-1)
    "3" -> do
      incMod uiConf uiST "main.application" "color" "lib" 1
    "e" -> do
      incMod uiConf uiST "main.application" "color" "lib" (-1)
    "4" -> do
      incMod uiConf uiST "disp" "post" "lib" 1
    "r" -> do
      incMod uiConf uiST "disp" "post" "lib" (-1)
    "a" -> do
      pure "scr main.application.t incZn idx:0 ofs:1"
    "z" -> do
      pure "scr main.application.t incZn idx:0 ofs:-1"
    "A" -> do
      pure "scr main.application.t incZn idx:0 ofs:i"
    "Z" -> do
      pure "scr main.application.t incZn idx:0 ofs:-i"
    "s" -> do
      pure "scr main.application.t incZn idx:1 ofs:1"
    "x" -> do
      pure "scr main.application.t incZn idx:1 ofs:-1"
    "S" -> do
      pure "scr main.application.t incZn idx:1 ofs:i"
    "X" -> do
      pure "scr main.application.t incZn idx:1 ofs:-i"
    "d" -> do
      pure "scr main.application.t incZn idx:2 ofs:1"
    "c" -> do
      pure "scr main.application.t incZn idx:2 ofs:-1"
    "D" -> do
      pure "scr main.application.t incZn idx:2 ofs:i"
    "C" -> do
      pure "scr main.application.t incZn idx:2 ofs:-i"
    "f" -> do
      pure "scr main.application.t incZn idx:3 ofs:1"
    "v" -> do
      pure "scr main.application.t incZn idx:3 ofs:-1"
    "F" -> do
      pure "scr main.application.t incZn idx:3 ofs:i"
    "V" -> do
      pure "scr main.application.t incZn idx:3 ofs:-i"
    _   -> commonKeyHandler ucRef usRef char
  where
    incMod uiConf uiST adr childN lib ofs = do
      let idn' = adr <> childN
      let idx = if (member idn' uiST.incIdx) then ((fromJust $ lookup idn' uiST.incIdx) + ofs) else 0
      let dt = insert idn' idx uiST.incIdx
      modifySTRef usRef (\s -> s {incIdx = dt})
      let spd = show uiConf.keyboardSwitchSpd
      pure $ inj "scr %0 switch childN:%1 op:load by:query typ:mod query:%2 accs:%3 spd:%4" [adr, childN, lib, (show idx), spd]


prodKeyHandler :: KeyHandler
prodKeyHandler ucRef usRef char = do
  uiConf <- readSTRef ucRef
  uiST   <- readSTRef usRef

  case char of
    _ -> commonKeyHandler ucRef usRef char

commonKeyHandler :: KeyHandler
commonKeyHandler ucRef usRef char = do
  uiConf <- readSTRef ucRef
  uiST   <- readSTRef usRef

  case char of
    "~"  -> pure "dev"
    "|"  -> pure "showFps"
    "\\" -> pure "clear"
    " "  -> pure "save"
    "`"  -> pure "pause"
    "?"  -> pure "halt"
    _    -> pure "null"
