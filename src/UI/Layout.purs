module Layout where

import Prelude
import Config (EpiS, Pattern, SystemST, UIST, UIConf, Epi)
import Console (renderConsole)
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Data.DOM.Simple.Element (classRemove, classAdd, setInnerHTML, setStyleAttr)
import Data.DOM.Simple.Window (innerHeight, innerWidth, document, globalWindow)
import Data.Maybe (Maybe(Just, Nothing))
import UIUtil (findElt)
import Util (lg)

initLayout :: forall eff. UIConf -> UIST -> Epi eff Unit
initLayout uiConf uiST = do
  let window = globalWindow
  doc <- lift $ document window
  width  <- lift $ innerWidth window
  height <- lift $ innerHeight window

  canvas <- findElt uiConf.canvasId
  cont <- findElt "container"
  menu <- findElt "menu"
  console <- findElt uiConf.consoleId
  fps <- findElt uiConf.fpsId

  lift $ classAdd "hide" console
  lift $ classAdd "hide" menu
  lift $ classAdd "hide" fps
  lift $ classRemove "fullWindow" cont

  when uiConf.showFps do
    lift $ classRemove "hide" fps

  case uiConf.windowState of
    "dev" -> do
      lift $ setStyleAttr "width" (show (height - 10.0) <> "px") canvas
      lift $ setStyleAttr "height" (show (height - 11.0) <> "px") canvas

      lift $ classRemove "hide" console
      lift $ setStyleAttr "width" (show (width - height - 30.0) <> "px") console
      lift $ setStyleAttr "height" (show (height - 21.0) <> "px") console
    _ -> do
      lift $ setStyleAttr "width" "" canvas
      lift $ setStyleAttr "height" "" canvas
      let ofs = (width - height) / -2.0
      lift $ setStyleAttr "top" (show ofs <> "px") canvas
      lift $ setStyleAttr "bottom" (show ofs <> "px") canvas

      lift $ classAdd "fullWindow" cont
      lift $ classRemove "hide" menu
      pure unit


-- hides malformed html issues
updateLayout :: forall eff h. (Partial) => UIConf -> UIST -> SystemST h -> Pattern -> Boolean -> EpiS eff h Unit
updateLayout uiConf uiST systemST pattern force = do
  when (force ||
        (systemST.frameNum `mod` uiConf.uiUpdateFreq == 0 && not systemST.paused)) do

    when uiConf.showFps do
      case systemST.fps of
        (Just fps) -> do
          fpsDiv <- findElt uiConf.fpsId
          lift $ setInnerHTML (show fps) fpsDiv
        Nothing -> pure unit

    when (uiConf.windowState == "dev") do
      renderConsole uiConf uiST systemST pattern

    pure unit
