module UI where

import Prelude
import Command (command)
import Config (UIST, UIConf, EpiS, SystemST, SystemConf, Pattern, EngineST, EngineConf, defaultUIST)
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (STRef, readSTRef, newSTRef)
import KeyHandlers (keyHandler)
import Layout (initLayout)
import System (loadLib)
import Util (Now)

foreign import registerEventHandler :: forall eff. (String -> Eff eff Unit) -> Eff eff Unit
foreign import registerKeyHandler :: forall eff. (String -> Eff eff String) -> Eff eff Unit
foreign import addGlobalEventListeners :: forall eff. (String -> Eff eff Unit) -> Eff eff Unit
foreign import registerAuxImages :: forall eff. Array String -> Eff eff Unit

-- PUBLIC
initUIST :: forall eff h. (Partial) => STRef h UIConf -> STRef h EngineConf -> STRef h EngineST -> STRef h Pattern -> STRef h SystemConf -> STRef h (SystemST h) -> EpiS (now :: Now | eff) h (STRef h UIST)
initUIST ucRef ecRef esRef pRef scRef ssRef = do
  let uiST = defaultUIST
  usRef  <- lift $ newSTRef uiST
  uiConf <- lift $ readSTRef ucRef

  initLayout uiConf uiST

  let handler = command ucRef usRef ecRef esRef pRef scRef ssRef
  lift $ addGlobalEventListeners handler
  lift $ registerEventHandler handler
  lift $ registerKeyHandler (keyHandler ucRef usRef)

  systemST <- lift $ readSTRef ssRef
  imgLib <- loadLib "all_images" systemST.indexLib "all_images registerAux"

  lift $ registerAuxImages imgLib.lib

  pure usRef
