module Command where

import Prelude
import Compiler (compileShaders)
import Config (Schema, patternSchema, moduleSchema, EpiS, Pattern, SystemST, SystemConf, EngineST, EngineConf, UIST, UIConf)
import Control.Monad (when, unless)
import Control.Monad.Eff (Eff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (writeSTRef, STRef, ST, modifySTRef, readSTRef)
import DOM (DOM)
import Data.Array (uncons, updateAt, length, head, tail)
import Data.Maybe (Maybe(Just))
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (values, insert, toUnfoldable)
import Data.String (joinWith, split)
import Data.String (Pattern(..)) as S
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Engine (initEngineST)
import Graphics.Canvas (CANVAS)
import Layout (updateLayout, initLayout)
import Pattern (findModule)
import ScriptUtil (addScript)
import Serialize (unsafeSerialize)
import System (mUp, loadLib)
import Texture (clearFB)
import Util (halt, Now, cxFromStringE, intFromStringE, numFromStringE, lg, uuid, handleError)

foreign import saveCanvas :: forall eff. Eff eff Unit

command :: forall eff h. (Partial) => STRef h UIConf -> STRef h UIST -> STRef h EngineConf -> STRef h EngineST -> STRef h Pattern -> STRef h SystemConf -> STRef h (SystemST h) -> String -> Eff (canvas :: CANVAS, dom :: DOM, st :: ST h, now :: Now | eff) Unit
command ucRef usRef ecRef esRef pRef scRef ssRef msg = handleError do
  systemConf <- lift $ readSTRef scRef
  systemST   <- lift $ readSTRef ssRef
  uiConf     <- lift $ readSTRef ucRef
  uiST       <- lift $ readSTRef usRef
  engineConf <- lift $ readSTRef ecRef
  engineST   <- lift $ readSTRef esRef
  pattern    <- lift $ readSTRef pRef

  --let x = lg $ "EXECUTE: " <> (show msg)

  let dt = split (S.Pattern " ") msg

  unless (length dt == 0) do
    let cmd = fromJust $ head dt
    let args = fromJust $ tail dt

    case cmd of
      "null" -> pure unit
      "pause" -> do
        lift $ modifySTRef ssRef (\s -> s {paused = not s.paused})
        pure unit
      "halt" -> do
        lift $ halt
        pure unit
      "pauseAfterSwitch" -> do
        lift $ modifySTRef ssRef (\s -> s {pauseAfterSwitch = true})
        pure unit
      "killScripts" -> do
        let upd = (\r -> lift $ modifySTRef r (\m -> m {scripts = []}))
        traverse upd (values systemST.moduleRefPool)

        pure unit
      "scr" -> do
        when (length args >= 2) do -- check for errors here
          {head: addr, tail: rst}  <- pure $ fromJust $ uncons args
          {head: name, tail: rst'} <- pure $ fromJust $ uncons rst
          mid <- findModule systemST.moduleRefPool pattern addr true
          addScript systemST mid name (joinWith " " rst')

          pure unit
      "setP" -> do
        case args of
          [addr, par, val] -> do
            val' <- numFromStringE val
            mid  <- findModule systemST.moduleRefPool pattern addr true
            mUp systemST mid \m ->
              m {par = insert par (show val') m.par}

            pure unit
          _ -> throwError "invalid format: setP addr par val"
      "setZn" -> do
        case args of
          [addr, idx, val] -> do
            val' <- cxFromStringE val
            idx' <- intFromStringE idx
            mid  <- findModule systemST.moduleRefPool pattern addr true
            mRef <- loadLib mid systemST.moduleRefPool "find module - setP"
            mod <- lift $ readSTRef mRef

            zn' <- case (updateAt idx' (show val') mod.zn) of
              Just x -> pure x
              _ -> throwError "zn idx out of bounds setZn"
            lift $ modifySTRef mRef (\m -> m {zn = zn'})

            pure unit
          _ -> throwError "invalid format: setPar addr par val"
      "setT" -> do
        let tExp = joinWith "" args
        mid <- findModule systemST.moduleRefPool pattern "main.application.t.t_inner" true
        mUp systemST mid \m ->
          m {sub = insert "t_expr" tExp m.sub}
        compileShaders systemConf ssRef engineConf esRef pRef false

        pure unit
      "save" -> do
        save systemST pattern
      "fullWindow" -> do
        lift $ modifySTRef ucRef (\ui -> ui {windowState = "full"})
        uiConf' <- lift $ readSTRef ucRef
        initLayout uiConf' uiST

        pure unit
      "dev" -> do
        lift $ modifySTRef ucRef (\ui -> ui {windowState = "dev", keySet = "dev", showFps = true})
        uiConf' <- lift $ readSTRef ucRef
        initLayout uiConf' uiST

        pure unit
      "initLayout" -> do
        initLayout uiConf uiST
        pure unit
      "updateLayout" -> do
        updateLayout uiConf uiST systemST pattern true
      "showFps" -> do
        lift $ modifySTRef ucRef (\ui -> ui {showFps = not ui.showFps})
        uiConf' <- lift $ readSTRef ucRef
        initLayout uiConf' uiST

        pure unit
      "setKernelDim" -> do
        case args of
          [dim] -> do
            dim' <- intFromStringE dim
            lift $ modifySTRef ecRef (\ec -> ec {kernelDim = dim'})
            engineConf' <- lift $ readSTRef ecRef
            initEngineST systemConf engineConf' systemST uiConf.canvasId (Just esRef)

          _ -> throwError "invalid format: setKerneldim dim"
        pure unit
      "setFract" -> do
        case args of
          [fract] -> do
            fract' <- intFromStringE fract
            lift $ modifySTRef ecRef (\ec -> ec {fract = fract'})
            engineConf' <- lift $ readSTRef ecRef
            compileShaders systemConf ssRef engineConf' esRef pRef false

          _ -> throwError "invalid format: setFract fract"
        pure unit
      "setEngineProfile" -> do
        case args of
          [lib] -> do
            engineConf' <- loadLib lib systemST.engineConfLib "setEngineProfile"
            lift $ writeSTRef ecRef engineConf'

            initEngineST systemConf engineConf' systemST uiConf.canvasId (Just esRef)

          _ -> throwError "invalid format: setFract fract"
        pure unit
      "clear" -> do
        clearFB engineConf engineST
      _ -> throwError $ "Unknown command: " <> msg


-- PRIVATE
save :: forall eff h. (SystemST h) -> Pattern -> EpiS eff h Unit
save systemST pattern = do
  -- pattern
  id <- lift $ uuid
  ps <- unsafeSerialize patternSchema (Just id) pattern

  -- modules
  mods <- (traverse (serializeTup moduleSchema) $ toUnfoldable systemST.moduleRefPool) :: EpiS eff h (Array String)
  let mres = joinWith "\n\n" mods

  let res = "#PATTERN\n" <> ps <> "\n\n#MODULES\n" <> mres
  let a = lg res

  lift $ saveCanvas

  pure unit

  where
    serializeTup :: forall a. Schema -> (Tuple String (STRef h a)) -> EpiS eff h String
    serializeTup schema (Tuple n ref) = do
      obj <- lift $ readSTRef ref
      st <- unsafeSerialize schema (Just n) obj
      pure st
