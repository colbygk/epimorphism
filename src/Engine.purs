module Engine where

import Prelude
import Data.TypedArray as T
import Graphics.WebGL.Raw as GL
import Graphics.WebGL.Raw.Enums as GLE
import Audio (audioData, initAudio)
import Config (newCompST, fullCompile, UniformBindings, EpiS, Pattern, EngineST, EngineConf, SystemST, SystemConf)
import Control.Monad (when)
import Control.Monad.Eff (forE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Trans (lift)
import Control.Monad.ST (writeSTRef, STRef, newSTRef, modifySTRef, readSTRef)
import Data.Array (length, (!!), (..))
import Data.Int (toNumber, fromNumber)
import Data.Maybe (maybe, Maybe(Nothing, Just))
import Data.Maybe.Unsafe (fromJust)
import Data.Tuple (Tuple(Tuple), snd, fst)
import EngineUtil (execGL)
import Graphics.Canvas (setCanvasHeight, setCanvasWidth, getCanvasElementById)
import Graphics.WebGL.Context (getWebglContextWithAttrs, defaultWebglContextAttrs)
import Graphics.WebGL.Methods (uniform2fv, uniform1fv, drawArrays, uniform1f, clearColor)
import Graphics.WebGL.Types (WebGLContext, WebGLTexture, DrawMode(Triangles), Uniform(Uniform), WebGLError(ShaderError))
import Texture (initAux, initTexFb, emptyImage)
import Util (hasAttr, unsafeGetAttr, lg, dbg, Now, unsafeNull)

--  PUBLIC

-- initialize the rendering engine & create state.  updates an existing state if passed
-- maybe validate that kernelDim > 0?
initEngineST :: forall eff h. SystemConf -> EngineConf -> SystemST h -> String -> Maybe (STRef h EngineST) -> EpiS (now :: Now | eff) h (STRef h EngineST)
initEngineST sysConf engineConf systemST canvasId esRef' = do
  -- find canvas & create context
  canvasM <- liftEff $ getCanvasElementById canvasId
  canvas <- case canvasM of
    Just c -> pure c
    Nothing -> throwError $ "init engine - canvas not found: " <> canvasId

  let attrs = defaultWebglContextAttrs {
        alpha =                 false
      , depth =                 false
      , antialias =             false
      , preserveDrawingBuffer = true}

  ctxM <- liftEff $ getWebglContextWithAttrs canvas attrs
  ctx <- case ctxM of
    Just c -> pure c
    Nothing -> throwError "Unable to get a webgl context!!!"

  empty <- lift $ emptyImage engineConf.kernelDim

  -- get reference
  esRef <- case esRef' of
    Just ref -> do
      lift $ modifySTRef ref (\r -> r {empty = empty, currentImages = Nothing})
      pure ref
    Nothing -> do
      let compST = newCompST
      let tmp = {dispProg: Nothing, mainProg: Nothing, tex: Nothing, fb: Nothing, aux: Nothing, audio: Nothing, currentImages: Nothing, compQueue: fullCompile, mainUnif: Nothing, dispUnif: Nothing, ctx, empty, compST}
      lift $ newSTRef tmp

  es <- lift $ readSTRef esRef

  -- if we change kernel_dim we need to redo this
  let dim = engineConf.kernelDim
  lift $ setCanvasWidth (toNumber dim) canvas
  lift $ setCanvasHeight (toNumber dim) canvas

  -- webgl initialization
  res <- execGL ctx do
    Tuple tex0 fb0 <- initTexFb dim
    Tuple tex1 fb1 <- initTexFb dim
    aux <- initAux engineConf es.ctx empty
    audio <- initAudio engineConf es.ctx empty

    clearColor 0.0 0.0 0.0 1.0
    liftEff $ GL.clear ctx GLE.colorBufferBit
    liftEff $ GL.viewport ctx 0 0 dim dim

    pure $ es {
        fb  = Just (Tuple fb0 fb1)
      , tex = Just (Tuple tex0 tex1)
      , aux = Just aux
      , audio = audio
    }

  -- set shaders
  lift $ writeSTRef esRef res

  pure esRef

-- do the thing!
renderFrame :: forall eff h. (Partial) => SystemST h -> EngineConf -> EngineST -> Pattern -> Array Number -> Array Number -> Int -> EpiS eff h WebGLTexture
renderFrame systemST engineConf engineST pattern par zn frameNum = do
  let ctx = engineST.ctx

  -- unpack
  tex <- case engineST.tex of
    Just x -> pure x
    Nothing -> throwError "RenderFrame: missing textures"
  fbs <- case engineST.fb of
    Just x -> pure x
    Nothing -> throwError "RenderFrame: missing framebuffers"
  aux <- case engineST.aux of
    Just x -> pure x
    Nothing -> throwError "RenderFrame: missing aux"
  main <- case engineST.mainProg of
    Just x -> pure x
    Nothing -> throwError "RenderFrame: missing main program"
  mainUnif <- case engineST.mainUnif of
    Just x -> pure x
    Nothing -> throwError "RenderFrame: missing main program"

  -- bind par & zn
  execGL ctx (liftEff $ GL.useProgram ctx main)
  bindParZn ctx mainUnif par zn

  execGL ctx do
    -- bind main uniforms
    uniform1f (unsafeGetAttr mainUnif "time") (systemST.t - pattern.tPhase)
    uniform1f (unsafeGetAttr mainUnif "kernel_dim") (toNumber engineConf.kernelDim)

    -- BUG!!! audio has to be before aux???
    --audio info

    let numAux = maybe 0 length engineST.currentImages
    case engineST.audio of
      Just (Tuple audioTex analyser) -> do
        case (hasAttr mainUnif "audioData") of
          true -> do
            liftEff $ GL.bindTexture ctx GLE.texture2d audioTex
            dta <- lift $ lift $ audioData analyser
            liftEff $ GL.texImage2D_ ctx GLE.texture2d 0 GLE.alpha engineConf.audioBufferSize 1 0 GLE.alpha GLE.unsignedByte dta

            let ofs = numAux + 1
            liftEff $ GL.uniform1i ctx (unsafeGetAttr mainUnif "audioData") ofs
            liftEff $ GL.activeTexture ctx (GLE.texture0 + ofs)
            liftEff $ GL.bindTexture ctx GLE.texture2d audioTex
          false -> pure unit
      Nothing -> pure unit

    -- aux
    when (numAux > 0) do
      when (not $ hasAttr mainUnif "aux[0]") do
        throwError $ ShaderError "missing aux uniform!"
      liftEff $ GL.uniform1iv ctx (unsafeGetAttr mainUnif "aux[0]") (1..numAux)
      liftEff $ forE 0 numAux \i -> do
        GL.activeTexture ctx (GLE.texture1 + i)
        GL.bindTexture ctx GLE.texture2d $ fromJust (aux !! i)

    -- ping pong buffers
    let tm = if frameNum `mod` 2 == 0 then fst tex else snd tex
    let td = if frameNum `mod` 2 == 1 then fst tex else snd tex
    let fb = if frameNum `mod` 2 == 1 then fst fbs else snd fbs

    -- draw
    liftEff $ GL.activeTexture ctx GLE.texture0
    liftEff $ GL.bindTexture ctx GLE.texture2d tm
    liftEff $ GL.bindFramebuffer ctx GLE.framebuffer fb
    drawArrays Triangles 0 6

    pure td


postprocessFrame :: forall eff h. SystemST h -> EngineConf -> EngineST -> WebGLTexture -> Array Number -> Array Number -> EpiS eff h Unit
postprocessFrame systemST engineConf engineST tex par zn = do
  let ctx = engineST.ctx

  disp <- case engineST.dispProg of
    Just x -> pure x
    Nothing -> throwError "RenderFrame: missing disp program"
  dispUnif <- case engineST.dispUnif of
    Just x -> pure x
    Nothing -> throwError "RenderFrame: missing disp program"

  execGL ctx (liftEff $ GL.useProgram ctx disp)
  bindParZn ctx dispUnif par zn

  execGL ctx do
    -- bind disp uniforms
    uniform1f (unsafeGetAttr dispUnif "kernel_dim") (toNumber engineConf.kernelDim)

    -- draw
    liftEff $ GL.bindTexture ctx GLE.texture2d tex
    liftEff $ GL.bindFramebuffer ctx GLE.framebuffer unsafeNull
    drawArrays Triangles 0 6


-- bind parameters & zn values from pattern into program
bindParZn :: forall h eff. WebGLContext -> UniformBindings -> Array Number -> Array Number -> EpiS eff h Unit
bindParZn ctx unif par zn = do
  execGL ctx do
    when (length par > 0) do
      when (not $ hasAttr unif "par[0]") do
        throwError $ ShaderError "missing par binding!"
      uniform1fv (Uniform (unsafeGetAttr unif "par[0]")) (T.asFloat32Array par)

    when (length zn > 0) do
      when (not $ hasAttr unif "zn[0]") do
        throwError $ ShaderError "missing zn binding!"
      uniform2fv (Uniform (unsafeGetAttr unif "zn[0]")) (T.asFloat32Array zn)
