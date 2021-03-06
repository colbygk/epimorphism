module Texture where

import Prelude
import Graphics.WebGL.Raw as GL
import Graphics.WebGL.Raw.Enums as GLE
import Graphics.WebGL.Raw.Types as GLT
import Config (EpiS, EngineST, EngineConf, Epi)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Control.Monad.Reader.Class (ask)
import Data.Array (filter, length, zip, (!!), (..))
import Data.Maybe (fromMaybe, Maybe(Nothing, Just))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple), snd, fst)
import EngineUtil (execGL)
import Graphics.WebGL.Methods (createFramebuffer, createTexture)
import Graphics.WebGL.Types (WebGLTexture, WebGLContext, WebGL, WebGLFramebuffer)
import Util (dbg, dbg2, elg, fromJustE, unsafeNull)

foreign import loadImages :: forall eff. Array String -> Array String -> Eff eff Unit
foreign import registerImages :: forall eff. Array String -> Eff eff Unit
foreign import getImageSrc :: forall eff.String -> Eff eff GLT.TexImageSource
foreign import emptyImage :: forall eff. Int -> Eff eff GLT.TexImageSource

-- get a webgl texture, set default properties
newTex :: WebGL WebGLTexture
newTex = do
  ctx <- ask
  tex <- createTexture
  liftEff $ GL.bindTexture ctx GLE.texture2d tex
  liftEff $ GL.texParameteri ctx GLE.texture2d GLE.textureWrapS GLE.repeat
  liftEff $ GL.texParameteri ctx GLE.texture2d GLE.textureWrapT GLE.repeat
  liftEff $ GL.texParameteri ctx GLE.texture2d GLE.textureMinFilter GLE.linear
  liftEff $ GL.texParameteri ctx GLE.texture2d GLE.textureMagFilter GLE.linear
  -- use mipmaps?

  pure tex


-- initialize framebuffer/texture pair
initTexFb :: Int -> WebGL (Tuple WebGLTexture WebGLFramebuffer)
initTexFb dim = do
  ctx <- ask
  tex <- newTex
  fb <- createFramebuffer
  liftEff $ GL.texImage2D_ ctx GLE.texture2d 0 GLE.rgba dim dim 0 GLE.rgba GLE.unsignedByte (unsafeNull :: GLT.ArrayBufferView)
  liftEff $ GL.bindFramebuffer ctx GLE.framebuffer fb
  liftEff $ GL.framebufferTexture2D ctx GLE.framebuffer GLE.colorAttachment0 GLE.texture2d tex 0

  pure $ Tuple tex fb


-- initialize auxiliary textures
initAuxTex :: EngineConf -> WebGLContext -> GLT.TexImageSource -> WebGL (Array WebGLTexture)
initAuxTex engineConf ctx empty = do
  traverse doInit (0..(engineConf.numAuxBuffers - 1))
  where
    doInit i = do
      aux <- newTex
      liftEff $ GL.bindTexture ctx GLE.texture2d aux
      liftEff $ GL.texImage2D ctx GLE.texture2d 0 GLE.rgba GLE.rgba GLE.unsignedByte empty
      pure aux

-- upload aux textures
uploadAux :: forall eff. EngineST -> String -> Array String -> Epi eff Unit
uploadAux es host names = do
  case es.auxTex of
    Nothing -> throwError "aux textures not initialized"
    (Just aux) -> do
      when (length aux < length names) do
        throwError "not enough aux textures"

      let currentImages' = fromMaybe [] es.currentImages
      let currentImages = map (\i -> fromMaybe "" (currentImages' !! i)) (0..(length names - 1))
      let zipped = map mapIt $ filter doFilter $ zip aux (zip currentImages names)

      lift $ registerImages names
      traverse (uploadImage es.ctx host) zipped
  pure unit
  where
    doFilter (Tuple a (Tuple c n)) = c /= n
    mapIt (Tuple a (Tuple c n)) = (Tuple a n)


uploadImage :: forall eff. WebGLContext -> String -> (Tuple WebGLTexture String) -> Epi eff Unit
uploadImage ctx host (Tuple aux name) = do
  src <- lift $ getImageSrc (host <> name)
  execGL ctx do
    liftEff $ GL.bindTexture ctx GLE.texture2d aux
    liftEff $ GL.texImage2D ctx GLE.texture2d 0 GLE.rgba GLE.rgba GLE.unsignedByte src

clearFB :: forall eff h. EngineConf -> EngineST -> EpiS eff h Unit
clearFB engineConf engineST = do
  let ctx = engineST.ctx
  tex <- fromJustE engineST.tex "engine textures not initialized!"
  execGL ctx do
    liftEff $ GL.bindTexture ctx GLE.texture2d $ fst $ tex
    liftEff $ GL.texImage2D ctx GLE.texture2d 0 GLE.rgba GLE.rgba GLE.unsignedByte engineST.empty
    liftEff $ GL.bindTexture ctx GLE.texture2d $ snd $ tex
    liftEff $ GL.texImage2D ctx GLE.texture2d 0 GLE.rgba GLE.rgba GLE.unsignedByte engineST.empty
  pure unit
