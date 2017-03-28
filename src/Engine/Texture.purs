module Texture where

import Prelude
import Graphics.WebGL.Raw as GL
import Graphics.WebGL.Raw.Enums as GLE
import Graphics.WebGL.Raw.Types as GLT
import Config (EpiS, EngineST, EngineConf, Epi)
import Control.Monad (when)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Reader.Trans (lift)
import Data.Array (length, (!!), (..), zip, foldM)
import Data.Maybe (fromMaybe, maybe, Maybe(Nothing, Just))
import Data.Maybe.Unsafe (fromJust)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple), snd, fst)
import EngineUtil (execGL)
import Graphics.WebGL (runWebgl)
import Graphics.WebGL.Methods (createFramebuffer, createTexture)
import Graphics.WebGL.Types (WebGLTexture, WebGLContext, WebGL, WebGLFramebuffer)
import Util (dbg2, unsafeNull)

foreign import preloadImages :: forall eff. Array String -> Eff eff Unit -> Eff eff Unit
foreign import emptyImage :: forall eff. Int -> Eff eff GLT.TexImageSource

-- get a webgl texture, set default properties
newTex :: WebGL WebGLTexture
newTex = do
  ctx <- ask
  tex <- createTexture
  liftEff $ GL.bindTexture ctx GLE.texture2d tex
  liftEff $ GL.texParameteri ctx GLE.texture2d GLE.textureWrapS GLE.clampToEdge
  liftEff $ GL.texParameteri ctx GLE.texture2d GLE.textureWrapT GLE.clampToEdge
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
initAux :: EngineConf -> WebGLContext -> GLT.TexImageSource -> WebGL (Array WebGLTexture)
initAux engineConf ctx empty = do
  traverse doInit (0..(engineConf.numAux - 1))
  where
    doInit i = do
      aux <- newTex
      liftEff $ GL.bindTexture ctx GLE.texture2d aux
      liftEff $ GL.texImage2D ctx GLE.texture2d 0 GLE.rgba GLE.rgba GLE.unsignedByte empty
      pure aux

-- upload aux textures
uploadAux :: forall eff. EngineST -> String -> Array String -> Epi eff Unit
uploadAux es host names = do
  let currentImages = fromMaybe [] es.currentImages
  case es.aux of
    Nothing -> throwError "aux textures not initialized"
    (Just aux) -> do
      when (length aux < length names) do
        throwError "not enough aux textures"

      foldM (uploadImage es.ctx currentImages host) 0 (zip aux names)
      pure unit


-- create an image object. can throw error if images missing!
uploadImage :: forall eff. WebGLContext -> (Array String) -> String  -> Int -> (Tuple WebGLTexture String) -> Epi eff Int
uploadImage ctx currentImages host c (Tuple aux name) = do
  let doUpload = maybe true ((/=) name) (currentImages !! c)

  when doUpload do
    dbg2 "uploading"
    lift $ uploadImageImpl (host <> name) \img -> do
      runWebgl (do
        liftEff $ GL.bindTexture ctx GLE.texture2d aux
        liftEff $ GL.texImage2D ctx GLE.texture2d 0 GLE.rgba GLE.rgba GLE.unsignedByte img
        pure unit
      ) ctx
      pure unit
    pure unit
  pure $ c + 1

foreign import uploadImageImpl :: forall eff. String ->
                                  (GLT.TexImageSource -> Eff eff Unit) ->
                                  Eff eff Unit


clearFB :: forall eff h. (Partial) => EngineConf -> EngineST -> EpiS eff h Unit
clearFB engineConf engineST = do
  let ctx = engineST.ctx
  execGL ctx do
    liftEff $ GL.bindTexture ctx GLE.texture2d $ fst $ fromJust engineST.tex
    liftEff $ GL.texImage2D ctx GLE.texture2d 0 GLE.rgba GLE.rgba GLE.unsignedByte engineST.empty
    liftEff $ GL.bindTexture ctx GLE.texture2d $ snd $ fromJust engineST.tex
    liftEff $ GL.texImage2D ctx GLE.texture2d 0 GLE.rgba GLE.rgba GLE.unsignedByte engineST.empty
  pure unit
