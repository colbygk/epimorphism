module Compiler where

import Prelude
import Data.TypedArray as T
import Config (newCompST, UniformBindings, EpiS, Pattern, SystemST, EngineST, EngineConf, SystemConf, CompOp(..))
import Control.Alt ((<|>))
import Control.Monad (when)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.ST (modifySTRef, readSTRef, STRef)
import Control.Monad.Trans.Class (lift)
import Data.Array (length, uncons)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Tuple (fst, Tuple(Tuple))
import EngineUtil (execGL)
import Graphics.WebGL.Methods (vertexAttribPointer, enableVertexAttribArray, bindBuffer, bufferData, createBuffer)
import Graphics.WebGL.Shader (getUniformBindings, getAttrBindings, compileShadersIntoProgram, linkProgram)
import Graphics.WebGL.Types (WebGLProgram, DataType(Float), BufferData(DataSource), BufferUsage(StaticDraw), ArrayBufferType(ArrayBuffer))
import Parser (parseShader)
import Pattern (purgeModule)
import Texture (uploadAux)
import Util (unsafeCast, lg, now2, now, replaceAll, dbg, Now)

-- compile shaders and load into systemST
compileShaders :: forall eff h. (Partial) => SystemConf -> STRef h (SystemST h) -> EngineConf -> STRef h EngineST -> STRef h Pattern -> Boolean -> EpiS (now :: Now | eff) h Boolean
compileShaders sysConf ssRef engineConf esRef pRef full = do
  systemST <- lift $ readSTRef ssRef
  es <- lift $ readSTRef esRef
  let compRef = fromMaybe pRef systemST.compPattern
  pattern <- lift $ readSTRef compRef

  case (uncons es.compQueue) of
    Just {head: op, tail: rst} -> do
      dbg op
      done <- case op of
        CompMainShader -> do
          Tuple main aux <- parseMain systemST pattern engineConf.fract
          lift $ modifySTRef esRef (\es' -> es' {compST = es'.compST {mainSrc = Just main, aux = Just aux}})
          pure false
        CompDispShader -> do
          disp <- parseDisp systemST pattern
          lift $ modifySTRef esRef (\es' -> es' {compST = es'.compST {dispSrc = Just disp}})
          pure false
        CompVertShader -> do
          vert <- parseVert systemST pattern
          lift $ modifySTRef esRef (\es' -> es' {compST = es'.compST {vertSrc = Just vert}})
          pure false
        --CompUploadAux -> do
        CompMainProg -> do
          case (Tuple es.compST.mainSrc es.compST.vertSrc) of
            Tuple (Just mainSrc) (Just vertSrc) -> do
              mainProg <- execGL es.ctx (compileShadersIntoProgram vertSrc mainSrc)
              lift $ modifySTRef esRef (\es' -> es' {compST = es'.compST {mainProg = Just mainProg}})
              pure false
            _ -> throwError "need to compute sources first!"
        CompDispProg -> do
          case (Tuple es.compST.dispSrc es.compST.vertSrc) of
            Tuple (Just dispSrc) (Just vertSrc) -> do
              dispProg <- execGL es.ctx (compileShadersIntoProgram vertSrc dispSrc)
              lift $ modifySTRef esRef (\es' -> es' {compST = es'.compST {dispProg = Just dispProg}})
              pure false
            _ -> throwError "need to compute sources first!"
        CompFinish -> do
          -- aux
          case es.compST.aux of
            Just aux -> do
              uploadAux es sysConf.host aux
              pure unit
            Nothing -> pure unit -- no aux

          -- unif
          mainUnif <- case es.compST.mainProg of
            Just prog -> do
              res <- linkShader es prog
              pure (Just res)
            Nothing -> pure Nothing

          dispUnif <- case es.compST.dispProg of
            Just prog -> do
              res <- linkShader es prog
              pure (Just res)
            Nothing -> pure Nothing

          -- update engine state with new info
          lift $ modifySTRef esRef (\es' ->
                                     es' {mainProg = es'.compST.mainProg <|> es'.mainProg,
                                          dispProg = es'.compST.dispProg <|> es'.dispProg,
                                          mainUnif = mainUnif <|> es'.mainUnif,
                                          dispUnif = dispUnif <|> es'.dispUnif,
                                          currentImages = es'.compST.aux <|> es'.currentImages })

          -- clean old pattern
          pold <- lift $ readSTRef pRef
          when (pold.main /= pattern.main) do
            purgeModule ssRef pold.main
          when (pold.disp /= pattern.disp) do
            purgeModule ssRef pold.disp
          when (pold.vert /= pattern.vert) do
            purgeModule ssRef pold.vert

          -- update pattern & reset comp info
          lift $ modifySTRef ssRef (\s -> s {compPattern = Nothing})
          lift $ modifySTRef pRef (\_ -> pattern)
          lift $ modifySTRef esRef (\es' -> es' {compST = newCompST {vertSrc = es.compST.vertSrc}})

          pure true
        CompStall -> do
          pure false

      lift $ modifySTRef esRef (\es' -> es' {compQueue = rst})
      when (length rst /= 0 && full) do
        compileShaders sysConf ssRef engineConf esRef pRef full
        pure unit

      pure $ done || full
    Nothing -> throwError "shouldn't call compile with an empty queue chump!"


parseMain :: forall eff h. (Partial) => SystemST h -> Pattern -> Int -> EpiS eff h (Tuple String (Array String))
parseMain systemST pattern fract = do
  Tuple main' aux <- parseShader systemST pattern.main pattern.includes
  let main = replaceAll "\\$fract\\$" (show fract) main'
  pure $ Tuple main aux

parseDisp :: forall eff h. (Partial) => SystemST h -> Pattern -> EpiS eff h String
parseDisp systemST pattern = fst <$> parseShader systemST pattern.disp pattern.includes

parseVert :: forall eff h. (Partial) => SystemST h -> Pattern -> EpiS eff h String
parseVert systemST pattern = fst <$> parseShader systemST pattern.vert []

linkShader :: forall eff h. EngineST -> WebGLProgram -> EpiS eff h UniformBindings
linkShader es prog = execGL es.ctx do
  pos <- createBuffer
  bindBuffer ArrayBuffer pos
  bufferData ArrayBuffer (DataSource (T.asFloat32Array [-1.0,-1.0,1.0,-1.0,-1.0,1.0,
                                                        -1.0,1.0,1.0,-1.0,1.0,1.0])) StaticDraw

  linkProgram prog
  attr <- getAttrBindings prog
  enableVertexAttribArray attr.a_position
  vertexAttribPointer attr.a_position 2 Float false 0 0
  unif <- getUniformBindings prog
  pure (unsafeCast unif)
