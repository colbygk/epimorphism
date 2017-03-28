module Switch where

import Prelude
import Config (PMut(..), ScriptRes(ScriptRes), Module, ScriptFn, EpiS, SystemST)
import Control.Monad (when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (modifySTRef, STRef, readSTRef)
import Data.Array (index, length, updateAt) as A
import Data.Maybe (Maybe(Nothing), fromMaybe)
import Data.Maybe.Unsafe (fromJust)
import Data.Set (singleton)
import Data.StrMap (insert, fromFoldable, union)
import Data.Tuple (Tuple(..))
import Pattern (CloneRes(CloneRes), purgeModule, ImportObj(ImportRef, ImportModule), replaceModule, findParent, importModule)
import ScriptUtil (getClone, addScript, purgeScript)
import System (loadLib, family)
import Text.Format (precision, format)
import Util (dbg, intFromStringE, inj, randInt, numFromStringE, gmod)


switch :: forall eff h. (Partial) => ScriptFn eff h
switch ssRef pRef t midPre idx dt = do
  CloneRes newRootN pattern mid <- getClone ssRef pRef midPre
  systemST <- lift $ readSTRef ssRef

  spd <- (loadLib "spd" dt "switch spd") >>= numFromStringE

  -- get the root, name of child & id of child to be switched
  op  <- loadLib "op" dt "switch op" -- either load or clone
  (Tuple rootId childN) <- case op of
    "load" -> do
      childN' <- loadLib "childN" dt "switch childN"
      pure $ Tuple mid childN'
    "clone" -> do
      findParent systemST.moduleRefPool pattern mid
    x -> throwError $ "invalid 'op' for switch, must be load | clone : " <> x

  -- get the relevant name to be used to either load or for the mutator
  by <- loadLib "by" dt "switch by" -- either query or value
  name <- case by of
    "val" -> do
      loadLib "val" dt "switch val"
    "query" -> do
      accs <- loadLib "accs" dt "switch accs"
      query <- loadLib "query" dt "switch query"
      typ <- loadLib "typ" dt "switch typ" -- either mod or idx
      lib <- case typ of
        "mod" -> do
          pure $ family systemST.moduleLib childN [query] [] -- using childN here is wrong - seed1, etc
        "idx" -> loadLib query systemST.indexLib "switch index" >>= \x -> pure x.lib
        x -> throwError $ "invalid 'typ' for switch, must be mod | idx : " <> x

      when (lib == []) do
        throwError "your index is empty!"

      idx <- case accs of
        "rand" -> do
          lift $ randInt $ A.length lib
        iS -> do
          i <- (pure iS) >>= intFromStringE
          pure $ i `gmod` (A.length lib)

      pure $ fromJust (A.index lib idx)

    x -> throwError $ "invalid 'by' for switch, must be query | val : " <> x

  -- import new module
  purgeScript systemST mid idx
  let nxtN = if (op == "load") then name else mid
  nxtId <- importModule ssRef (ImportRef nxtN)
  systemST' <- lift $ readSTRef ssRef

  -- if cloning, perform mutation
  when (op == "clone") do
    nxtRef <- loadLib nxtId systemST'.moduleRefPool "switch nxtId"
    idx <- loadLib "idx" dt "switch idx"
    mutatorN <- loadLib "mut" dt "switch mut"
    mutator <- getMutator mutatorN idx name
    lift $ modifySTRef nxtRef mutator

    pure unit

  -- switch! (should we inline this?)
  let tPhase = systemST'.t - t -- recover phase
  switchModules ssRef (t + tPhase) rootId childN nxtId spd

  pure $ ScriptRes (PMut pattern (singleton newRootN)) Nothing


getMutator :: forall eff h. String -> String -> String -> EpiS eff h (Module -> Module)
getMutator mut idx name  = do
  case mut of
    "image" -> do
      idx' <- intFromStringE idx
      pure $ \mod ->
        let images' = fromMaybe mod.images (A.updateAt idx' name mod.images) in
        mod {images = images'}
    "script" -> do
      idx' <- intFromStringE idx
      pure $ \mod ->
        let scripts' = fromMaybe mod.scripts (A.updateAt idx' name mod.scripts) in
        mod {scripts = scripts'}
    "sub" -> do
      pure $ \mod ->
        let sub' = insert idx name mod.sub in
        mod {sub = sub'}
    _ -> throwError $ "unknown mutator: " <> name



-- should check if dim & var are the same across m0 & m1
-- m1 is a reference id(we assume also that it was previously imported & floating)
switchModules :: forall eff h. STRef h (SystemST h) -> Number -> String -> String -> String -> Number -> EpiS eff h Unit
switchModules ssRef t rootId childN m1 spd = do
  systemST <- lift $ readSTRef ssRef

  mRef  <- loadLib rootId systemST.moduleRefPool "switch module"
  m     <- lift $ readSTRef mRef
  m0    <- loadLib childN m.modules "switch find child"
  m0Ref <- loadLib m0 systemST.moduleRefPool "switch m0"
  m0M   <- lift $ readSTRef m0Ref

  m1Ref <- loadLib m1 systemST.moduleRefPool "switch m1"
  m1M   <- lift $ readSTRef m1Ref

  -- create switch module
  switchMod <- loadLib "smooth_switch" systemST.moduleLib "switchModules"

  let modules = fromFoldable [(Tuple "m0" m0), (Tuple "m1" m1)]
  let sub'    = union (fromFoldable [(Tuple "dim" m0M.dim), (Tuple "var" m0M.var)]) switchMod.sub
  let path    = inj "linear@%0 %1" [(format (precision 2) t), (format (precision 2) spd)]
  let par     = fromFoldable [(Tuple "intrp" path)]
  let switch' = switchMod {par=par, sub = sub', modules = modules, var = m0M.var, dim = m0M.dim}

  swid <- replaceModule ssRef rootId childN m0 (ImportModule switch')
  purgeModule ssRef m1 -- we assume this was imported previously, so it was imported again by replace

  -- create & import blending script
  systemST' <- lift $ readSTRef ssRef
  addScript systemST' swid "finishSwitch" (inj "delay:%0" [show spd])

  pure unit


finishSwitch :: forall eff h. (Partial) => ScriptFn eff h
finishSwitch ssRef pRef t rootIdPre idx dt = do
  -- get data
  delay <- (loadLib "delay" dt "finishSwitch delay") >>= numFromStringE

  case t * delay of
    -- we're done
    x | x >= 1.0 -> do
      --let a = lg "DONE SWITCHING"
      CloneRes newRootN pattern rootId <- getClone ssRef pRef rootIdPre

      systemST <- lift $ readSTRef ssRef

      -- find parent & m1
      (Tuple parent subN) <- findParent systemST.moduleRefPool pattern rootId
      mRef   <- loadLib rootId systemST.moduleRefPool "finishSwitch module"
      m      <- lift $ readSTRef mRef
      m1id   <- loadLib "m1" m.modules "finishSwitch module m1"
      m1Ref  <- loadLib m1id systemST.moduleRefPool "finishSwitch m1"
      m1     <- lift $ readSTRef m1Ref

      -- replace.  this removes all scripts wrt this as well
      replaceModule ssRef parent subN rootId (ImportModule m1)

      -- this is pretty ghetto.  its for the dev ui
      when systemST.pauseAfterSwitch do
        lift $ modifySTRef ssRef (\s -> s {pauseAfterSwitch = false})
        addScript systemST parent "pause" ""
        pure unit

      --dbg "finish switch!!!!"
      pure $ ScriptRes (PMut pattern (singleton newRootN)) Nothing
    _ -> do
      pure $ ScriptRes PMutNone Nothing
