module Script where

import Prelude
import Config (PMut(PMut, PMutNone), Script(Script), ScriptRes(ScriptRes), Pattern, SystemST, ScriptFn, EpiS)
import Control.Monad (when)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.ST (readSTRef, STRef)
import Control.Monad.Trans.Class (lift)
import Data.Array (length, foldM, elemIndex, updateAt, (..), zip)
import Data.Foldable (or)
import Data.Maybe (Maybe(Just))
import Data.Maybe.Unsafe (fromJust)
import Data.Set (union)
import Data.StrMap (keys, member)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import ScriptUtil (serializeScript, parseScript)
import Scripts (null, randomize, pause, incZn)
import Switch (finishSwitch, switch)
import System (mFold, mUp, loadLib)
import Util (dbg, lg)

-- find script fuction given name
lookupScriptFN :: forall eff h. (Partial) => String -> EpiS eff h (ScriptFn eff h)
lookupScriptFN n = case n of
  "null"         -> pure null
  "switch"       -> pure switch
  "incZn"        -> pure incZn
  "finishSwitch" -> pure finishSwitch
  "pause"        -> pure pause
  "randomize"    -> pure randomize
  _              -> throwError $ "script function not found: " <> n


-- execute all scripts & script pool. abort as soon as something mutates the pattern
runScripts :: forall eff h. (Partial) => STRef h (SystemST h) -> STRef h Pattern -> EpiS eff h PMut
runScripts ssRef pRef = do
  pattern <- lift $ readSTRef pRef
  r0 <- mFold ssRef PMutNone pattern.main (runModScripts ssRef pRef)
  r1 <- mFold ssRef r0 pattern.disp (runModScripts ssRef pRef)
  mFold ssRef r1 pattern.vert (runModScripts ssRef pRef)


runModScripts :: forall eff h. (Partial) => STRef h (SystemST h) -> STRef h Pattern -> PMut -> String -> EpiS eff h PMut
runModScripts ssRef pRef mut mid = do
  systemST <- lift $ readSTRef ssRef
  mRef     <- loadLib mid systemST.moduleRefPool "mid! runScripts"
  m        <- lift $ readSTRef mRef

  foldM (runScript ssRef pRef mid) mut m.scripts


runScript :: forall eff h. (Partial) => STRef h (SystemST h) -> STRef h Pattern -> String -> PMut -> String -> EpiS eff h PMut
runScript ssRef pRef mid x scr = do
  --dbg $ "running script : " <> scr <> " : for " <> mid
  --dbg x
  (Script name phase args) <- parseScript scr
  systemST <- lift $ readSTRef ssRef
  fn <- lookupScriptFN name
  let t' = systemST.t - phase

  mRef <- loadLib mid systemST.moduleRefPool "mid! runScript"
  m    <- lift $ readSTRef mRef
  idx  <- pure $ fromJust $ elemIndex scr m.scripts
  (ScriptRes mut update) <- fn ssRef pRef t' mid idx args

  case update of
    Just dt -> do
      let new = serializeScript (Script name phase dt)
      m'        <- lift $ readSTRef mRef
      systemST' <- lift $ readSTRef ssRef
      idx'      <- pure $ fromJust $ elemIndex scr m'.scripts

      mUp systemST' mid \m1 ->
        m1 {scripts = fromJust $ updateAt idx' new m1.scripts}
      pure unit
    _ -> pure unit

  case x of
    PMutNone -> pure mut
    PMut pat res -> do
      case mut of
        PMutNone -> pure x
        PMut mutP mutS -> pure $ PMut pat (union res mutS)

--runScript _ _ _ x _ = pure x
