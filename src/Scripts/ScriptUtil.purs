module ScriptUtil where

import Prelude
import Config (EpiS, Pattern, SystemST, Script(Script))
import Control.Monad.Except.Trans (throwError)
import Control.Monad.ST (modifySTRef, newSTRef, readSTRef, STRef)
import Control.Monad.Trans.Class (lift)
import Data.Array (head, foldM, uncons, deleteAt, cons)
import Data.Maybe (fromMaybe, Maybe(Nothing, Just))
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (StrMap, insert, empty, toUnfoldable)
import Data.String (split, joinWith, trim)
import Data.String (Pattern(..)) as S
import Data.Tuple (Tuple(Tuple))
import Pattern (clonePattern, findModule, findAddr, CloneRes(CloneRes))
import System (mUp)
import Text.Format (precision, format)
import Util (dbg, inj, numFromStringE)

addScript :: forall eff h. SystemST h -> String -> String -> String -> EpiS eff h Unit
addScript systemST mid name args = do
  let scr = inj "%0@%1 %2" [name, (format (precision 2) systemST.t), args]
  mUp systemST mid \m ->
    m {scripts = cons scr m.scripts}

purgeScript :: forall eff h. SystemST h -> String -> Int -> EpiS eff h Unit
purgeScript systemST mid idx = do
  mUp systemST mid \m ->
    m {scripts = fromMaybe m.scripts $ deleteAt idx m.scripts}

parseScript :: forall eff h. String -> EpiS eff h Script
parseScript dta = do
  let dta' = split (S.Pattern " ") $ trim dta

  --let a = lg dta'
  {head: name, tail: allargs} <- case uncons dta' of
    Just x -> pure x
    _ -> throwError "invalid path syntax"

  (Tuple name' phase) <- case (split (S.Pattern "@") name) of
    [x] -> pure $ Tuple x 0.0
    [x, y] -> do
      y' <- numFromStringE y
      pure $ Tuple x y'
    _ -> throwError "wtf are you doing?2"

  args <- foldM parseArg empty allargs

  pure $ Script name' phase args
  where
    parseArg dt arg = do
      case (split (S.Pattern ":") arg) of
        [k, v] -> pure $ insert k v dt
        _ -> throwError $ "malformed arg " <> arg


serializeScript :: Script -> String
serializeScript (Script name phase args) =
  inj "%0@%1 %2" [name, (format (precision 2) phase), (serializeArgs args)]
  where
    serializeArgs :: StrMap String -> String
    serializeArgs args = joinWith " " $ map (\(Tuple k v) -> k <> ":" <> v) (toUnfoldable args)


-- This method is called by scripts that modify the state tree.  we perform modificatiosn in a cloned tree so we can compile asynchronously
-- I don't like how this modifies ssRef
getClone :: forall eff h. (Partial) => STRef h (SystemST h) -> STRef h Pattern -> String -> EpiS eff h CloneRes
getClone ssRef pRef mid = do
  systemST <- lift $ readSTRef ssRef
  pattern  <- lift $ readSTRef pRef
  pRef' <- case systemST.compPattern of
    Just ref -> pure ref
    Nothing -> do
      dbg "cloning pattern"
      pattern' <- clonePattern ssRef pattern
      ref <- lift $ newSTRef pattern'
      lift $ modifySTRef ssRef (\s -> s {compPattern = Just ref})
      pure ref

  systemST' <- lift $ readSTRef ssRef
  pClone <- lift $ readSTRef pRef'
  addr <- findAddr systemST'.moduleRefPool pattern mid
  mid' <- findModule systemST'.moduleRefPool pClone addr false
  root <- pure $ fromJust $ head $ split (S.Pattern ".") addr
  pure $ CloneRes root pClone mid'
