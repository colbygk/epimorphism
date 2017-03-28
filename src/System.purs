module System where

import SLibrary
import Config (Module, EpiS, moduleSchema, patternSchema, systemConfSchema, uiConfSchema, engineConfSchema, Schema, Epi, SystemST, defaultSystemST)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (modifySTRef, readSTRef, STRef)
import Data.Array (concat, foldM, (:), sort, snoc)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set (member) as S
import Data.StrMap (member, values, empty, insert, fold, lookup, StrMap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Data.List (toUnfoldable)
import Library (parseLib)
import Prelude (unit, Unit, (==), ($), not, (&&), (<>), pure, bind)
import Util (dbg, urlGet)

data DataSource = LocalHTTP | LocalStorage | RemoteDB

initSystemST :: forall eff h. (Partial) => String -> Epi eff (SystemST h)
initSystemST host = do
  -- gather system data here

  -- initialize libraries
  systemConfLib <- buildLib systemConfSchema $ host <> "/lib/system_conf.lib"
  engineConfLib <- buildLib engineConfSchema $ host <> "/lib/engine_conf.lib"
  uiConfLib     <- buildLib uiConfSchema     $ host <> "/lib/ui_conf.lib"
  moduleLib     <- buildLib moduleSchema     $ host <> "/lib/modules.lib"
  patternLib    <- buildLib patternSchema    $ host <> "/lib/patterns.lib"

  componentLib  <- buildSLib buildComponent  $ host <> "/lib/components.slib"
  indexLib      <- buildSLib buildIndex      $ host <> "/lib/indexes.slib"

  pure $ defaultSystemST {
      systemConfLib = systemConfLib
    , engineConfLib = engineConfLib
    , uiConfLib     = uiConfLib
    , moduleLib     = moduleLib
    , patternLib    = patternLib
    , componentLib  = componentLib
    , indexLib      = indexLib
  }

buildLib :: forall eff a. (Partial) => Schema -> String -> Epi eff (StrMap a)
buildLib schema loc = do
  dt <- lift $ urlGet loc
  case dt of
    (Left er) -> throwError $ "Error loading lib : " <> er
    (Right res) -> parseLib schema res

-- build a shader library from a location with a builder
buildSLib :: forall eff a.  (SHandle -> SLib (Tuple String a)) -> String -> Epi eff (StrMap a)
buildSLib f loc = do
  dt <- lift $ urlGet loc
  case dt of
    (Left er) -> throwError $ "Error loading slib : " <> er
    (Right res) -> case (parseSLib f res) of
      (Right res') -> pure res'
      (Left (SLibError s)) -> throwError $ "Error building slib at : " <> loc <> " : " <> s


-- load from a map, throw error if not found. passed context for debugging purposes
loadLib :: forall eff a. String -> (StrMap a) -> String -> Epi eff a
loadLib name lib ctx = do
  case (lookup name lib) of
    (Just d) -> pure d
    Nothing  -> throwError ("Load from lib - can't find: " <> name <> ": context :" <> ctx)


-- FLAG FAMILYS & SO FORTH
checkFlags :: forall r. {flags :: Set String | r} -> Array String -> Array String -> Boolean
checkFlags obj inc exc = (foldl (\dt f -> dt && S.member f obj.flags) true inc) &&
                         (foldl (\dt f -> dt && (not $ S.member f obj.flags)) true exc)

-- filter a family by specific include & exclude flags, return the keys sorted alphabetically
flagFamily :: forall r. StrMap {flags :: Set String | r} -> Array String -> Array String -> Array String
flagFamily col inc exc = sort $ fold handle [] col
  where
    handle res k v = case (checkFlags v inc exc) of
      true -> snoc res k
      false -> res

-- filter a family by family, include & exclude flags, return the keys sorted alphabetically
family :: forall r. StrMap {family :: String, flags :: Set String | r} -> String -> Array String -> Array String -> Array String
family col fam inc exc = flagFamily (fold handle empty col) inc exc
  where
    handle res k v = case (v.family == fam) of
      true -> insert k v res
      false -> res


-- implement a breadth first fold over the modules rooted at mid
mFold :: forall eff h a. STRef h (SystemST h) -> a -> String -> (a -> String -> EpiS eff h a) -> EpiS eff h a
mFold ssRef val mid fn = bff val [mid]
  where
    bff :: a -> Array String -> EpiS eff h a
    bff tval [] = pure tval
    bff tval xs = do
      tval' <- (foldM fn tval xs)
      children <- traverse childValues xs
      bff tval' (concat children)
    childValues :: String -> EpiS eff h (Array String)
    childValues cid = do
      systemST <- lift $ readSTRef ssRef
      case (member cid systemST.moduleRefPool) of -- calling fn may have removed the module
        true -> do
          cRef <- loadLib cid systemST.moduleRefPool "mFold cid"
          child <- lift $ readSTRef cRef
          pure $ toUnfoldable $ values child.modules
        false -> pure []


mUp :: forall eff h. SystemST h -> String -> (Module -> Module) -> EpiS eff h Unit
mUp systemST mid func = do
  mRef <- loadLib mid systemST.moduleRefPool "mUp"
  lift $ modifySTRef mRef (\m -> func m)
  pure unit
