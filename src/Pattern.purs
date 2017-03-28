module Pattern where

import Prelude
import Config (EpiS, Module, Pattern, SystemST)
import Control.Monad (when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (STRef, modifySTRef, newSTRef, readSTRef)
import Data.Array (cons, head, tail, foldM, length, last, init)
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (member, StrMap, delete, insert, values, toUnfoldable)
import Data.String (Pattern(..)) as S
import Data.String (split, joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import System (loadLib)
import Util (dbg, uuid)


------------------------ FIND ------------------------

-- find a module given an address - ie main.application.t or a reference
findModule :: forall eff h. (Partial) => StrMap (STRef h Module) -> Pattern -> String -> Boolean -> EpiS eff h String
findModule mpool pattern dt followSwitch = do
  case (member dt mpool) of
    true -> pure dt
    false -> do
      let addr = split (S.Pattern ".") dt
      case (head addr) of
        Nothing -> throwError "we need data, chump"
        Just "vert" -> findModule' mpool pattern.vert (fromJust $ tail addr) followSwitch
        Just "disp" -> findModule' mpool pattern.disp (fromJust $ tail addr) followSwitch
        Just "main" -> findModule' mpool pattern.main (fromJust $ tail addr) followSwitch
        Just x      -> throwError $ "value should be main, vert, or disp : " <> x


findModule' :: forall eff h. (Partial) => StrMap (STRef h Module) -> String -> Array String -> Boolean -> EpiS eff h String
findModule' mpool mid addr followSwitch = do
  maybe (pure $ mid) handle (head addr)
  where
    handle mid' = do
      mRef    <- loadLib mid mpool "findModule'"
      mod     <- lift $ readSTRef mRef
      childId <- loadLib mid' mod.modules "findModule' find child"
      cRef    <- loadLib childId mpool "findModule' child ref"
      child   <- lift $ readSTRef cRef
      addr'   <- pure $ fromJust $ tail addr

      case (child.family == "switch" && followSwitch) of
        true ->  findModule' mpool childId (cons "m1" addr') followSwitch
        false -> findModule' mpool childId addr' followSwitch


findAddr :: forall eff h. StrMap (STRef h Module) -> Pattern -> String -> EpiS eff h String
findAddr mpool pattern mid = do
  m0 <- find' "main" pattern.main
  m1 <- find' "disp" pattern.disp
  m2 <- find' "vert" pattern.vert
  case (m0 <> m1 <> m2) of
    Just addr -> pure addr
    Nothing -> throwError $ "orphan module? " <> mid
  where
    find' :: String -> String -> EpiS eff h (Maybe String)
    find' addr cid = case (cid == mid) of
      true -> pure (Just addr)
      false -> do
        cRef <- loadLib cid mpool "cid findAddr"
        cMod <- lift $ readSTRef cRef
        foldM (search addr) Nothing (toUnfoldable cMod.modules)
    search addr val (Tuple k v) = do
      res <- find' (addr <> "." <> k) v
      pure $ val <> res


findParent :: forall eff h. (Partial) => StrMap (STRef h Module) -> Pattern -> String -> EpiS eff h (Tuple String String)
findParent mpool pattern mid = do
  addr <- findAddr mpool pattern mid

  let cmp = split (S.Pattern ".") addr
  when (length cmp < 2) do
    throwError $ "malformed addr: " <> addr

  let lst = fromJust $ last cmp
  let addr' = joinWith "." $ fromJust $ init cmp

  pId <- findModule mpool pattern addr' false
  pure $ Tuple pId lst

------------------------ IMPORTING ------------------------

-- import the modules of a pattern into the ref pool
data ImportObj = ImportModule Module | ImportRef String
importPattern :: forall eff h. STRef h (SystemST h) -> STRef h Pattern -> EpiS eff h Unit
importPattern ssRef pRef =  do
  systemST <- lift $ readSTRef ssRef
  pattern  <- lift $ readSTRef pRef

  -- import all modules
  main <- importModule ssRef (ImportRef pattern.main)
  disp <- importModule ssRef (ImportRef pattern.disp)
  vert <- importModule ssRef (ImportRef pattern.vert)
  lift $ modifySTRef pRef (\p -> p {main = main, disp = disp, vert = vert})

  pure unit


-- import a module into the ref pool
importModule :: forall eff h. STRef h (SystemST h) -> ImportObj -> EpiS eff h String
importModule ssRef obj = do
  systemST <- lift $ readSTRef ssRef
  id <- lift $ uuid
  -- find module
  mod <- case obj of
    ImportModule m -> pure m
    ImportRef n -> case (member n systemST.moduleRefPool) of
      true -> do
        ref <- loadLib n systemST.moduleRefPool "reimport from pool"
        lift $ readSTRef ref
      false -> do
        minit <- loadLib n systemST.moduleLib "import module lib"
        pure $ minit {libName = n}

  -- update pool
  ref <- lift $ newSTRef mod
  systemST' <- lift $ readSTRef ssRef
  let mp' = insert id ref systemST'.moduleRefPool  -- maybe check for duplicates here?
  lift $ modifySTRef ssRef (\s -> s {moduleRefPool = mp'})

  -- import children
  let d = toUnfoldable mod.modules :: Array (Tuple String String)
  traverse (importChild ssRef id) d

  pure id

  where
    importChild :: STRef h (SystemST h) -> String -> (Tuple String String) -> EpiS eff h Unit
    importChild ssRef mid (Tuple k v) = do
      when (v /= "") do
        systemST <- lift $ readSTRef ssRef

        -- import child
        child <- importModule ssRef (ImportRef v)

        -- update parent
        mRef <- loadLib mid systemST.moduleRefPool "import module - update parent"
        m <- lift $ readSTRef mRef
        let modules' = insert k child m.modules
        lift $ modifySTRef mRef (\m' -> m' {modules = modules'})

        pure unit


-- remove a module from the ref pool
purgeModule :: forall eff h. STRef h (SystemST h) -> String -> EpiS eff h Unit
purgeModule ssRef mid = do
  when (mid /= "") do
    systemST <- lift $ readSTRef ssRef
    mRef <- loadLib mid systemST.moduleRefPool "purge module"
    mod <- lift $ readSTRef mRef

    -- delete self
    let mp = delete mid systemST.moduleRefPool
    lift $ modifySTRef ssRef (\s -> s {moduleRefPool = mp})

    -- purge children
    traverse (purgeModule ssRef) (values mod.modules)

    pure unit


-- replace child subN:cid(in ref pool) with child subN:obj
replaceModule :: forall eff h. STRef h (SystemST h) -> String -> String -> String -> ImportObj -> EpiS eff h String
replaceModule ssRef mid subN cid obj = do
  systemST <- lift $ readSTRef ssRef
  mRef <- loadLib mid systemST.moduleRefPool "replace module"
  m <- lift $ readSTRef mRef

  -- import & purge
  n' <- importModule ssRef obj
  purgeModule ssRef cid

  -- update
  let mod' = insert subN n' m.modules
  lift $ modifySTRef mRef (\m' -> m' {modules = mod'})

  pure n'


--  slightly janky
data CloneRes = CloneRes String Pattern String
clonePattern :: forall eff h. STRef h (SystemST h) -> Pattern -> EpiS eff h Pattern
clonePattern ssRef pattern = do
  systemST <- lift $ readSTRef ssRef

  main' <- importModule ssRef (ImportRef pattern.main)
  disp' <- importModule ssRef (ImportRef pattern.disp)
  vert' <- importModule ssRef (ImportRef pattern.vert)

  pure $ pattern {main = main', disp = disp', vert = vert'}
