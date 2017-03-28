module Library where
import Prelude
import Config (Epi, Schema, SchemaEntry(..), SchemaEntryType(..))
import Control.Monad (when)
import Control.Monad.Except.Trans (throwError)
import Data.Array (tail, head, cons, foldM, reverse, filter, length, index) as A
import Data.Complex (Complex)
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Data.Set (Set, fromFoldable, empty) as S
import Data.StrMap (lookup, foldM, StrMap, empty, insert, fromFoldable)
import Data.String (Pattern(..), joinWith, null, trim, stripPrefix, split)
import Data.String.Regex (match)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Util (cxFromStringE, numFromStringE, intFromStringE, boolFromStringE, unsafeSetAttr, tryRegex)

foreign import unsafeGenericObjectImpl :: forall a. Schema -> S.Set String -> a

unsafeGenericObject :: forall a. Schema -> a
unsafeGenericObject schema = unsafeGenericObjectImpl schema S.empty

-- parse an entire library file
parseLib :: forall eff a. (Partial) => Schema -> String -> Epi eff (StrMap a)
parseLib schema lib = do
  let groups = A.filter ((/=) "") $ A.filter ((/=) "\n") $ split (Pattern "\n\n") lib
  A.foldM (parseGroup schema) empty groups

-- parse an individual element
parseGroup :: forall eff a. (Partial) => Schema -> (StrMap a) -> String -> Epi eff (StrMap a)
parseGroup schema lib group = do
  let lines = A.filter ((/=) "") $ A.filter ((/=) "\n") $ split (Pattern "\n") group

  -- get name & parent
  {name, parent} <- parseName $ A.head lines
  rst <- pure $ fromJust $ A.tail lines -- monadic wrapper, so fromJust is safe

  -- get default obj
  obj <- if (not $ null parent) then
           case (lookup parent lib) of
             (Just p) -> pure p
             Nothing  -> throwError $ "couldn't find parent " <> parent
         else
           pure $ unsafeGenericObject schema

  -- set attrs
  final <- A.foldM (parseLine schema) obj rst

  pure $ insert name final lib

  where
    parseName (Just line) = do
      case (stripPrefix (Pattern "--") line) of
        (Just str) -> do
          let cmp = map trim $ A.filter ((/=) "") $ split (Pattern "<") str
          case cmp of
            [n]    -> pure {name: n, parent: ""}
            [n, p] -> pure {name: n, parent: p}
            _      -> throwError "wtf are you doing"
        Nothing -> throwError $ "expecting a name: " <> line
    parseName Nothing = throwError "group doesnt have title line"


-- parse a line & update an object
parseLine :: forall eff a. (Partial) => Schema -> a -> String -> Epi eff a
parseLine schema obj line = do
  let tokens = A.filter ((/=) "") $ split (Pattern " ") line
  case (A.length tokens) of
    0 -> throwError $ "malformed line " <> line
    1 -> pure obj
    _ -> do
      let attrn = fromJust $ A.index tokens 0
      let val = joinWith " " $ fromJust $ A.tail tokens

      let ses = A.filter (schemaSel attrn) schema
      when (A.length ses /= 1) do
        throwError $ line <> " matched " <> (show $ A.length ses) <> " schema entries!!"

      (SchemaEntry st _) <- pure $ fromJust $ A.head ses

      case st of
        SE_St -> do
          pure $ unsafeSetAttr obj attrn val
        SE_N -> do
          n <- numFromStringE val
          pure $ unsafeSetAttr obj attrn n
        SE_I -> do
          i <- intFromStringE val
          pure $ unsafeSetAttr obj attrn i
        SE_B -> do
          b <- boolFromStringE val
          pure $ unsafeSetAttr obj attrn b
        SE_S -> do
          s <- parseSet val
          pure $ unsafeSetAttr obj attrn s
        SE_M_St -> do
          m <- parseMp val
          pure $ unsafeSetAttr obj attrn m
        SE_M_N -> do
          mn <- parseMp val >>= parseNMp
          pure $ unsafeSetAttr obj attrn mn
        SE_A_St -> do
          l <- parseLst val
          pure $ unsafeSetAttr obj attrn l
        SE_A_Cx -> do
          cx <- parseLst val >>= parseCLst
          pure $ unsafeSetAttr obj attrn cx

  where
    schemaSel n (SchemaEntry _ sen) = (n == sen)


-- ELEMENT PARSERS
parseMString :: forall eff. String -> Epi eff (Maybe String)
parseMString s = do
  pure $ if (s == "Nothing") then Nothing else Just s


parseSet :: forall eff. String -> Epi eff (S.Set String)
parseSet st = do
  rgx <- tryRegex "^\\{([^\\{\\}]*)\\}$"

  case (match rgx st) of
    (Just [(Just _), (Just "")]) -> do
      pure S.empty
    (Just [(Just _), (Just l)]) -> pure $ S.fromFoldable $ map trim $ split (Pattern ",") l
    _ -> throwError $ "Expected " <> st <> " to be a set"

parseMp :: forall eff. String -> Epi eff (StrMap String)
parseMp st = do
  rgx <- tryRegex "^\\{([^\\{\\}]*)\\}$"
  case (match rgx st) of
    (Just [(Just _), (Just "")]) -> do
      pure empty
    (Just [(Just _), (Just l)]) -> do
      dt <- traverse (exp <<< split (Pattern ":")) $ split (Pattern ",") l
      pure $ fromFoldable dt
    _ -> throwError $ "Expected " <> st <> " to be a map1"
  where
    exp [a, b] = pure $ Tuple (trim a) (trim b)
    exp _ = throwError $ "Expected " <> st <> " to be a map2"

parseNMp :: forall eff. StrMap String -> Epi eff (StrMap Number)
parseNMp sm = foldM handle empty sm
  where
    handle dt k v = do
      nv <- numFromStringE v
      pure $ insert k nv dt

parseLst :: forall eff. String -> Epi eff (Array String)
parseLst st = do
  rgx <- tryRegex "^\\[([^\\[\\]]*)\\]$"
  case (match rgx st) of
    (Just [(Just _), (Just "")]) -> pure []
    (Just [(Just _), (Just l)]) -> pure $ map trim $ split (Pattern ",") l
    _ -> throwError $ "Expected " <> st <> " to be a list"

parseCLst :: forall eff. Array String -> Epi eff (Array Complex)
parseCLst st = A.reverse <$> A.foldM handle [] st
  where
    handle dt v = do
      cv <- cxFromStringE v
      pure $ A.cons cv dt
