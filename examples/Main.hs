{-# LANGUAGE ApplicativeDo  #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TupleSections  #-}
module Main where
import Control.Monad.OrdCall (OrdCall, call, runOrdCallSort)
import Control.Monad.State (State, evalState, gets, modify, runState)
import Data.Bifunctor (Bifunctor(..))
import Data.List (findIndex, intercalate, isPrefixOf, sortOn, tails)
import Data.Maybe (fromJust)

-- | Error location type
type Pos = (Int, Int)

-- | Simple errors
type Err = (Pos, String)

-- | String for separating error message
splitter :: String
splitter = "-------------------\n"

-- | Simple /O(nm)/ renderer of simple errors
renderErrors :: String -> [Err] -> String
renderErrors src = intercalate splitter
                 . map renderErr
  where
    renderErr ((l, r), lbl) = unlines
      [ take (r - l + 1) $ drop l src
      , lbl
      ]

-- | Type of state for stateful renderer
type RenderState = (String, Int)

-- | Optimized /O(n + m)/ renderer
renderErrors2 :: String -> [Err] -> String
renderErrors2 src = intercalate splitter
                  . map snd
                  . sortOn fst
                  . process
                  . sortOn (fst . snd)
                  . zip [1 :: Int .. ]
  where
    process :: [(Int, Err)] -> [(Int, String)]
    process ierrs = flip evalState (src, 0) $
      mapM (\(i, e) -> (i, ) <$> renderErr e) ierrs
    renderErr :: ((Int, Int), String) -> State RenderState String
    renderErr ((l, r), lbl) = do
      modify \(str, p) -> (drop (l - p) str, l)
      str <- gets $ take (r - l + 1) . fst
      pure $ str <> "\n" <> lbl <> "\n"

-- | Complex error type
data CompoundErr
  = UndefinedVar Pos String
  | MultipleDecl Pos Pos String
  | Conflicts [Pos] String

-- | Simple renderer of complex errors
renderErrors3 :: String -> [CompoundErr] -> String
renderErrors3 src = intercalate splitter
                  . map renderErr
  where
    cutErrStr :: Pos -> String
    cutErrStr (l, r) = take (r - l + 1) $ drop l src
    renderErr :: CompoundErr -> String
    renderErr = \case
      UndefinedVar p    name -> unlines
        [ "Undefined varriable " <> show name <> ":"
        , cutErrStr p
        ]
      MultipleDecl p p' name -> unlines
        [ "Multiple decalations of " <> show name <> ":"
        , cutErrStr p
        , "Previously declared here:"
        , cutErrStr p'
        ]
      Conflicts    ps   lbl  ->
        lbl <> ":\n" <> unlines (cutErrStr <$> ps)

-- | Optimized renderer of complex errors
renderErrors4 :: String -> [CompoundErr] -> String
renderErrors4 src = intercalate splitter
                  . flip evalState (src, 0)
                  . runOrdCallSort cutErrStr
                  . traverse renderErr
  where
    cutErrStr :: Pos -> State RenderState String
    cutErrStr (l, r) = do
      modify \(str, p) -> (drop (l - p) str, l)
      gets $ take (r - l + 1) . fst
    renderErr :: CompoundErr -> OrdCall Pos String String
    renderErr = \case
      UndefinedVar p name -> do
        str <- call p
        pure $ unlines
          [ "Undefined varriable " <> show name <> ":"
          , str
          ]
      MultipleDecl p p' name -> do
        str  <- call p
        str' <- call p'
        pure $ unlines
          [ "Multiple decalations of " <> show name <> ":"
          , str
          , "Previously declared here:"
          , str'
          ]
      Conflicts ps lbl ->
        ((lbl <> ":\n") <>) . unlines <$> traverse call ps


testStr :: String
testStr = "Lorem ipsum foo bar Foo dolor sit amet"

testErrs :: [CompoundErr]
testErrs = [ UndefinedVar (ind "bar") "bar"
           , MultipleDecl (ind "foo") (ind "Foo") "foo"
           , Conflicts [ind "sit", ind "ipsum", ind "amet"] "Some conflicts"]
  where
    ind str = let b = fromJust $ subIndex str testStr in (b, b + length str - 1)

main :: IO ()
main = pure ()

subIndex :: Eq a => [a] -> [a] -> Maybe Int
subIndex substr str = findIndex (isPrefixOf substr) (tails str)
