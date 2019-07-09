{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.Snippet.SimpleTable (main) where

import qualified Data.Text as T
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Reflex.Dom

import Frontend.Util

keyTools :: DomBuilder t m => m ()
keyTools = card $ do
  el "h5" $ text "Key Tools"
  -- TODO: Investigate that this note is true.
  el "i" $ do
    text "Hydration Failed message in console during "
    inlineCode "ob run"
    text "."
  el "p" $ inlineCode "reflex-dom warning: hydration failed: the DOM was not as expected at switchover time. This may be due to invalid HTML which the browser has altered upon parsing, some external JS altering the DOM, or the page being served from an outdated cache."
  el "p" $ text "Ensure all tables consist of thead, tbody, and tfoot as some browsers may alter the DOM as the message states."
  key "elAttr" "Takes a tag name and an attribute map to generate a tag in the DOM."
  key "void" "Allows us to cleanly discard values we don't care about."

imports :: DomBuilder t m => m ()
imports = card $ do
  el "h5" $ text "Imports of Note"
  code
    [ "import Reflex.Dom"
    , "import Control.Monad.Fix (MonadFix)"
    ]

btn :: DomBuilder t m => T.Text -> m (Event t ())
btn t = do
  (b, _) <- elAttr' "button" ("type" =: "button" <> "class" =: "btn btn-primary") $ text t
  return $ domEvent Click b

btnD :: (PostBuild t m, DomBuilder t m) => Dynamic t T.Text -> m (Event t ())
btnD dyT = do
  elAttr "button" ("type" =: "button" <> "class" =: "btn btn-primary" <> "disabled" =: "") $ dynText dyT
  return never

data Data = Data 
  { dataA :: T.Text
  , dataB :: T.Text
  , dataC :: T.Text
  } deriving (Eq, Ord)

mkData :: [Data]
mkData = foldr (\v acc -> [Data ("Data" <> ps v <> "-A") ("Data" <> ps v <> "-B") ("Data" <> ps v <> "-C")] <> acc) [] ([0..1000]::[Integer])
  where
    ps = T.pack . show

dataRow :: (DomBuilder t m, PostBuild t m) => Dynamic t Data -> m ()
dataRow dy = el "tr" $ do
  el "td" $ dynText (dataA <$> dy)
  el "td" $ dynText (dataB <$> dy)
  el "td" $ dynText (dataC <$> dy)

simpleDynTable :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m) => Dynamic t [Data] -> m ()
simpleDynTable dySourceData = do
  evPb <- getPostBuild
  card $ do
    el "h5" $ text "Example: A simple list with a dynamic source showing 5 items at a time."
    rec
      (evPrev, evNext) <- 
        divClass "btn-toolbar" $ do
          evP <- divClass "btn-group" $ btn "Prev"
          _ <- btnD (showPage <$> dyPage)
          evN <- divClass "btn-group" $ btn "Next"
          return (evP, evN)
      dyPage <- foldDyn calcPage 0 (leftmost [(-1) <$ evPrev, 1 <$ evNext, 0 <$ updated dySourceData, 0 <$ evPb])
    dyViewData <- holdDyn [] $ attachPromptlyDynWith paginate dySourceData (updated dyPage)
    el "table" $ do
      el "thead" blank
      el "tbody" $
        void $ simpleList dyViewData dataRow
      el "tfoot" blank
  where
    calcPage x acc = case (x, acc) of
      (-1, 0) -> 0
      (v, _) -> v + acc
    paginate b e = (take 5 . drop (5 * e)) b
    showPage i = "Page: " <> (T.pack $ show i)

simpleTable :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m) => [Data] -> m ()
simpleTable sourceData = do
  evPb <- getPostBuild
  card $ do
    el "h5" $ text "Example: A simple list with a static source showing 5 items at a time."
    rec
      (evPrev, evNext) <- 
        divClass "btn-toolbar" $ do
          evP <- divClass "btn-group" $ btn "Prev"
          _ <- btnD (showPage <$> dyPage)
          evN <- divClass "btn-group" $ btn "Next"
          return (evP, evN)
      dyPage <- foldDyn calcPage 0 (leftmost [(-1) <$ evPrev, 1 <$ evNext, 0 <$ evPb])
    dyViewData <- holdDyn [] $ paginate <$> (updated dyPage)
    el "table" $ do
      el "thead" blank
      el "tbody" $
        void $ simpleList dyViewData dataRow
      el "tfoot" blank
  where
    calcPage x acc = case (x, acc) of
      (-1, 0) -> 0
      (v, _) -> v + acc
    paginate e = (take 5 . drop (5 * e)) sourceData
    showPage i = "Page: " <> (T.pack $ show i)

main :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m) => m ()
main = do
  el "h3" $ text "Obelisk/Reflex Snippets: Simple Table"
  el "p" $ text "A read-only tables with pagination."
  keyTools
  imports
  simpleDynTable (constDyn mkData)
  simpleTable mkData
