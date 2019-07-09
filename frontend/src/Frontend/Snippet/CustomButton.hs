{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
module Frontend.Snippet.CustomButton (main) where

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import qualified Data.Text as T
import Reflex.Dom

import Frontend.Util

keyTools :: DomBuilder t m => m ()
keyTools = card $ do
  el "h5" $ text "Key Tools"
  key "elAttr'" "Takes a tag name and an attribute map to generate a tag in the DOM."

imports :: DomBuilder t m => m ()
imports = card $ do
  el "h5" $ text "Imports of Note"
  code
    [ "import qualified Data.Text as T"
    , "import Control.Monad.Fix (MonadFix)"
    , "import Reflex.Dom"
    ]

disabledButton :: DomBuilder t m => T.Text -> m (Event t ())
disabledButton t = do
  elAttr "button" ("type" =: "button" <> "class" =: "btn btn-primary" <> "disabled" =: "") $ text t
  return never

clickableButton :: DomBuilder t m => T.Text -> m (Event t ())
clickableButton t = do
  (b, _) <- elAttr' "button" ("type" =: "button" <> "class" =: "btn btn-primary") $ text t
  return $ domEvent Click b

disableableButton :: DomBuilder t m => T.Text -> Bool -> m (Event t ())
disableableButton t b =
  if b
  then clickableButton t
  else disabledButton t

toggleButton :: (DomBuilder t m, MonadFix m, PostBuild t m, MonadHold t m) => Bool -> m (Dynamic t Bool)
toggleButton b = do
  rec
    dyB <- toggle b ev
    (e, _) <- elAttr' "button" ("type" =: "button" <> "class" =: "btn btn-primary") $ 
      dynText $ trans dyB
    let ev = domEvent Click e
  return dyB
  where
    trans = fmap $ \case
      True -> "On"
      False -> "Off"

dependentButton :: (DomBuilder t m, PostBuild t m) => m (Event t Bool)
dependentButton = do
  dyA <- do
    cb <- el "div" $ do
      c <- checkbox False def
      el "label" $ text "Accept A"
      return c
    return $ value cb
  dyB <- do
    cb <- el "div" $ do
      c <- checkbox False def
      el "label" $ text "Accept B"
      return c
    return $ value cb
  dyC <- do
    cb <- el "div" $ do
      c <- checkbox False def
      el "label" $ text "Accept C"
      return c
    return $ value cb
  let dy = tpl <$> dyA <*> dyB <*> dyC
  return . check $ updated dy
  where
    tpl a b c = (a, b, c)
    check = fmap $ \case
      (True, True, True) -> True
      _ -> False

clickableAnything :: DomBuilder t m => m (Event t T.Text)
clickableAnything = do
  (d, _) <- el' "div" $ text "I'm a clickable div."
  (s, _) <- el' "span" $ text "I'm a clickable span."
  return $ leftmost 
    [ "DIV" <$ domEvent Click d
    , "SPAN" <$ domEvent Click s
    ]

clickableText :: (DomBuilder t m, PostBuild t m) => Dynamic t T.Text -> m ()
clickableText dy = el "li" $ dynText dy

main :: (DomBuilder t m, MonadFix m, PostBuild t m, MonadHold t m) => m ()
main = do
  el "h3" $ text "Obelisk/Reflex Snippets: Buttons"
  el "p" $ text "When it comes to functionality Reflex-Dom "
  keyTools
  imports
  card $ do
    el "h5" $ text "Example: Clickable button."
    divClass "row" $ do
      evCB <- divClass "col" $ clickableButton "Click Me"
      divClass "col" $ do
        dyL <- foldDyn (\a l -> l <> [a]) [] ("Clicked." <$ evCB)
        void $ simpleList dyL clickableText
  card $ do
    el "h5" $ text "Example: Disabled button."
    divClass "row" $ do
      evDB <- divClass "col" $ disabledButton "Can't Click Me"
      divClass "col" $ do
        dyL <- foldDyn (\a l -> l <> [a]) [] ("Clicked." <$ evDB)
        void $ simpleList dyL clickableText
  card $ do
    el "h5" $ text "Example: Toggle button."
    void $ toggleButton False
  card $ do
    el "h5" $ text "Example: Disabled until all checked."
    ev <- dependentButton
    void $ widgetHold (disabledButton "Continue") (disableableButton "Continue" <$> ev)
  card $ do
    el "h5" $ text "Example: Clickable Anything"
    divClass "row" $ do
      evCa <- divClass "col" $ clickableAnything
      divClass "col" $ do
        dyL <- foldDyn (\a l -> l <> [a]) [] evCa
        void $ simpleList dyL clickableText
